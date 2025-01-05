{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Fine.Codegen (runGenCode) where

import Control.Monad.Trans.Reader (Reader, ask, asks, local, runReader, withReaderT)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.List.NonEmpty.Extra (unsnoc)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Fine.Syntax.Common
  ( Bind (..),
    Data (Data),
    Ext (Ext),
    Var (Var),
    VariantSpec (variantExtValue),
    varName,
  )
import Fine.Syntax.Expr (Closure (Closure), Expr (..), Module (EntryModule, Module))
import Fine.Syntax.Pattern (Pattern, boundVars)
import qualified Fine.Syntax.Pattern as Patt

class CodeGens t ctx where
  genCode :: t -> Reader ctx Text

data Ctx = Ctx
  { indentation :: Text,
    separator :: Char,
    symNames :: Map Char Text,
    variantExtValues :: Map Var Ext
  }

withIndentation :: Text -> Ctx -> Ctx
withIndentation ind ctx = ctx {indentation = ind}

sanitize :: Text -> Reader (Map Char Text) Text
sanitize name = do
  syms <- ask
  let f = go syms
  return (T.concat $ map f $ T.unpack name)
  where
    go names ch = M.findWithDefault (T.singleton ch) ch names

genObjMemberCode :: (CodeGens t Ctx) => (Var, t) -> Reader Ctx Text
genObjMemberCode (name, value) = do
  value' <- genCode value
  return [i|#{name}: #{value'}|]

genDataCode :: (CodeGens t Ctx) => Data t -> Reader Ctx Text
genDataCode (Data members) = T.intercalate ", " <$> mapM genObjMemberCode members

instance CodeGens Pattern Ctx where
  genCode :: Pattern -> Reader Ctx Text
  genCode (Patt.Int v _) = return (T.pack $ show v)
  genCode (Patt.Float v _) = return (T.pack $ show v)
  genCode (Patt.Str s _) = return [i|"#{s}"|]
  genCode (Patt.Unit _) = return "fine$unit"
  genCode (Patt.Obj dt _) = do
    members' <- genDataCode dt
    return [i|({#{members'}})|]
  genCode (Patt.Variant tag dt@(Data members) _) = do
    extValue <- asks (M.lookup tag . variantExtValues)
    case extValue of
      Nothing -> do
        let tagged = [i|$tag: "#{tag}"|] :: Text
        members' <- genDataCode dt
        return $
          if null members
            then [i|({#{tagged}})|]
            else [i|({#{tagged}, #{members'}})|]
      Just (Ext code _) -> return code
  genCode (Patt.Tuple fst' snd' rest _) = do
    fst'' <- genCode fst'
    snd'' <- genCode snd'
    rest' <-
      if null rest
        then return ""
        else mapM genCode rest >>= (return . T.append ", " . T.intercalate ", ")
    return [i|fine$tuple(#{fst''}, #{snd''}#{rest'})|]
  genCode (Patt.Capture (Var name _)) = return [i|fine$capture("#{name}")|]

genPartialMatchCode :: Pattern -> Reader Ctx Text
genPartialMatchCode patt = do
  let bound = boundVars patt
  let fHead =
        if null bound
          then "()" :: Text
          else [i|({ #{T.intercalate ", " (map varName bound)} })|]
  patt' <- genCode patt
  return [i|#{patt'}, #{fHead} =>|]

genStmtsCode :: NonEmpty Expr -> Reader Ctx Text
genStmtsCode exprs = do
  oldIndent <- asks indentation
  let indent = oldIndent <> "  "
  exprs' <- local (withIndentation indent) (mapM genCode exprs)
  let (stmts, expr) = unsnoc exprs'
  let stmts' = T.concat $ map (\stmt -> [i|#{indent}#{stmt};\n|] :: Text) stmts
  let expr' = [i|#{indent}return #{expr};|] :: Text
  return [i|{\n#{stmts'}#{expr'}\n#{oldIndent}}|]

genFunCode :: Text -> [Var] -> Expr -> Reader Ctx Text
genFunCode name params body = do
  let params' = T.intercalate ", " (map varName params)
  case body of
    Block exprs _ -> do
      body' <- genStmtsCode exprs
      return [i|function #{name}(#{params'}) #{body'}|]
    _ -> do
      body' <- genCode body
      return [i|function #{name}(#{params'}) { return #{body'}; }|]

instance CodeGens Expr Ctx where
  genCode :: Expr -> Reader Ctx Text
  genCode (Int v _) = return (T.pack $ show v)
  genCode (Float v _) = return (T.pack $ show v)
  genCode (Str s _) = return [i|"#{s}"|]
  genCode (Unit _) = return "fine$unit"
  genCode (Obj dt _) = do
    members' <- genDataCode dt
    return [i|({#{members'}})|]
  genCode (Variant tag dt@(Data members) _) = do
    extValue <- asks (M.lookup tag . variantExtValues)
    case extValue of
      Nothing -> do
        let tagged = [i|$tag: "#{tag}"|] :: Text
        members' <- genDataCode dt
        return $
          if null members
            then [i|({#{tagged}})|]
            else [i|({#{tagged}, #{members'}})|]
      Just (Ext code _) -> return code
  genCode (Tuple fst' snd' rest _) = do
    fst'' <- genCode fst'
    snd'' <- genCode snd'
    rest' <-
      if null rest
        then return ""
        else mapM genCode rest >>= (return . T.append ", " . T.intercalate ", ")
    return [i|fine$tuple(#{fst''}, #{snd''}#{rest'})|]
  genCode (Id (Var name _)) = withReaderT symNames (sanitize name)
  genCode (App f args _) = do
    f' <- genCode f
    args' <- (T.intercalate ", ") <$> mapM genCode args
    return [i|#{f'}(#{args'})|]
  genCode (Access expr (Var prop _)) = do
    expr' <- genCode expr
    return [i|#{expr'}.#{prop}|]
  genCode (Cond cond yes no _) = do
    cond' <- genCode cond
    yes' <- genCode yes
    no' <- genCode no
    return [i|#{cond'} ? #{yes'} : #{no'}|]
  genCode (PatternMatch expr matches _) = do
    oldIndent <- asks indentation
    let indent = oldIndent <> "  "
    expr' <- local (withIndentation indent) (genCode expr)
    let matchList = toList matches
    patterns <- mapM (genPartialMatchCode . fst) matchList
    exprs <- local (withIndentation indent) (mapM (genCode . snd) matchList)
    let matches' =
          T.concat $
            zipWith
              (\p e -> [i|#{indent}[#{p} #{e}],\n|] :: Text)
              patterns
              exprs
    return [i|fine$match(\n#{indent}#{expr'},\n#{matches'}#{oldIndent})|]
  genCode (Fun params body _) = genFunCode "" params body
  genCode (Block exprs _) = do
    content <- genStmtsCode exprs
    return [i|(() => #{content})()|]
  genCode (Parens expr) = genCode expr
  genCode (ExtExpr (Ext code _)) = return code

instance CodeGens (Bind () (Closure Expr)) Ctx where
  genCode :: Bind () (Closure Expr) -> Reader Ctx Text
  genCode (Bind (Var name _) _ (Closure _ expr _)) = do
    name' <- withReaderT symNames (sanitize name)
    case expr of
      Fun params body _ -> genFunCode name' params body
      _ -> do
        expr' <- genCode expr
        return [i|const #{name'} = #{expr'};|]

instance CodeGens Module Ctx where
  genCode :: Module -> Reader Ctx Text
  genCode (Module bindings _ specs) = do
    let extValues = M.mapMaybe variantExtValue specs
    stmts <-
      local
        (\ctx -> ctx {variantExtValues = M.union extValues (variantExtValues ctx)})
        (mapM genCode bindings)
    return (T.intercalate "\n" stmts)
  genCode (EntryModule bindings fixs specs (Closure _ expr _)) = do
    code <- genCode (Module bindings fixs specs)
    entry <- genCode expr
    return [i|#{code}\n#{entry};|]

runGenCode :: (CodeGens t Ctx) => [Text] -> t -> Text
runGenCode codeInjections x =
  let code =
        runReader
          (genCode x)
          Ctx
            { indentation = "",
              separator = '$',
              symNames =
                M.fromList
                  [ ('+', "$plus"),
                    ('-', "$mnus"),
                    ('*', "$ast"),
                    ('/', "$sol"),
                    ('%', "$pcnt"),
                    ('^', "$hat"),
                    ('|', "$bar"),
                    ('&', "$amp"),
                    ('<', "$lt"),
                    ('>', "$gt"),
                    ('=', "$eq"),
                    (':', "$coln"),
                    ('\\', "$bsol"),
                    ('?', "$qust"),
                    ('!', "$excl"),
                    ('$', "$dllr"),
                    ('@', "$at"),
                    ('~', "$tild"),
                    ('.', "$dot")
                  ],
              variantExtValues = M.empty
            }
   in T.intercalate "\n\n" (codeInjections ++ [code]) <> "\n"
