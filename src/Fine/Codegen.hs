{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Fine.Codegen (runGenCode) where

import Control.Monad.Trans.Reader (Reader, ask, asks, local, runReader, withReaderT)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.List.NonEmpty.Extra (unsnoc)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Fine.Syntax (Closure (Closure), Expr (..), Module (..), Pattern (..), PropsPattern (..), boundVars)
import Fine.Syntax.Common
  ( Bind (..),
    Ext (Ext),
    Lit (..),
    Prop (..),
    Var (Var),
    VariantSpec (variantExtValue),
    varName,
  )

class CodeGens t ctx where
  genCode :: t -> Reader ctx Text

instance CodeGens Lit ctx where
  genCode :: Lit -> Reader ctx Text
  genCode (Int v) = return (T.pack $ show v)
  genCode (Float v) = return (T.pack $ show v)
  genCode (Bool True) = return "true"
  genCode (Bool False) = return "false"
  genCode (Str s) = return [i|"#{s}"|]
  genCode (Unit) = return "undefined"

data Ctx = Ctx
  { indentation :: Text,
    separator :: Char,
    symNames :: Map Char Text,
    variantExtValues :: Map Var Ext
  }

withIndentation :: Text -> Ctx -> Ctx
withIndentation ind ctx = ctx {indentation = ind}

increaseIndentation :: Reader Ctx Text
increaseIndentation = do
  indent <- asks indentation
  return (indent <> "  ")

sanitize :: Text -> Reader (Map Char Text) Text
sanitize name = do
  syms <- ask
  let f = go syms
  return (T.concat $ map f $ T.unpack name)
  where
    go names ch = M.findWithDefault (T.singleton ch) ch names

genPropsPatternCode :: PropsPattern -> Reader Ctx Text
genPropsPatternCode (PropsPattern named objCapture) = do
  let (names, patts) = unzip named
  patts' <- mapM genCode patts
  let named' = zipWith (\n p -> [i|#{n}: #{p}|] :: Text) names patts'
  let objCapture' = case objCapture of
        Just name -> [[i|...fine$captureObj("#{name}")|] :: Text]
        Nothing -> []
  return $ T.intercalate ", " (objCapture' ++ named')

instance CodeGens Pattern Ctx where
  genCode :: Pattern -> Reader Ctx Text
  genCode (LiteralPatt lit _) = genCode lit
  genCode (ObjPatt props _) = do
    props' <- genPropsPatternCode props
    return [i|({#{props'}})|]
  genCode (VariantPatt tag props _) = do
    extValue <- asks (M.lookup tag . variantExtValues)
    case extValue of
      Nothing -> do
        let tagged = [i|$tag: "#{tag}"|] :: Text
        props' <- genPropsPatternCode props
        return $
          if T.null props'
            then [i|({#{tagged}})|]
            else [i|({#{tagged}, #{props'}})|]
      Just (Ext code _) -> return code
  genCode (TuplePatt fst' snd' rest _) = do
    fst'' <- genCode fst'
    snd'' <- genCode snd'
    rest' <-
      if null rest
        then return ""
        else mapM genCode rest >>= (return . T.append ", " . T.intercalate ", ")
    return [i|fine$tuple(#{fst''}, #{snd''}#{rest'})|]
  genCode (Capture (Var name _)) = return [i|fine$capture("#{name}")|]

instance (CodeGens t Ctx) => CodeGens (Prop t) Ctx where
  genCode :: Prop t -> Reader Ctx Text
  genCode (NamedProp name value) = do
    value' <- genCode value
    return [i|#{name}: #{value'}|]
  genCode (SpreadProp value) = do
    value' <- genCode value
    return [i|...#{value'}|]

genPropsCode :: (CodeGens t Ctx) => [Prop t] -> Reader Ctx Text
genPropsCode props = T.intercalate ", " <$> mapM genCode props

genMatchCode :: (Pattern, Expr) -> Reader Ctx Text
genMatchCode (patt, expr) = do
  oldIndent <- asks indentation
  indent <- increaseIndentation
  patt' <- genCode patt
  expr' <- local (withIndentation indent) (genFunCode True "" (boundVars patt) expr)
  return [i|[\n#{indent}#{patt'},\n#{indent}#{expr'}\n#{oldIndent}]|]

genStmtsCode :: NonEmpty Expr -> Reader Ctx Text
genStmtsCode exprs = do
  oldIndent <- asks indentation
  indent <- increaseIndentation
  exprs' <- local (withIndentation indent) (mapM genCode exprs)
  let (stmts, expr) = unsnoc exprs'
  let stmts' = T.concat $ map (\stmt -> [i|#{indent}#{stmt};\n|] :: Text) stmts
  let expr' = [i|#{indent}return #{expr};|] :: Text
  return [i|{\n#{stmts'}#{expr'}\n#{oldIndent}}|]

genFunCode :: Bool -> Text -> [Var] -> Expr -> Reader Ctx Text
genFunCode areObjParams name params body = do
  let params' = T.intercalate ", " (map varName params)
  let params'' = if areObjParams then [i|{#{params'}}|] else params'
  case body of
    Block exprs _ -> do
      body' <- genStmtsCode exprs
      return [i|function #{name}(#{params''}) #{body'}|]
    _ -> do
      body' <- genCode body
      return [i|function #{name}(#{params''}) { return #{body'}; }|]

instance CodeGens Expr Ctx where
  genCode :: Expr -> Reader Ctx Text
  genCode (Literal lit _) = genCode lit
  genCode (Obj props _) = do
    props' <- genPropsCode props
    return [i|({#{props'}})|]
  genCode (Variant tag props _) = do
    extValue <- asks (M.lookup tag . variantExtValues)
    case extValue of
      Nothing -> do
        let tagged = [i|$tag: "#{tag}"|] :: Text
        props' <- genPropsCode props
        return $
          if null props
            then [i|({#{tagged}})|]
            else [i|({#{tagged}, #{props'}})|]
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
    indent <- increaseIndentation
    expr' <- local (withIndentation indent) (genCode expr)
    matches' <-
      T.intercalate [i|,\n#{indent}|]
        <$> local (withIndentation indent) (mapM genMatchCode $ toList matches)
    return [i|fine$match(\n#{indent}#{expr'},\n#{indent}#{matches'}\n#{oldIndent})|]
  genCode (Fun params body _) = genFunCode False "" params body
  genCode (Block exprs _) = do
    content <- genStmtsCode exprs
    return [i|(() => #{content})()|]
  genCode (Parens expr) = genCode expr
  genCode (ExtId (Ext code _)) = return code
  genCode (ExtOpApp (Ext op _) l r) = do
    l' <- genCode l
    r' <- genCode r
    return [i|#{l'} #{op} #{r'}|]
  genCode (Debug expr _) = do
    expr' <- genCode expr
    return [i|fine$debug(#{expr'})|]

instance CodeGens (Bind () (Closure Expr)) Ctx where
  genCode :: Bind () (Closure Expr) -> Reader Ctx Text
  genCode (Bind (Var name _) _ (Closure _ expr _)) = do
    name' <- withReaderT symNames (sanitize name)
    case expr of
      Fun params body _ -> genFunCode False name' params body
      _ -> do
        expr' <- genCode expr
        return [i|const #{name'} = #{expr'};|]

instance CodeGens Module Ctx where
  genCode :: Module -> Reader Ctx Text
  genCode (Module binds _ specs) = do
    let extValues = M.mapMaybe variantExtValue specs
    stmts <-
      local
        (\ctx -> ctx {variantExtValues = M.union extValues (variantExtValues ctx)})
        (mapM genCode binds)
    return (T.intercalate "\n\n" stmts)
  genCode (EntryModule binds fixs specs (Closure _ expr _)) = do
    code <- genCode (Module binds fixs specs)
    entry <- genCode expr
    return [i|#{code}\n\n#{entry};|]

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
