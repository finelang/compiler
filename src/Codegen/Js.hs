{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Codegen.Js (runGenCode) where

import Control.Monad.Trans.Reader (Reader, ask, asks, local, runReader, withReaderT)
import Data.List (unsnoc)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Syntax.Common (Bind (..), Data (Data), Var (Var), varName)
import Syntax.Expr (Closure (Closure), Expr (..), Module (Module))

unsnoc' :: NonEmpty a -> ([a], a)
unsnoc' (x :| xs) = case unsnoc xs of
  Just (xs', x') -> (x : xs', x')
  Nothing -> ([], x)

class CodeGens t ctx where
  genCode :: t -> Reader ctx Text

data Ctx = Ctx
  { indentation :: Text,
    separator :: Char,
    symNames :: Map Char Text
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

instance CodeGens (NonEmpty Expr) Ctx where
  genCode :: NonEmpty Expr -> Reader Ctx Text
  genCode exprs = do
    oldIndent <- asks indentation
    let indent = oldIndent <> "  "
    exprs' <- local (withIndentation indent) (mapM genCode exprs)
    let (stmts, expr) = unsnoc' exprs'
    let stmts' = T.concat $ map (\stmt -> [i|#{indent}#{stmt};\n|] :: Text) stmts
    let expr' = [i|#{indent}return #{expr};|] :: Text
    return [i|{\n#{stmts'}#{expr'}\n#{oldIndent}}|]

genObjMemberCode :: (Var, Expr) -> Reader Ctx Text
genObjMemberCode (name, value) = do
  indent <- asks indentation
  value' <- genCode value
  return [i|#{indent}#{name}: #{value'}|]

instance CodeGens Expr Ctx where
  genCode :: Expr -> Reader Ctx Text
  genCode (Int v _) = return (T.pack $ show v)
  genCode (Float v _) = return (T.pack $ show v)
  genCode (Str s _) = return [i|"#{s}"|]
  genCode (Obj (Data members) _) =
    if null members
      then return "({})"
      else do
        members' <- do
          chunks <- mapM genObjMemberCode members
          return (T.intercalate ", " chunks)
        return [i|({ #{members'} })|]
  genCode (Id (Var name _)) = withReaderT symNames (sanitize name)
  genCode (App f args _) = do
    f' <- genCode f
    args' <- (T.intercalate ", ") <$> mapM genCode args
    return [i|#{f'}(#{args'})|]
  genCode (Fun params body _) = do
    let params' = T.intercalate ", " (map varName params)
    body' <- case body of
      Block exprs _ -> genCode exprs
      _ -> genCode body
    return [i|(#{params'}) => #{body'}|]
  genCode (Ctor tag params) =
    if null params
      then return [i|({ $tag: "#{tag}" })|]
      else do
        let params' = T.intercalate ", " (map varName params)
        return [i|(#{params'}) => ({ $tag: "#{tag}", #{params'} })|]
  genCode (Block exprs _) = do
    content <- genCode exprs
    return [i|(() => #{content})()|]
  genCode (Parens expr) = do
    expr' <- genCode expr
    return [i|(#{expr'})|]

instance CodeGens (Bind () (Closure Expr)) Ctx where
  genCode :: Bind () (Closure Expr) -> Reader Ctx Text
  genCode (Bind (Var name _) _ (Closure _ expr _)) = do
    name' <- withReaderT symNames (sanitize name)
    expr' <- genCode expr
    return [i|const #{name'} = #{expr'};|]

instance CodeGens Module Ctx where
  genCode :: Module -> Reader Ctx Text
  genCode (Module bindings _) = do
    stmts <- mapM genCode bindings
    return (T.intercalate "\n" stmts <> "\n")

runGenCode :: (CodeGens t Ctx) => t -> Text
runGenCode x =
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
              ('~', "$tild")
            ]
      }
