{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}

module Codegen.Js (runGenCode) where

import Control.Monad.Trans.Reader (Reader, asks, local, runReader)
import Data.List (unsnoc)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String.Interpolate (i)
import Data.Text (Text, concat, intercalate, pack)
import Syntax.Common (Bind (..), Var (Var), varName)
import Syntax.Expr (Closure (Closure), Expr (..), Module (Module))
import Prelude hiding (concat)

unsnoc' :: NonEmpty a -> ([a], a)
unsnoc' (x :| xs) = case unsnoc xs of
  Just (xs', x') -> (x : xs', x')
  Nothing -> ([], x)

class CodeGens t ctx where
  genCode :: t -> Reader ctx Text

newtype Ctx = Ctx {indentation :: Text}

withIndentation :: Text -> Ctx -> Ctx
withIndentation ind ctx = ctx {indentation = ind}

instance CodeGens (NonEmpty Expr) Ctx where
  genCode :: NonEmpty Expr -> Reader Ctx Text
  genCode exprs = do
    oldIndent <- asks indentation
    let indent = oldIndent <> "  "
    exprs' <- local (withIndentation indent) (mapM genCode exprs)
    let (stmts, expr) = unsnoc' exprs'
    let stmts' = concat $ map (\stmt -> [i|#{indent}#{stmt};\n|] :: Text) stmts
    let expr' = [i|#{indent}return #{expr};|] :: Text
    return [i|{\n#{stmts'}#{expr'}\n#{oldIndent}}|]

instance CodeGens Expr Ctx where
  genCode :: Expr -> Reader Ctx Text
  genCode (Int v _) = return (pack $ show v)
  genCode (Float v _) = return (pack $ show v)
  genCode (Id (Var name _)) = return name
  genCode (App f args _) = do
    f' <- genCode f
    args' <- (intercalate ", ") <$> mapM genCode args
    return [i|#{f'}(#{args'})|]
  genCode (Fun params body _) = do
    let params' = intercalate ", " (map varName params)
    body' <- case body of
      Block exprs _ -> genCode exprs
      _ -> genCode body
    return [i|(#{params'}) => #{body'}|]
  genCode (Block exprs _) = do
    content <- genCode exprs
    return [i|(() => #{content})()|]
  genCode (Parens expr) = do
    expr' <- genCode expr
    return [i|(#{expr'})|]

instance CodeGens (Bind () (Closure any Expr)) Ctx where
  genCode :: Bind () (Closure any Expr) -> Reader Ctx Text
  genCode (Bind (Var name _) _ (Closure _ expr)) = do
    expr' <- genCode expr
    return [i|const #{name} = #{expr'};|]

instance CodeGens Module Ctx where
  genCode :: Module -> Reader Ctx Text
  genCode (Module bindings _) = do
    stmts <- mapM genCode bindings
    return (intercalate "\n" stmts <> "\n")

runGenCode :: (CodeGens t Ctx) => t -> Text
runGenCode x = runReader (genCode x) Ctx {indentation = ""}
