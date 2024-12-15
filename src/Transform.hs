module Transform (transformParsedExpr) where

import Control.Monad (unless)
import Control.Monad.Trans.RWS (RWS, asks, local, runRWS, tell)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Error (ErrorCollection (ErrorCollection), SemanticError (..), SemanticWarning, collectError)
import ShuntingYard (runSy)
import Syntax.Common (Binder (binderName), Fixity, OpChain (..), Operator (Operator))
import Syntax.Expr (Expr (..))
import qualified Syntax.Parsed as P

data Ctx = Ctx
  { vars :: S.Set Text,
    fixities :: M.Map Text Fixity
  }

type Errors = ErrorCollection SemanticError SemanticWarning

type UnusedBinders = S.Set Binder

emptyCtx :: Ctx
emptyCtx = Ctx S.empty M.empty

addVars :: [Text] -> Ctx -> Ctx
addVars bs' (Ctx bs fs) = Ctx (S.union bs $ S.fromList bs') fs

shuntingYard :: OpChain Expr -> RWS Ctx Errors s Expr
shuntingYard chain = do
  ctx <- asks fixities
  let (expr, errors) = runSy ctx chain
  tell errors
  return expr

repeated :: (Ord a) => [a] -> [a]
repeated xs = go (sort xs)
  where
    go [] = []
    go [_] = []
    go (x : y : zs) | x == y = x : go (dropWhile (== x) zs)
    go (_ : y : zs) = go (y : zs)

transformChain :: OpChain P.Expr -> RWS Ctx Errors UnusedBinders (OpChain Expr)
transformChain (Operand expr) = Operand <$> transform expr
transformChain (Operation left op@(Operator name range) chain) = do
  left' <- transform left
  ctx <- asks vars
  unless (S.member name ctx) (tell $ collectError $ UndefinedVar name range)
  chain' <- transformChain chain
  return (Operation left' op chain')

transform :: P.Expr -> RWS Ctx Errors UnusedBinders Expr
transform (P.Int v r) = return (Int v r)
transform (P.Float v r) = return (Float v r)
transform (P.Var name r) = do
  ctx <- asks vars
  unless (S.member name ctx) (tell $ collectError $ UndefinedVar name r)
  return (Var name r)
transform (P.App f args r) = do
  f' <- transform f
  args' <- mapM transform args
  return (App f' args' r)
transform (P.Fun params body r) = do
  let paramNames = map binderName params
  let repeatedParams = repeated paramNames
  unless
    (null repeatedParams)
    (tell $ collectError $ RepeatedParams (head repeatedParams :| tail repeatedParams) r)
  body' <- local (addVars paramNames) (transform body)
  return (Fun params body' r)
transform (P.Parens expr _) = transform expr
transform (P.Chain chain) = transformChain chain >>= shuntingYard

transformParsedExpr :: P.Expr -> (Either [SemanticError] Expr, [SemanticWarning])
transformParsedExpr pexpr =
  let (expr, _, ErrorCollection errors warnings) = runRWS (transform pexpr) emptyCtx S.empty
   in (if null errors then Right expr else Left errors, warnings)
