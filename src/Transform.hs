module Transform (transformParsedExpr) where

import Control.Monad (unless)
import Control.Monad.Trans.RWS (RWS, ask, get, gets, modify, runRWS, tell)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as M
import Data.Text (Text)
import Error
  ( ErrorCollection (ErrorCollection),
    SemanticError (..),
    SemanticWarning (UnusedVar),
    collectErrors,
    collectWarnings,
  )
import ShuntingYard (runSy)
import Syntax.Common (Binder (binderName), Fixity, OpChain (..), Operator (Operator))
import Syntax.Expr (Expr (..))
import qualified Syntax.Parsed as P

type Fixities = M.Map Text Fixity

type Errors = ErrorCollection SemanticError SemanticWarning

type Vars = M.Map Text Bool

shuntingYard :: OpChain Expr -> RWS Fixities Errors s Expr
shuntingYard chain = do
  ctx <- ask
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

transformChain :: OpChain P.Expr -> RWS Fixities Errors Vars (OpChain Expr)
transformChain (Operand expr) = Operand <$> transform expr
transformChain (Operation left op@(Operator name r) chain) = do
  left' <- transform left
  vars <- get
  if M.member name vars
    then modify (M.insert name True)
    else tell $ collectErrors [UndefinedVar name r]
  chain' <- transformChain chain
  return (Operation left' op chain')

transform :: P.Expr -> RWS Fixities Errors Vars Expr
transform (P.Int v r) = return (Int v r)
transform (P.Float v r) = return (Float v r)
transform (P.Var name r) = do
  vars <- get
  if M.member name vars
    then modify (M.insert name True)
    else tell $ collectErrors [UndefinedVar name r]
  return (Var name r)
transform (P.App f args r) = do
  f' <- transform f
  args' <- mapM transform args
  return (App f' args' r)
transform (P.Fun params body r) = do
  let sortedParams = sort params
  let repeatedParams = repeated sortedParams
  unless
    (null repeatedParams)
    (tell $ collectErrors [RepeatedParams $ head repeatedParams :| tail repeatedParams])
  let params' = M.fromAscList $ map (\b -> (binderName b, b)) sortedParams
  let paramNames = M.map (const False) params'
  shadowed <- gets (`M.intersection` paramNames)
  modify (M.union paramNames) -- override the values of shadowed
  body' <- transform body
  unused <- gets $ M.filter not . (`M.intersection` paramNames)
  tell $ collectWarnings $ map (UnusedVar . (M.!) params') (M.keys unused)
  modify $ (`M.union` shadowed) . (`M.difference` paramNames)
  return (Fun params body' r)
transform (P.Parens expr _) = transform expr
transform (P.Chain chain) = transformChain chain >>= shuntingYard

transformParsedExpr :: P.Expr -> (Either [SemanticError] Expr, [SemanticWarning])
transformParsedExpr pexpr =
  let (expr, _, ErrorCollection errors warnings) = runRWS (transform pexpr) M.empty M.empty
   in (if null errors then Right expr else Left errors, warnings)
