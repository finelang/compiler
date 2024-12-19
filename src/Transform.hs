module Transform (transform, transformModule, try) where

import Control.Monad.Trans.RWS (RWS, ask, get, gets, modify, runRWS, tell)
import Data.List (sort)
import qualified Data.Map as M
import Data.Text (Text)
import Error
  ( ErrorCollection,
    SemanticError (..),
    SemanticWarning (BindingShadowing, UnusedVar),
    collectErrors,
    collectWarnings,
  )
import ShuntingYard (runSy)
import Syntax.Common
  ( Binder (Binder, binderName),
    Binding (Binding),
    Fixity,
    OpChain (..),
    Operator (Operator),
  )
import Syntax.Expr (Expr (..), Module (Module))
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
  tell $ collectErrors $ map RepeatedParam repeatedParams
  let params' = M.fromAscList $ map (\b -> (binderName b, b)) sortedParams
  let paramNames = M.map (const False) params'
  shadowed <- gets (`M.intersection` paramNames)
  tell $ collectWarnings $ map (BindingShadowing . (M.!) params') (M.keys shadowed)
  modify (M.union paramNames) -- override the values of shadowed
  body' <- transform body
  unused <- gets $ M.filter not . (`M.intersection` paramNames)
  tell $ collectWarnings $ map (UnusedVar . (M.!) params') (M.keys unused)
  modify $ (`M.union` shadowed) . (`M.difference` paramNames)
  return (Fun params body' r)
transform (P.Parens expr _) = transform expr
transform (P.Chain chain) = transformChain chain >>= shuntingYard

transformBinding :: Binding () P.Expr -> RWS Fixities Errors Vars (Binding () Expr)
transformBinding (Binding b@(Binder name _) ttype value isRec) = do
  value' <-
    if isRec
      then modify (M.insert name False) >> transform value
      else transform value >>= \v -> modify (M.insert name False) >> return v
  return (Binding b ttype value' isRec)

transformModule :: P.Module -> RWS Fixities Errors Vars Module
transformModule (P.Module bindings) = mapM transformBinding bindings >>= return . Module

try :: (p -> RWS Fixities Errors Vars q) -> p -> (Either [SemanticError] q, [SemanticWarning])
try f x =
  let (y, _, (errors, warnings)) = runRWS (f x) M.empty M.empty
   in (if null errors then Right y else Left errors, warnings)
