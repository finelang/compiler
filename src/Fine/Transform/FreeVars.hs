module Fine.Transform.FreeVars (runFreeVars) where

import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Error (Error (UndefinedVar), Errors, Warning (UnusedVar), collectErrors, collectWarnings)
import Fine.Syntax.Common (Data (Data), OpChain (..), Var)
import Fine.Syntax.Parsed (Expr (..))

type Vars = Map Var [Var]

singleton' :: Var -> Vars
singleton' v = M.singleton v [v]

union' :: Vars -> Vars -> Vars
union' = M.unionWith (++)

unions' :: (Foldable t) => t Vars -> Vars
unions' = foldl union' M.empty

chainFreeVars :: OpChain Expr -> Writer Errors Vars
chainFreeVars (Operand expr) = freeVars expr
chainFreeVars (Operation left op chain) = do
  leftFvs <- freeVars left
  chainFvs <- chainFreeVars chain
  return $ unions' [leftFvs, singleton' op, chainFvs]

dataFreeVars :: Data Expr -> Writer Errors Vars
dataFreeVars (Data members) = unions' <$> mapM (freeVars . snd) members

freeVars :: Expr -> Writer Errors Vars
freeVars (Int _ _) = return M.empty
freeVars (Float _ _) = return M.empty
freeVars (Str _ _) = return M.empty
freeVars (Unit _) = return M.empty
freeVars (Obj d _) = dataFreeVars d
freeVars (Variant _ d _) = dataFreeVars d
freeVars (Tuple fst' snd' rest _) = unions' <$> mapM freeVars (fst' : snd' : rest)
freeVars (Id var) = return (singleton' var)
freeVars (App f args _) = do
  fVars <- freeVars f
  argVars <- mapM freeVars args
  return $ unions' (fVars : argVars)
freeVars (Access expr _) = freeVars expr
freeVars (Cond cond yes no _) = unions' <$> mapM freeVars [cond, yes, no]
freeVars (Fun params body _) = do
  let params' = M.fromList $ map (\v -> (v, v)) params
  bodyVars <- freeVars body
  tell (collectWarnings $ map UnusedVar $ M.elems $ M.difference params' bodyVars)
  return (M.difference bodyVars params')
freeVars (Parens expr) = freeVars expr
freeVars (Block exprs _) = unions' <$> mapM freeVars exprs
freeVars (Chain chain) = chainFreeVars chain
freeVars (ExtExpr _) = return M.empty

undefinedVars :: Set Var -> Vars -> [Var]
undefinedVars alreadyFree free =
  let alreadyFree' = M.fromAscList $ map (\v -> (v, ())) $ S.toAscList alreadyFree
   in concat $ M.elems $ M.difference free alreadyFree'

runFreeVars :: Set Var -> Expr -> (Set Var, Errors)
runFreeVars alreadyFree expr =
  let (free, errors) = runWriter (freeVars expr)
      free' = S.intersection alreadyFree (M.keysSet free)
      undefined' = undefinedVars alreadyFree free
   in (free', errors <> collectErrors (map UndefinedVar undefined'))
