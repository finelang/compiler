module Transform.FreeVars (runFreeVars) where

import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Error (Error (UndefinedVar), Errors, Warning (UnusedVar), collectErrors, collectWarnings)
import Syntax.Common (OpChain (..), Var)
import Syntax.Parsed (Expr (..))

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

freeVars :: Expr -> Writer Errors Vars
freeVars (Int _ _) = return M.empty
freeVars (Float _ _) = return M.empty
freeVars (Obj members _) = unions' <$> (mapM (freeVars . snd) members)
freeVars (Id var) = return (singleton' var)
freeVars (App f args _) = do
  fVars <- freeVars f
  argVars <- mapM freeVars args
  return $ unions' (fVars : argVars)
freeVars (Fun params body _) = do
  let params' = M.fromList $ map (\v -> (v, v)) params
  bodyVars <- freeVars body
  tell (collectWarnings $ map UnusedVar $ M.elems $ M.difference params' bodyVars)
  return (M.difference bodyVars params')
freeVars (Parens expr) = freeVars expr
freeVars (Block exprs _) = unions' <$> (mapM freeVars exprs)
freeVars (Chain chain) = chainFreeVars chain

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
