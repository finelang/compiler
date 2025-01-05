module Fine.Transform.FreeVars (runFreeVars) where

import Control.Monad (forM_)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.List.Extra (repeated)
import qualified Data.List.NonEmpty as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Error (Error (RepeatedVar, UndefinedVar), Errors, Warning (UnusedVar), collectErrors, collectWarnings)
import Fine.Syntax.Common (Data (Data), Var)
import Fine.Syntax.Expr (Expr (..))
import Fine.Syntax.Pattern (Pattern)
import qualified Fine.Syntax.Pattern as Patt

dataBoundVars :: Data Pattern -> [Var]
dataBoundVars (Data members) = concat $ map (boundVars . snd) members

boundVars :: Pattern -> [Var]
boundVars (Patt.Int _ _) = []
boundVars (Patt.Float _ _) = []
boundVars (Patt.Str _ _) = []
boundVars (Patt.Unit _) = []
boundVars (Patt.Obj d _) = dataBoundVars d
boundVars (Patt.Variant _ d _) = dataBoundVars d
boundVars (Patt.Tuple fst' snd' rest _) = concat $ map boundVars (fst' : snd' : rest)
boundVars (Patt.Capture var) = [var]

handleBoundVars :: [Var] -> Writer Errors (Set Var)
handleBoundVars vars = do
  tell (collectErrors $ map RepeatedVar $ repeated vars)
  return (S.fromList vars)

checkUnusedBoundVars :: Set Var -> Set Var -> Writer Errors ()
checkUnusedBoundVars free bound =
  tell (collectWarnings $ map UnusedVar $ S.toList $ S.difference bound free)

type VarOcurrences = Map Var [Var]

singleton' :: Var -> VarOcurrences
singleton' v = M.singleton v [v]

union' :: VarOcurrences -> VarOcurrences -> VarOcurrences
union' = M.unionWith (++)

unions' :: (Foldable t) => t VarOcurrences -> VarOcurrences
unions' = foldl union' M.empty

dataFreeVars :: Data Expr -> Writer Errors VarOcurrences
dataFreeVars (Data members) = unions' <$> mapM (freeVars . snd) members

freeVars :: Expr -> Writer Errors VarOcurrences
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
freeVars (PatternMatch expr matches _) = do
  exprVars <- freeVars expr
  let matches' = L.toList matches
  boundVarsList <- mapM (handleBoundVars . boundVars . fst) matches'
  freeVarsList <- mapM (freeVars . snd) matches'
  forM_ (zip (fmap M.keysSet freeVarsList) boundVarsList) (uncurry checkUnusedBoundVars)
  let freeVarsList' = zipWith M.withoutKeys freeVarsList boundVarsList
  return (unions' $ exprVars : freeVarsList')
freeVars (Fun params body _) = do
  let params' = M.fromList $ map (\v -> (v, v)) params
  bodyVars <- freeVars body
  tell (collectWarnings $ map UnusedVar $ M.elems $ M.difference params' bodyVars)
  return (M.difference bodyVars params')
freeVars (Parens expr) = freeVars expr
freeVars (Block exprs _) = unions' <$> mapM freeVars exprs
freeVars (ExtExpr _) = return M.empty

undefinedVars :: Set Var -> VarOcurrences -> [Var]
undefinedVars alreadyFree free =
  let alreadyFree' = M.fromAscList $ map (\v -> (v, ())) $ S.toAscList alreadyFree
   in concat $ M.elems $ M.difference free alreadyFree'

runFreeVars :: Set Var -> Expr -> (Set Var, Errors)
runFreeVars alreadyFree expr =
  let (free, errors) = runWriter (freeVars expr)
      free' = S.intersection alreadyFree (M.keysSet free)
      undefined' = undefinedVars alreadyFree free
   in (free', errors <> collectErrors (map UndefinedVar undefined'))
