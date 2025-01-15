module Fine.Transform.Vars (handleVars) where

import Control.Monad (forM_)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Error
  ( Error (UndefinedVar),
    Errors,
    Warning (UnusedVar),
    collectErrors,
    collectWarnings,
  )
import Fine.Syntax (Expr (..), boundVars)
import Fine.Syntax.Common (Prop (..), Var)

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

propFreeVars :: Prop Expr -> Writer Errors VarOcurrences
propFreeVars (NamedProp _ expr) = freeVars expr
propFreeVars (SpreadProp expr) = freeVars expr

freeVars :: Expr -> Writer Errors VarOcurrences
freeVars (Int _ _) = return M.empty
freeVars (Float _ _) = return M.empty
freeVars (Str _ _) = return M.empty
freeVars (Unit _) = return M.empty
freeVars (Obj props _) = unions' <$> mapM propFreeVars props
freeVars (Variant _ props _) = unions' <$> mapM propFreeVars props
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
  let (patts, exprs) = unzip (L.toList matches)
  let boundVarsList = map (S.fromList . boundVars) patts
  freeVarsList <- mapM freeVars exprs
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
freeVars (ExtId _) = return M.empty
freeVars (ExtOpApp _ l r) = unions' <$> mapM freeVars [l, r]
freeVars (Debug expr _) = freeVars expr

undefinedVars :: Set Var -> VarOcurrences -> [Var]
undefinedVars available free =
  let available' = M.fromAscList $ map (\v -> (v, ())) $ S.toAscList available
   in concat $ M.elems $ M.difference free available'

handleVars :: Set Var -> Expr -> (Set Var, Errors)
handleVars available expr =
  let (free, errors) = runWriter (freeVars expr)
      free' = S.intersection available (M.keysSet free)
      undefined' = undefinedVars available free
   in (free', errors <> collectErrors (map UndefinedVar undefined'))
