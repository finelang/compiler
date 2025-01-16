module Fine.Transform.Vars (handleVars) where

import Control.Monad (forM_, liftM2)
import Control.Monad.Trans.RW (RW, asks, runRW, tell, withReader)
import qualified Data.List.NonEmpty as L
import Data.Maybe (mapMaybe)
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

type AvailableVars = Set Var

type FreeVars = Set Var

data VarStatus
  = Undefined Var
  | Unused Var

justUndefined :: VarStatus -> Maybe Var
justUndefined (Undefined var) = Just var
justUndefined _ = Nothing

justUnused :: VarStatus -> Maybe Var
justUnused (Unused var) = Just var
justUnused _ = Nothing

propFreeVars :: Prop Expr -> RW AvailableVars [VarStatus] FreeVars
propFreeVars (NamedProp _ expr) = freeVars expr
propFreeVars (SpreadProp expr) = freeVars expr

-- a version of 'Fine.FreeVars.freeVars' that collects:
-- 1- undefined vars (given the available vars at the moment)
-- 2- unused vars (calculated with free vars and bound vars)
freeVars :: Expr -> RW AvailableVars [VarStatus] FreeVars
freeVars (Int _ _) = return S.empty
freeVars (Float _ _) = return S.empty
freeVars (Str _ _) = return S.empty
freeVars (Unit _) = return S.empty
freeVars (Obj props _) = S.unions <$> mapM propFreeVars props
freeVars (Variant _ props _) = S.unions <$> mapM propFreeVars props
freeVars (Tuple fst' snd' rest _) = S.unions <$> mapM freeVars (fst' : snd' : rest)
freeVars (Id var) = do
  isDefined <- asks (S.member var)
  if isDefined
    then return (S.singleton var)
    else do
      tell [Undefined var]
      return S.empty
freeVars (App f args _) = S.unions <$> mapM freeVars (f : args)
freeVars (Access expr _) = freeVars expr
freeVars (Cond cond yes no _) = S.unions <$> mapM freeVars [cond, yes, no]
freeVars (PatternMatch expr matches _) = do
  exprVars <- freeVars expr
  let (patts, exprs) = unzip (L.toList matches)
  let boundVarsList = map (S.fromList . boundVars) patts
  freeVarsList <-
    mapM
      (\(bvs, expr') -> withReader (S.union bvs) (freeVars expr'))
      (zip boundVarsList exprs)
  forM_
    (zipWith S.difference boundVarsList freeVarsList)
    (tell . map Unused . S.toList)
  return (S.unions $ exprVars : zipWith S.difference freeVarsList boundVarsList)
freeVars (Fun params body _) = do
  let params' = S.fromList params
  bodyVars <- withReader (S.union params') (freeVars body)
  tell (map Unused $ S.toList $ S.difference params' bodyVars)
  return (S.difference bodyVars params')
freeVars (Parens expr) = freeVars expr
freeVars (Block exprs _) = S.unions <$> mapM freeVars exprs
freeVars (ExtId _) = return S.empty
freeVars (ExtOpApp _ l r) = liftM2 S.union (freeVars l) (freeVars r)
freeVars (Debug expr _) = freeVars expr

handleVars :: AvailableVars -> Expr -> (FreeVars, Errors)
handleVars vars expr =
  let (free, statuses) = runRW (freeVars expr) vars
      errors =
        (collectErrors $ mapMaybe (fmap UndefinedVar . justUndefined) statuses)
          <> (collectWarnings $ mapMaybe (fmap UnusedVar . justUnused) statuses)
   in (free, errors)
