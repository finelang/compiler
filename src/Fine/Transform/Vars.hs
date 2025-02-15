module Fine.Transform.Vars (handleVars) where

import Control.Monad (forM_)
import Control.Monad.Trans.RW (RW, asks, runRW, tell, withReader)
import Data.List.NonEmpty (cons, toList)
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
import Fine.Syntax.Abstract (Block (..), Expr (..), Pattern (..), boundVars)
import Fine.Syntax.Common (Id)

type AvailableVars = Set Id

type FreeVars = Set Id

data VarStatus
  = Undefined Id
  | Unused Id

justUndefined :: VarStatus -> Maybe Id
justUndefined (Undefined var) = Just var
justUndefined _ = Nothing

justUnused :: VarStatus -> Maybe Id
justUnused (Unused var) = Just var
justUnused _ = Nothing

blockFreeVars :: Block -> RW AvailableVars [VarStatus] FreeVars
blockFreeVars (Return expr) = freeVars expr
blockFreeVars (Do expr block) = S.union <$> freeVars expr <*> blockFreeVars block
blockFreeVars (Debug expr block) = S.union <$> freeVars expr <*> blockFreeVars block
blockFreeVars (Let bound _ expr block) = do
  exprVars <- freeVars expr
  blockVars <- withReader (S.insert bound) (blockFreeVars block)
  if S.member bound blockVars
    then return (S.union exprVars $ S.delete bound blockVars)
    else do
      tell [Unused bound]
      return (S.union exprVars blockVars)

patternFreeVars :: Pattern -> RW AvailableVars [VarStatus] FreeVars
patternFreeVars (LiteralP _ _) = return S.empty
patternFreeVars (DataP tag patts _) = do
  pattsFreeVars <- S.unions <$> mapM patternFreeVars patts
  isDefined <- asks (S.member tag)
  if isDefined
    then return (S.insert tag pattsFreeVars)
    else tell [Undefined tag] >> return pattsFreeVars
patternFreeVars (RecordP props _) = S.unions <$> mapM (patternFreeVars . snd) props
patternFreeVars (TupleP patts _) = S.unions <$> mapM patternFreeVars patts
patternFreeVars (Capture _) = return S.empty

-- a version of 'Fine.FreeVars.freeVars' that collects:
-- 1- undefined vars (given the available vars at the moment)
-- 2- unused vars (calculated with free vars and bound vars)
freeVars :: Expr -> RW AvailableVars [VarStatus] FreeVars
freeVars (Literal _ _) = return S.empty
freeVars (Data _ exprs _) = S.unions <$> mapM freeVars exprs
freeVars (Record props _) = S.unions <$> mapM (freeVars . snd) props
freeVars (Tuple exprs _) = S.unions <$> mapM freeVars exprs
freeVars (Var var) = do
  isDefined <- asks (S.member var)
  if isDefined
    then return (S.singleton var)
    else do
      tell [Undefined var]
      return S.empty
freeVars (App f args _) = S.unions <$> mapM freeVars (cons f args)
freeVars (Access expr _) = freeVars expr
freeVars (Index expr _ _) = freeVars expr
freeVars (Cond cond yes no _) = S.unions <$> mapM freeVars [cond, yes, no]
freeVars (PatternMatch expr matches _) = do
  exprFreeVars <- freeVars expr
  let (patts, conts) = unzip (toList matches)
  pattsFreeVars <- S.unions <$> mapM patternFreeVars patts
  contsFreeVars <- do
    let boundVarsList = map (S.fromList . boundVars) patts
    freeVarsList <-
      mapM
        (\(bvs, cont) -> withReader (S.union bvs) (freeVars cont))
        (zip boundVarsList conts)
    forM_
      (zipWith S.difference boundVarsList freeVarsList)
      (tell . map Unused . S.toList)
    return (S.unions $ zipWith S.difference freeVarsList boundVarsList)
  return (S.unions [exprFreeVars, pattsFreeVars, contsFreeVars])
freeVars (Fun params body _) = do
  let params' = S.fromList (toList params)
  bodyVars <- withReader (S.union params') (freeVars body)
  tell (map Unused $ S.toList $ S.difference params' bodyVars)
  return (S.difference bodyVars params')
freeVars (Block block _) = blockFreeVars block
freeVars (ExtExpr _) = return S.empty
freeVars (Closure _ _ _) = return S.empty

handleVars :: AvailableVars -> Expr -> (FreeVars, Errors)
handleVars vars expr =
  let (free, statuses) = runRW (freeVars expr) vars
      errors =
        (collectErrors $ mapMaybe (fmap UndefinedVar . justUndefined) statuses)
          <> (collectWarnings $ mapMaybe (fmap UnusedVar . justUnused) statuses)
   in (free, errors)
