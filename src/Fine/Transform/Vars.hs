module Fine.Transform.Vars (handleVars) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.RW (RW, asks, runRW, tell, withReader)
import Data.List.NonEmpty (toList)
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Error
  ( Error (AlreadyInScope, UndefinedVar, UsageBeforeInit),
    Errors,
    Warning (UnusedVar),
    collectError,
    collectWarning,
    collectWarnings,
  )
import Fine.Syntax.Abstract (Block (..), Expr (..), Pattern (..), boundVars)
import Fine.Syntax.Common (Id)

type FreeVars = Set Id

type GlobalVars = Set Id

data AvailableVars = AvailableVars
  { scoped :: Set Id,
    globally :: GlobalVars
  }

chechDefined :: Id -> RW GlobalVars Errors FreeVars
chechDefined var = do
  isDefined <- asks (S.member var)
  when (not isDefined) (tell $ collectError $ UndefinedVar var)
  return (S.singleton var)

blockFreeVars :: Block -> RW AvailableVars Errors FreeVars
blockFreeVars (Return expr) = withReader globally (freeVars expr)
blockFreeVars (Do expr block) = S.union <$> withReader globally (freeVars expr) <*> blockFreeVars block
blockFreeVars (Let _ bound _ expr block) = do
  do
    inScope <- asks (S.member bound . scoped)
    when inScope (tell $ collectError $ AlreadyInScope bound)
  exprVars <- withReader (S.insert bound . globally) (freeVars expr)
  case expr of
    Fun _ _ _ -> return ()
    _ -> when (S.member bound exprVars) (tell $ collectError $ UsageBeforeInit bound)
  blockVars <-
    withReader
      (\(AvailableVars sc gl) -> AvailableVars (S.insert bound sc) (S.insert bound gl))
      (blockFreeVars block)
  let allVars = S.union exprVars blockVars
  if S.member bound allVars
    then return (S.delete bound allVars)
    else tell (collectWarning $ UnusedVar bound) >> return allVars
blockFreeVars (Debug expr _ block) = S.union <$> withReader globally (freeVars expr) <*> blockFreeVars block
blockFreeVars Void = return S.empty
blockFreeVars (Loop cond actions block) =
  S.unions <$> sequence [withReader globally (freeVars cond), blockFreeVars actions, blockFreeVars block]

patternFreeVars :: Pattern -> RW GlobalVars Errors FreeVars
patternFreeVars (LiteralP _ _) = return S.empty
patternFreeVars (DataP tag patts _) = do
  pattsFreeVars <- S.unions <$> mapM patternFreeVars patts
  isDefined <- asks (S.member tag)
  if isDefined
    then return (S.insert tag pattsFreeVars)
    else tell (collectError $ UndefinedVar tag) >> return pattsFreeVars
patternFreeVars (RecordP props _) = S.unions <$> mapM (patternFreeVars . snd) props
patternFreeVars (TupleP patts _) = S.unions <$> mapM patternFreeVars patts
patternFreeVars (Capture _) = return S.empty
patternFreeVars (DiscardP _) = return S.empty

freeVars :: Expr -> RW GlobalVars Errors FreeVars
freeVars (Literal _ _) = return S.empty
freeVars (Data _ exprs _) = S.unions <$> mapM freeVars exprs
freeVars (Record props _) = S.unions <$> mapM (freeVars . snd) props
freeVars (Tuple exprs _) = S.unions <$> mapM freeVars exprs
freeVars (Var var) = chechDefined var
freeVars (Mut var expr) = S.union <$> chechDefined var <*> freeVars expr
freeVars (App f args _) = S.unions <$> mapM freeVars (f : args)
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
      (tell . collectWarnings . map UnusedVar . S.toList)
    return (S.unions $ zipWith S.difference freeVarsList boundVarsList)
  return (S.unions [exprFreeVars, pattsFreeVars, contsFreeVars])
freeVars (Fun _ (ExtExpr _) _) = return S.empty
freeVars (Fun params body _) = do
  let params' = S.fromList params
  bodyVars <- case body of
    Block block _ -> withReader (AvailableVars params' . S.union params') (blockFreeVars block)
    _ -> withReader (S.union params') (freeVars body)
  tell (collectWarnings $ map UnusedVar $ S.toList $ S.difference params' bodyVars)
  return (S.difference bodyVars params')
freeVars (Block block _) = withReader (AvailableVars S.empty) (blockFreeVars block)
freeVars (ExtExpr _) = return S.empty
freeVars (Closure _ expr _) = freeVars expr >> return S.empty

handleVars :: GlobalVars -> Expr -> (FreeVars, Errors)
handleVars vars expr = runRW (freeVars expr) vars
