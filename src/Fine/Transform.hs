module Fine.Transform (runTransform) where

import Control.Monad (liftM2, unless)
import Control.Monad.Trans.SW (SW, gets, modify, runSW, tell)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Error
  ( Error (..),
    Errors,
    Warning (UnusedVar),
    collectError,
    collectWarnings,
  )
import Fine.Syntax.Abstract
  ( Expr (..),
    Module (EntryModule, Module),
  )
import Fine.Syntax.Common
  ( Bind (..),
    Fixity (Fixity),
    Var,
    binder,
    boundValue,
  )
import qualified Fine.Syntax.Concrete as C
import Fine.Transform.Common (CtBinders, Fixities)
import qualified Fine.Transform.Expr as TE (runTransform)
import Fine.Transform.Vars (handleVars)

data State = State
  { vars :: Set Var,
    env :: Map Var Expr,
    fixities :: Fixities,
    ctBinders :: CtBinders
  }

transformExpr :: C.Expr -> SW State Errors Expr
transformExpr expr = do
  fixs <- gets fixities
  cts <- gets ctBinders
  let (expr', transfErrors) = TE.runTransform fixs cts expr
  tell transfErrors
  return expr'

transformBind :: Bind t C.Expr -> SW State Errors (Bind t Expr)
transformBind (Bind bder t v) = do
  value <- transformExpr v
  currentFreeVars <- do
    vs <- gets vars
    if S.member bder vs
      then tell (collectError $ RepeatedVar bder) >> return vs
      else return (S.insert bder vs)
  let (vFreeVars, fvErrors) = handleVars currentFreeVars value
  tell fvErrors
  currentEnv <- gets env
  let selfBinder = if S.member bder vFreeVars then Just bder else Nothing
  let valueEnv = M.restrictKeys currentEnv vFreeVars
  let value' =
        if isNothing selfBinder && M.null valueEnv
          then value
          else Closure (M.restrictKeys currentEnv vFreeVars) value selfBinder
  modify (\st -> st {vars = S.union currentFreeVars vFreeVars, env = M.insert bder value' currentEnv})
  return (Bind bder t value')

transformDefns :: [C.Defn] -> SW State Errors [Bind () Expr]
transformDefns [] = return []
transformDefns (C.FixDefn fix@(Fixity _ prec) op : defns) = do
  unless (0 <= prec && prec < 10) (tell $ collectError $ InvalidPrecedence 0 10 op) -- TODO: read from some config
  fixities' <- gets fixities
  if M.member op fixities'
    then tell (collectError $ RepeatedFixity op)
    else modify (\ctx -> ctx {fixities = M.insert op fix fixities'})
  transformDefns defns
transformDefns (C.Defn bind : defns) = liftM2 (:) (transformBind bind) (transformDefns defns)
transformDefns (C.CtorDefn tag params r : defns) = do
  let value =
        let exprs = map Id params
            data' = Data tag exprs r
         in case params of
              [] -> data'
              (param : params') -> Fun (param :| params') data' r
  modify
    ( \st ->
        st
          { vars = S.insert tag (vars st),
            env = M.insert tag value (env st),
            ctBinders = S.insert tag (ctBinders st)
          }
    )
  fmap (Bind tag () value :) (transformDefns defns)

transformEntryExpr :: C.Expr -> SW State Errors Expr
transformEntryExpr expr = do
  expr' <- transformExpr expr
  exprFreeVars <- do
    currentFreeVars <- gets vars
    let (fvs, errors) = handleVars currentFreeVars expr'
    tell errors
    return fvs
  expr'' <- do
    currentEnv <- gets env
    let exprEnv = M.restrictKeys currentEnv exprFreeVars
    return $
      if M.null exprEnv
        then expr'
        else Closure exprEnv expr' Nothing
  return expr''

closureVars :: Expr -> Set Var
closureVars (Closure clEnv _ self) =
  let clVars = M.keysSet clEnv
   in case self of
        Just var -> S.insert var clVars
        Nothing -> clVars
closureVars _ = S.empty

checkUnusedTopBinds :: [Bind () Expr] -> Maybe Expr -> Errors
checkUnusedTopBinds bs optExpr =
  let used = foldMap (closureVars . boundValue) bs
      used' = case fmap closureVars optExpr of
        Just more -> S.union used more
        _ -> used
      binders = S.fromList $ map binder bs
   in collectWarnings $ map UnusedVar $ S.toList $ S.difference binders used'

transform :: C.Module -> SW State Errors Module
transform (C.Module defns optExpr) = do
  bindings <- transformDefns defns
  optExpr' <- mapM transformEntryExpr optExpr
  tell (checkUnusedTopBinds bindings optExpr')
  fixities' <- gets fixities
  return $ case optExpr' of
    Nothing -> Module bindings fixities'
    Just expr -> EntryModule bindings fixities' expr

runTransform :: C.Module -> (Either [Error] Module, [Warning])
runTransform mdule =
  let st = State S.empty M.empty M.empty S.empty
      (mdule', _, (errors, warnings)) = runSW (transform mdule) st
   in (if null errors then Right mdule' else Left errors, warnings)
