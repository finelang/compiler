module Fine.Transform (runTransform) where

import Control.Monad (liftM2, unless)
import Control.Monad.Trans.SW (SW, gets, modify, runSW, tell)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
  ( Closure (Closure),
    Expr (..),
    Module (EntryModule, Module),
    closureVars,
  )
import Fine.Syntax.Common
  ( Bind (..),
    Fixity (Fixity),
    Prop (NamedProp),
    Var,
    binder,
    boundValue,
  )
import qualified Fine.Syntax.Concrete as C
import Fine.Transform.Common (Fixities, VariantSpec (VariantSpec), VariantSpecs)
import qualified Fine.Transform.Expr as TE (runTransform)
import Fine.Transform.Vars (handleVars)

data State = State
  { freeVars :: Set Var,
    closures :: Map Var (Closure Expr),
    fixities :: Fixities,
    variantSpecs :: VariantSpecs
  }

transformExpr :: C.Expr -> SW State Errors Expr
transformExpr expr = do
  fixs <- gets fixities
  specs <- gets variantSpecs
  let (expr', transfErrors) = TE.runTransform fixs specs expr
  tell transfErrors
  return expr'

transformBind :: Bind t C.Expr -> SW State Errors (Bind t (Closure Expr))
transformBind (Bind bder t v) = do
  value <- transformExpr v
  currentFreeVars <- do
    vs <- gets freeVars
    if S.member bder vs
      then tell (collectError $ RepeatedVar bder) >> return vs
      else return (S.insert bder vs)
  let (vFreeVars, fvErrors) = handleVars currentFreeVars value
  tell fvErrors
  currentEnv <- gets closures
  let recBder = if S.member bder vFreeVars then Just bder else Nothing
  let closure = Closure (M.restrictKeys currentEnv vFreeVars) value recBder
  modify (\st -> st {freeVars = S.union currentFreeVars vFreeVars, closures = M.insert bder closure currentEnv})
  return (Bind bder t closure)

transformDefns :: [C.Defn] -> SW State Errors [Bind () (Closure Expr)]
transformDefns [] = return []
transformDefns (C.FixDefn fix@(Fixity _ prec) op : defns) = do
  unless (0 <= prec && prec < 10) (tell $ collectError $ InvalidPrecedence 0 10 op) -- TODO: read from some config
  fixities' <- gets fixities
  if M.member op fixities'
    then tell (collectError $ RepeatedFixity op)
    else modify (\ctx -> ctx {fixities = M.insert op fix fixities'})
  transformDefns defns
transformDefns (C.Defn bind : defns) = liftM2 (:) (transformBind bind) (transformDefns defns)
transformDefns (C.CtorDefn tag props optExt r : defns) = do
  let value = case optExt of
        Just ext -> ExtExpr ext
        Nothing ->
          let props' = map (\prop -> NamedProp prop (Id prop)) props
              varnt = Variant tag props' r
           in if null props then varnt else Fun props varnt r
  let closure = Closure M.empty value Nothing
  let bind = Bind tag () closure
  modify
    ( \st ->
        st
          { freeVars = S.insert tag (freeVars st),
            closures = M.insert tag closure (closures st),
            variantSpecs = M.insert tag (VariantSpec tag props) (variantSpecs st)
          }
    )
  fmap (bind :) (transformDefns defns)

transformEntryExpr :: C.Expr -> SW State Errors (Closure Expr)
transformEntryExpr expr = do
  expr' <- transformExpr expr
  exprFreeVars <- do
    currentFreeVars <- gets freeVars
    let (fvs, errors) = handleVars currentFreeVars expr'
    tell errors
    return fvs
  closure <- do
    currentEnv <- gets closures
    return (Closure (M.restrictKeys currentEnv exprFreeVars) expr' Nothing)
  return closure

checkUnusedTopBinds :: [Bind () (Closure v)] -> Maybe (Closure v) -> Errors
checkUnusedTopBinds bs optCl =
  let used = S.fromList $ concat $ map (closureVars . boundValue) bs
      used' = case optCl of
        Just (Closure env _ _) -> S.union used (M.keysSet env)
        _ -> used
      binders = S.fromList $ map binder bs
   in collectWarnings $ map UnusedVar $ S.toList $ S.difference binders used'

transform :: C.Module -> SW State Errors Module
transform (C.Module defns optExpr) = do
  bindings <- transformDefns defns
  optClosure <- mapM transformEntryExpr optExpr
  tell (checkUnusedTopBinds bindings optClosure)
  fixities' <- gets fixities
  return $ case optClosure of
    Nothing -> Module bindings fixities'
    Just closure -> EntryModule bindings fixities' closure

runTransform :: C.Module -> (Either [Error] Module, [Warning])
runTransform mdule =
  let st = State S.empty M.empty M.empty M.empty
      (mdule', _, (errors, warnings)) = runSW (transform mdule) st
   in (if null errors then Right mdule' else Left errors, warnings)
