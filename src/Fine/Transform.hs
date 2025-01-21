module Fine.Transform (runTransform) where

import Control.Monad (unless)
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
import Fine.Syntax
  ( Closure (Closure),
    Expr (..),
    Module (EntryModule, Module),
    VariantSpec (VariantSpec),
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
import qualified Fine.Syntax.Parsed as P
import qualified Fine.Transform.Expr as TE (runTransform)
import Fine.Transform.Vars (handleVars)

data State = State
  { freeVars :: Set Var,
    closures :: Map Var (Closure Expr),
    fixities :: Map Var Fixity,
    variantSpecs :: Map Var VariantSpec,
    transformEmptyVariant :: Bool
  }

transformExpr :: P.Expr -> SW State Errors Expr
transformExpr expr = do
  fixs <- gets fixities
  specs <- gets variantSpecs
  tev <- gets transformEmptyVariant
  let (expr', transfErrors) = TE.runTransform fixs specs tev expr
  tell transfErrors
  return expr'

transformBind :: Bind t P.Expr -> SW State Errors (Bind t (Closure Expr))
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

transformDefns :: [P.Defn] -> SW State Errors [Bind () (Closure Expr)]
transformDefns [] = return []
transformDefns (P.FixDefn fix@(Fixity _ prec) op : defns) = do
  unless (0 <= prec && prec < 10) (tell $ collectError $ InvalidPrecedence 0 10 op) -- TODO: read from some config
  fixities' <- gets fixities
  if M.member op fixities'
    then tell (collectError $ RepeatedFixity op)
    else modify (\ctx -> ctx {fixities = M.insert op fix fixities'})
  transformDefns defns
transformDefns (P.Defn bind : defns) = do
  bind' <- transformBind bind
  binds <- transformDefns defns
  return (bind' : binds)
transformDefns (P.CtorDefn tag props optExt r : defns) = do
  modify
    (\st -> st {variantSpecs = M.insert tag (VariantSpec tag props) (variantSpecs st)})
  let value = case optExt of
        Just ext -> P.ExtId ext
        Nothing ->
          let props' = map (\prop -> NamedProp prop (P.Id prop)) props
              varnt = P.Variant tag props' r
           in if null props then varnt else P.Fun props varnt r
  bind <- do
    modify (\st -> st {transformEmptyVariant = False})
    b <- transformBind $ Bind tag () value
    modify (\st -> st {transformEmptyVariant = True})
    return b
  binds <- transformDefns defns
  return (bind : binds)

transformEntryExpr :: P.Expr -> SW State Errors (Closure Expr)
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

transform :: P.Module -> SW State Errors Module
transform (P.Module defns optExpr) = do
  bindings <- transformDefns defns
  optClosure <- mapM transformEntryExpr optExpr
  tell (checkUnusedTopBinds bindings optClosure)
  fixities' <- gets fixities
  specs <- gets variantSpecs
  return $ case optClosure of
    Nothing -> Module bindings fixities' specs
    Just closure -> EntryModule bindings fixities' specs closure

runTransform :: P.Module -> (Either [Error] Module, [Warning])
runTransform mdule =
  let st = State S.empty M.empty M.empty M.empty True
      (mdule', _, (errors, warnings)) = runSW (transform mdule) st
   in (if null errors then Right mdule' else Left errors, warnings)
