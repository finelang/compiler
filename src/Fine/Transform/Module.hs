module Fine.Transform.Module (runTransform) where

import Control.Monad (unless)
import Control.Monad.Trans.SW (SW, gets, modify, runSW, tell)
import Data.List.Extra (repeated)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Error (Error (..), Errors, Warning (UnusedVar), collectErrors, collectWarnings)
import Fine.Syntax.Common
  ( Bind (Bind),
    Fixities,
    Fixity (Fixity),
    Prop (SelfProp),
    Var,
    VariantSpec (VariantSpec),
    VariantSpecs,
    binder,
    boundValue,
  )
import Fine.Syntax.Expr (Closure (Closure), Expr (..), Module (EntryModule, Module), closureVars)
import qualified Fine.Syntax.ParsedExpr as P
import qualified Fine.Transform.Expr as TE (runTransform)
import Fine.Transform.FreeVars (runFreeVars)

data State = State
  { freeVars :: Set Var,
    closures :: Map Var (Closure Expr),
    fixities :: Fixities,
    variantSpecs :: VariantSpecs,
    entryClosure :: Maybe (Closure Expr)
  }

extractCt :: VariantSpec -> SW State Errors (Bind () (Closure Expr))
extractCt spec@(VariantSpec var props _ r) = do
  tell (collectErrors $ map RepeatedProp $ repeated props)
  specs <- gets variantSpecs
  if M.member var specs
    then tell (collectErrors [RepeatedVariant var])
    else modify (\st -> st {variantSpecs = M.insert var spec specs})
  let props' = map (\prop -> SelfProp prop) props
  let varnt = Variant var props' r
  let value = if null props then varnt else Fun props varnt r
  let closure = Closure M.empty value Nothing
  modify (\st -> st {closures = M.insert var closure (closures st), freeVars = S.insert var (freeVars st)})
  return (Bind var () closure)

transformExpr :: P.Expr -> SW State Errors Expr
transformExpr expr = do
  fixs <- gets fixities
  specs <- gets variantSpecs
  let (expr', transfErrors) = TE.runTransform fixs specs expr
  tell transfErrors
  return expr'

transformDefns :: [P.Defn] -> SW State Errors [Bind () (Closure Expr)]
transformDefns [] = return []
transformDefns (P.FixDefn fix@(Fixity _ prec) op : defns) = do
  unless (0 <= prec && prec < 10) (tell $ collectErrors [InvalidPrecedence 0 10 op]) -- TODO: read from some config
  fixities' <- gets fixities
  if M.member op fixities'
    then tell (collectErrors [RepeatedFixity op])
    else modify (\ctx -> ctx {fixities = M.insert op fix fixities'})
  transformDefns defns
transformDefns (P.DtypeDefn specs : defns) = do
  ctors <- mapM extractCt specs
  bs <- transformDefns defns
  return (ctors ++ bs)
transformDefns (P.Defn (Bind bder _ v) : defns) = do
  v' <- transformExpr v
  currentFreeVars <- do
    vs <- gets freeVars
    if S.member bder vs
      then tell (collectErrors [RepeatedVar bder]) >> return vs
      else return (S.insert bder vs)
  let (vFreeVars, fvErrors) = runFreeVars currentFreeVars v'
  tell fvErrors
  currentEnv <- gets closures
  let recBder = if S.member bder vFreeVars then Just bder else Nothing
  let closure = Closure (M.restrictKeys currentEnv vFreeVars) v' recBder
  let b' = Bind bder () closure
  modify (\st -> st {freeVars = S.union currentFreeVars vFreeVars, closures = M.insert bder closure currentEnv})
  bs' <- transformDefns defns
  return (b' : bs')
transformDefns (P.EntryDefn expr : _) = do
  expr' <- transformExpr expr
  exprFreeVars <- do
    currentFreeVars <- gets freeVars
    let (fvs, errors) = runFreeVars currentFreeVars expr'
    tell errors
    return fvs
  closure <- do
    currentEnv <- gets closures
    return (Closure (M.restrictKeys currentEnv exprFreeVars) expr' Nothing)
  modify (\st -> st {entryClosure = Just closure})
  return [] -- if entry defn exists, it should be the last

checkUnusedTopBinds :: [Bind () (Closure v)] -> Errors
checkUnusedTopBinds bs =
  let used = S.fromList $ concat $ map (closureVars . boundValue) bs
      binders = S.fromList $ map binder bs
   in collectWarnings $ map UnusedVar $ S.toList $ S.difference binders used

transform :: P.Module -> SW State Errors Module
transform (P.Module defns) = do
  bindings <- transformDefns defns
  tell (checkUnusedTopBinds bindings)
  fixities' <- gets fixities
  specs <- gets variantSpecs
  entry <- gets entryClosure
  return $ case entry of
    Nothing -> Module bindings fixities' specs
    Just closure -> EntryModule bindings fixities' specs closure

runTransform :: P.Module -> (Either [Error] Module, [Warning])
runTransform mdule =
  let st = State S.empty M.empty M.empty M.empty Nothing
      (mdule', _, (errors, warnings)) = runSW (transform mdule) st
   in (if null errors then Right mdule' else Left errors, warnings)
