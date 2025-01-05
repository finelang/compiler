module Fine.Transform.Module (runTransform) where

import Control.Monad (unless)
import Control.Monad.Trans.RWS (RWS, asks, get, gets, local, modify, runRWS, tell)
import Data.List.Extra (repeated)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Error (Error (..), Errors, Warning (UnusedVar), collectErrors, collectWarnings)
import Fine.Syntax.Common
  ( Bind (Bind),
    Data (Data),
    Fixities,
    Fixity (Fixity),
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

data RCtx = RCtx
  { vars :: Set Var,
    env :: Map Var (Closure Expr)
  }

data SCtx = SCtx
  { fixities :: Fixities,
    variantSpecs :: VariantSpecs,
    entryClosure :: Maybe (Closure Expr)
  }

handleSpec :: VariantSpec -> RWS r Errors SCtx P.Defn
handleSpec spec@(VariantSpec var props _ r) = do
  tell (collectErrors $ map RepeatedProp $ repeated props)
  specs' <- gets variantSpecs
  if M.member var specs'
    then tell (collectErrors [RepeatedVariant var])
    else modify (\ctx -> ctx {variantSpecs = M.insert var spec specs'})
  let data' = Data $ map (\prop -> (prop, P.Id prop)) props
  let varnt = P.Variant var data' r
  let value = if null props then varnt else P.Fun props varnt r
  return $ P.Defn (Bind var () value)

transformToExpr :: P.Expr -> RWS r Errors SCtx Expr
transformToExpr expr = do
  (SCtx fixs specs _) <- get
  let (expr', transfErrors) = TE.runTransform fixs specs expr
  tell transfErrors
  return expr'

transformDefns :: [P.Defn] -> RWS RCtx Errors SCtx [Bind () (Closure Expr)]
transformDefns [] = return []
transformDefns (P.FixDefn fix@(Fixity _ prec) op : defns) = do
  unless (0 <= prec && prec < 10) (tell $ collectErrors [InvalidPrecedence 0 10 op]) -- TODO: read from some config
  fixities' <- gets fixities
  if M.member op fixities'
    then tell (collectErrors [RepeatedFixity op])
    else modify (\ctx -> ctx {fixities = M.insert op fix fixities'})
  transformDefns defns
transformDefns (P.DtypeDefn specs : defns) = do
  ctors <- mapM handleSpec specs
  transformDefns (ctors ++ defns)
transformDefns (P.Defn (Bind bder _ v) : defns) = do
  v' <- transformToExpr v
  currentFreeVars <- do
    vs <- asks vars
    if S.member bder vs
      then tell (collectErrors [RepeatedVar bder]) >> return vs
      else return (S.insert bder vs)
  let (vFreeVars, fvErrors) = runFreeVars currentFreeVars v'
  tell fvErrors
  currentEnv <- asks env
  let recBder = if S.member bder vFreeVars then Just bder else Nothing
  let closure = Closure (M.restrictKeys currentEnv vFreeVars) v' recBder
  let b' = Bind bder () closure
  bs' <-
    local
      (\ctx -> ctx {vars = S.union currentFreeVars vFreeVars, env = M.insert bder closure currentEnv})
      (transformDefns defns)
  return (b' : bs')
transformDefns (P.EntryDefn expr : _) = do
  expr' <- transformToExpr expr
  exprFreeVars <- do
    currentFreeVars <- asks vars
    let (fvs, errors) = runFreeVars currentFreeVars expr'
    tell errors
    return fvs
  closure <- do
    currentEnv <- asks env
    return (Closure (M.restrictKeys currentEnv exprFreeVars) expr' Nothing)
  modify (\ctx -> ctx {entryClosure = Just closure})
  return [] -- if entry defn exists, it should be the last

checkUnusedTopBinds :: [Bind () (Closure v)] -> Errors
checkUnusedTopBinds bs =
  let used = S.fromList $ concat $ map (closureVars . boundValue) bs
      binders = S.fromList $ map binder bs
   in collectWarnings $ map UnusedVar $ S.toList $ S.difference binders used

transform :: P.Module -> RWS RCtx Errors SCtx Module
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
  let rctx = RCtx S.empty M.empty
      sctx = SCtx M.empty M.empty Nothing
      (mdule', _, (errors, warnings)) = runRWS (transform mdule) rctx sctx
   in (if null errors then Right mdule' else Left errors, warnings)
