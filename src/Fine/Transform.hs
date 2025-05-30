module Fine.Transform (runTransformer) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Trans.SEC (SEC, fail', runSEC, warn)
import Control.Monad.Trans.State.Strict (gets, modify)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Fine.Error (
  Error (MutRecBindNotFun, UsageBeforeInit),
  Warning (UnusedVar),
 )
import Fine.Syntax (
  Bind (..),
  BindType (..),
  Defn (..),
  Expr (..),
  Id,
  Module (Module),
  ParsedModule (ParsedModule),
  Phase (Parsed, Transformed),
  Type (..),
  binder,
  range,
 )
import Fine.Syntax.Utils (isFunction)
import Fine.Transform.Check qualified as Check
import Fine.Transform.Term (runExprTransformer, transformType)

data Env = Env
  { currentExprBinders :: Set Id,
    usedExprBinders :: Set Id,
    allTypeBinders :: Set Id, -- all binders to make available for type functions
    currentTypeBinders :: Set Id,
    usedTypeBinders :: Set Id
  }

initEnv :: [Defn] -> SEC Env Error Warning ()
initEnv [] = return ()
initEnv (defn : defns) = do
  case defn of
    Defn _ -> return ()
    TypeDefn (TypeBind binder' _) ->
      modify (\st -> st{allTypeBinders = Set.insert binder' (allTypeBinders st)})
    DataDefn (TypeBind binder' _) _ -> do
      modify (\st -> st{allTypeBinders = Set.insert binder' (allTypeBinders st)})
    MutRecDefns _ -> return ()
  initEnv defns

-- TYPE

checkType :: Maybe Id -> Type Transformed -> SEC Env Error Warning ()
checkType optBinder type' = do
  let isTFun = case type' of
        TFun _ _ _ -> True
        _ -> False
  tVars <- gets (if isTFun then allTypeBinders else currentTypeBinders)
  let (usedTVars, errs, wrns) = Check.checkType tVars type'
  forM_ errs fail'
  forM_ wrns warn
  unless isTFun $ forM_ optBinder $ \binder' ->
    when (Set.member binder' usedTVars) (fail' $ UsageBeforeInit binder')
  modify (\st -> st{usedTypeBinders = Set.union usedTVars (usedTypeBinders st)})

transformTypeBind :: Bind OfType Parsed -> SEC Env Error Warning (Bind OfType Transformed)
transformTypeBind (TypeBind binder' type') = do
  let type'' = transformType type'
  modify (\st -> st{currentTypeBinders = Set.insert binder' (currentTypeBinders st)})
  checkType (Just binder') type''
  return (TypeBind binder' type'')

-- EXPR

checkExpr :: Maybe Id -> Expr Transformed -> SEC Env Error Warning ()
checkExpr optBinder expr = do
  vars <- gets currentExprBinders
  tVars <- gets allTypeBinders
  let (usedVars, usedTVars, errs, wrns) = Check.checkExpr vars tVars expr
  forM_ errs fail'
  forM_ wrns warn
  unless (isFunction expr) $ forM_ optBinder $ \binder' ->
    when (Set.member binder' usedVars) (fail' $ UsageBeforeInit binder')
  modify
    ( \st ->
        st
          { usedExprBinders = Set.union usedVars (usedExprBinders st),
            usedTypeBinders = Set.union usedTVars (usedTypeBinders st)
          }
    )

transformExprBind :: Bind OfExpr Parsed -> SEC Env Error Warning (Bind OfExpr Transformed)
transformExprBind bind = do
  do
    let binder' = binder bind
    modify (\st -> st{currentExprBinders = Set.insert binder' (currentExprBinders st)})
  case bind of
    ExprBind binder' type' expr -> do
      let type'' = transformType type'
      checkType Nothing type''
      let (expr', errs) = runExprTransformer expr
      forM_ errs fail'
      let expr'' = case type'' of
            Forall _ tparams _ -> GenFun (range expr') () tparams expr'
            _ -> expr'
      checkExpr (Just binder') expr''
      return (ExprBind binder' type'' expr'')
    ForeignBind binder' type' code -> do
      let type'' = transformType type'
      checkType Nothing type''
      return (ForeignBind binder' type'' code)

-- MODULE

checkRepeatedBinders :: [Defn] -> SEC r Error w ()
checkRepeatedBinders defns = do
  forM_ (Check.alreadyDefined $ concat $ map exprDefnBinders defns) fail'
  forM_ (Check.alreadyDefined $ mapMaybe typeDefnBinder defns) fail'
 where
  exprDefnBinders (Defn bind) = [binder bind]
  exprDefnBinders (DataDefn _ ctors) = map binder $ NonEmpty.toList ctors
  exprDefnBinders (MutRecDefns binds) = map binder $ NonEmpty.toList binds
  exprDefnBinders _ = []

  typeDefnBinder (TypeDefn bind) = Just (binder bind)
  typeDefnBinder (DataDefn bind _) = Just (binder bind)
  typeDefnBinder _ = Nothing

transformDefn :: Defn -> SEC Env Error Warning [Either (Bind OfExpr Transformed) (Bind OfType Transformed)]
transformDefn (Defn bind) = do
  bind' <- transformExprBind bind
  return [Left bind']
transformDefn (TypeDefn bind) = do
  bind' <- transformTypeBind bind
  return [Right bind']
transformDefn (DataDefn bind ctBinds) = do
  bind' <- transformTypeBind bind
  ctBinds' <- mapM transformExprBind ctBinds
  return (Right bind' : (map Left . NonEmpty.toList) ctBinds')
transformDefn (MutRecDefns binds) = do
  currentBinders <- gets currentExprBinders
  let binders = Set.fromList $ map binder $ NonEmpty.toList binds
  binds' <- forM binds $ \bind -> do
    let binder' = binder bind
    unless (hasFunExpr bind) (fail' $ MutRecBindNotFun $ binder')
    let rest = Set.delete binder' binders
    modify (\st -> st{currentExprBinders = Set.union rest currentBinders})
    transformExprBind bind
  modify (\st -> st{currentExprBinders = Set.union binders currentBinders})
  return (map Left $ NonEmpty.toList binds')
 where
  hasFunExpr :: Bind OfExpr Parsed -> Bool
  hasFunExpr (ExprBind _ _ expr) = isFunction expr
  hasFunExpr (ForeignBind _ _ _) = False

warnUnusedBinders :: SEC Env e Warning ()
warnUnusedBinders = do
  do
    all' <- gets currentExprBinders
    used <- gets usedExprBinders
    forM_ (Set.difference all' used) (warn . UnusedVar)
  do
    all' <- gets allTypeBinders
    used <- gets usedTypeBinders
    forM_ (Set.difference all' used) (warn . UnusedVar)

transformModule :: ParsedModule -> SEC Env Error Warning (Module Transformed)
transformModule (ParsedModule defns entry) = do
  checkRepeatedBinders defns
  initEnv defns
  (exprBinds, typeBinds) <- partitionEithers . concat <$> mapM transformDefn defns
  entry' <- forM entry $ \expr -> do
    let (expr', errs) = runExprTransformer expr
    forM_ errs fail'
    checkExpr Nothing expr'
    return expr'
  warnUnusedBinders
  return (Module exprBinds typeBinds entry')

runTransformer :: ParsedModule -> (Either (NonEmpty Error) (Module Transformed), [Warning])
runTransformer mdule =
  let env =
        Env Set.empty Set.empty Set.empty Set.empty Set.empty
   in runSEC (transformModule mdule) env
