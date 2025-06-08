module Fine.Transform (runTransformer) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Errors (Errors, runErrors)
import Control.Monad.Errors qualified as Errors
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Collector (CollectorT)
import Control.Monad.Trans.Collector qualified as Collector
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify)
import Data.List (singleton)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set, (\\))
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
import Fine.Transform.Check (alreadyDefined, runCheckExpr, runCheckType)
import Fine.Transform.Term (transformExpr, transformType)

type SCE s c e a = StateT s (CollectorT c (Errors e)) a

failure :: e -> SCE s c e a
failure = lift . lift . Errors.failure

collect :: c -> SCE s c e ()
collect = lift . Collector.collect

data Env = Env
  { currentExprBinders :: Set Id,
    usedExprBinders :: Set Id,
    allTypeBinders :: Set Id, -- all binders to make available for type functions
    currentTypeBinders :: Set Id,
    usedTypeBinders :: Set Id
  }

initEnv :: [Defn] -> SCE Env c e ()
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

checkType :: Maybe Id -> Type Transformed -> SCE Env Warning Error ()
checkType optBinder type' = do
  let isTFun = case type' of
        TFun _ _ _ -> True
        _ -> False
  tVars <- gets (if isTFun then allTypeBinders else currentTypeBinders)
  case runErrors $ runCheckType tVars type' of
    Left errs -> forM_ errs failure
    Right (usedTVars, wrns) -> do
      forM_ wrns collect
      unless isTFun $ forM_ optBinder $ \binder' ->
        when (Set.member binder' usedTVars) (failure $ UsageBeforeInit binder')
      modify (\st -> st{usedTypeBinders = Set.union usedTVars (usedTypeBinders st)})

transformTypeBind :: Bind OfType Parsed -> SCE Env Warning Error (Bind OfType Transformed)
transformTypeBind (TypeBind binder' type') = do
  let type'' = transformType type'
  modify (\st -> st{currentTypeBinders = Set.insert binder' (currentTypeBinders st)})
  checkType (Just binder') type''
  return (TypeBind binder' type'')

-- -- EXPR

checkExpr :: Maybe Id -> Expr Transformed -> SCE Env Warning Error ()
checkExpr optBinder expr = do
  vars <- gets currentExprBinders
  tVars <- gets allTypeBinders
  case runErrors $ runCheckExpr vars tVars expr of
    Left errs -> forM_ errs failure
    Right (usedVars, usedTVars, wrns) -> do
      forM_ wrns collect
      unless (isFunction expr) $ forM_ optBinder $ \binder' ->
        when (Set.member binder' usedVars) (failure $ UsageBeforeInit binder')
      modify
        ( \st ->
            st
              { usedExprBinders = Set.union usedVars (usedExprBinders st),
                usedTypeBinders = Set.union usedTVars (usedTypeBinders st)
              }
        )

transformExprBind :: Bind OfExpr Parsed -> SCE Env Warning Error (Bind OfExpr Transformed)
transformExprBind bind = do
  do
    let binder' = binder bind
    modify (\st -> st{currentExprBinders = Set.insert binder' (currentExprBinders st)})
  case bind of
    ExprBind binder' type' expr -> do
      let type'' = transformType type'
      checkType Nothing type''
      expr' <- lift $ lift $ transformExpr expr
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

checkRepeatedBinders :: [Defn] -> SCE s c Error ()
checkRepeatedBinders defns = do
  forM_ (alreadyDefined $ concat $ map exprDefnBinders defns) failure
  forM_ (alreadyDefined $ mapMaybe typeDefnBinder defns) failure
 where
  exprDefnBinders (Defn bind) = [binder bind]
  exprDefnBinders (DataDefn _ ctors) = map binder $ NonEmpty.toList ctors
  exprDefnBinders (MutRecDefns binds) = map binder $ NonEmpty.toList binds
  exprDefnBinders _ = []

  typeDefnBinder (TypeDefn bind) = Just (binder bind)
  typeDefnBinder (DataDefn bind _) = Just (binder bind)
  typeDefnBinder _ = Nothing

transformExprDefn :: Defn -> SCE Env Warning Error [Bind OfExpr Transformed]
transformExprDefn (Defn bind) = singleton <$> transformExprBind bind
transformExprDefn (TypeDefn _) = return []
transformExprDefn (DataDefn _ binds) = NonEmpty.toList <$> mapM transformExprBind binds
transformExprDefn (MutRecDefns binds) = do
  currentBinders <- gets currentExprBinders
  let binders = Set.fromList $ map binder $ NonEmpty.toList binds
  binds' <- forM binds $ \bind -> do
    let binder' = binder bind
    unless (hasFunExpr bind) (failure $ MutRecBindNotFun binder')
    let rest = Set.delete binder' binders
    modify (\st -> st{currentExprBinders = Set.union rest currentBinders})
    transformExprBind bind
  modify (\st -> st{currentExprBinders = Set.union binders currentBinders})
  return $ NonEmpty.toList binds'
 where
  hasFunExpr :: Bind OfExpr Parsed -> Bool
  hasFunExpr (ExprBind _ _ expr) = isFunction expr
  hasFunExpr (ForeignBind _ _ _) = False

transformTypeDefn :: Defn -> SCE Env Warning Error (Maybe (Bind OfType Transformed))
transformTypeDefn (Defn _) = return Nothing
transformTypeDefn (TypeDefn bind) = Just <$> transformTypeBind bind
transformTypeDefn (DataDefn bind _) = Just <$> transformTypeBind bind
transformTypeDefn (MutRecDefns _) = return Nothing

warnUnusedBinders :: SCE Env Warning e ()
warnUnusedBinders = do
  do
    all' <- gets currentExprBinders
    used <- gets usedExprBinders
    forM_ (Set.toList $ all' \\ used) (collect . UnusedVar)
  do
    all' <- gets allTypeBinders
    used <- gets usedTypeBinders
    forM_ (Set.toList $ all' \\ used) (collect . UnusedVar)

transformModule :: ParsedModule -> SCE Env Warning Error (Module Transformed)
transformModule (ParsedModule defns entry) = do
  checkRepeatedBinders defns
  initEnv defns
  typeBinds <- catMaybes <$> mapM transformTypeDefn defns
  exprBinds <- concat <$> mapM transformExprDefn defns
  entry' <- forM entry $ \expr -> do
    expr' <- lift $ lift $ transformExpr expr
    checkExpr Nothing expr'
    return expr'
  warnUnusedBinders
  return (Module exprBinds typeBinds entry')

runTransformer :: ParsedModule -> Errors Error (Module Transformed, [Warning])
runTransformer mdule =
  let env =
        Env Set.empty Set.empty Set.empty Set.empty Set.empty
   in Collector.runCollectorT (evalStateT (transformModule mdule) env)
