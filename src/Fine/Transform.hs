module Fine.Transform (runTransformer) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Collector (Collector)
import Control.Monad.Collector qualified as Collector
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Errors (ErrorsT)
import Control.Monad.Trans.Errors qualified as Errors
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify)
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty)
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
import Fine.Transform.Term (runExprTransformer, transformType)

type SEC s e c a = StateT s (ErrorsT e (Collector c)) a

failure :: e -> SEC s e c a
failure = lift . Errors.failure

fromEither :: Either (NonEmpty e) a -> SEC s e c a
fromEither = lift . Errors.fromEither

collect :: c -> SEC s e c ()
collect = lift . lift . Collector.collect

data Env = Env
  { currentExprBinders :: Set Id,
    usedExprBinders :: Set Id,
    allTypeBinders :: Set Id, -- all binders to make available for type functions
    currentTypeBinders :: Set Id,
    usedTypeBinders :: Set Id
  }

initEnv :: [Defn] -> SEC Env e c ()
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
  let (result, wrns) = runCheckType tVars type'
  forM_ wrns collect
  usedTVars <- fromEither result
  unless isTFun $ forM_ optBinder $ \binder' ->
    when (Set.member binder' usedTVars) (failure $ UsageBeforeInit binder')
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
  let (result, wrns) = runCheckExpr vars tVars expr
  forM_ wrns collect
  (usedVars, usedTVars) <- fromEither result
  unless (isFunction expr) $ forM_ optBinder $ \binder' ->
    when (Set.member binder' usedVars) (failure $ UsageBeforeInit binder')
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
      expr' <- fromEither $ runExprTransformer expr
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

checkRepeatedBinders :: [Defn] -> SEC s Error c ()
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

transformExprDefn :: Defn -> SEC Env Error Warning [Bind OfExpr Transformed]
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

transformTypeDefn :: Defn -> SEC Env Error Warning (Maybe (Bind OfType Transformed))
transformTypeDefn (Defn _) = return Nothing
transformTypeDefn (TypeDefn bind) = Just <$> transformTypeBind bind
transformTypeDefn (DataDefn bind _) = Just <$> transformTypeBind bind
transformTypeDefn (MutRecDefns _) = return Nothing

warnUnusedBinders :: SEC Env e Warning ()
warnUnusedBinders = do
  do
    all' <- gets currentExprBinders
    used <- gets usedExprBinders
    forM_ (Set.toList $ all' \\ used) (collect . UnusedVar)
  do
    all' <- gets allTypeBinders
    used <- gets usedTypeBinders
    forM_ (Set.toList $ all' \\ used) (collect . UnusedVar)

transformModule :: ParsedModule -> SEC Env Error Warning (Module Transformed)
transformModule (ParsedModule defns entry) = do
  checkRepeatedBinders defns
  initEnv defns
  typeBinds <- catMaybes <$> mapM transformTypeDefn defns
  exprBinds <- concat <$> mapM transformExprDefn defns
  entry' <- forM entry $ \expr -> do
    expr' <- fromEither $ runExprTransformer expr
    checkExpr Nothing expr'
    return expr'
  warnUnusedBinders
  return (Module exprBinds typeBinds entry')

runTransformer :: ParsedModule -> (Either (NonEmpty Error) (Module Transformed), [Warning])
runTransformer mdule =
  let env =
        Env Set.empty Set.empty Set.empty Set.empty Set.empty
   in Collector.runCollector $ Errors.runErrorsT $ evalStateT (transformModule mdule) env
