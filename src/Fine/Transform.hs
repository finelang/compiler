module Fine.Transform (runTransformer) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.SEC (SEC, fail', gets, modify, runSEC, warn)
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Fine.Error (
  Error (
    AlreadyInScope,
    InvalidPrecedence,
    RepeatedFixity,
    UsageBeforeInit
  ),
  Warning (UnusedVar),
 )
import Fine.Syntax (
  Bind (..),
  BindType (..),
  Defn (..),
  Expr (..),
  Fixity (Fixity),
  Id,
  Module (Module),
  ParsedModule (ParsedModule),
  Pass (Parsed, Transformed),
  Type (..),
  binder,
 )
import Fine.Syntax.Utils (isFunction)
import Fine.Transform.Common (Fixities)
import Fine.Transform.Expr (runExprTransformer)
import Fine.Transform.Type (transformType)
import Fine.Transform.Vars qualified as Vars

data Env = Env
  { allExprBinders :: Set Id, -- all binders to make available for functions
    currentExprBinders :: Set Id,
    usedExprBinders :: Set Id,
    allTypeBinders :: Set Id, -- all binders to make available for type functions
    currentTypeBinders :: Set Id,
    usedTypeBinders :: Set Id,
    fixities :: Fixities
  }

initEnv :: [Defn] -> SEC Env Error Warning ()
initEnv [] = return ()
initEnv (defn : defns) = do
  case defn of
    FixDefn _ _ -> return ()
    Defn bind ->
      modify (\st -> st{allExprBinders = Set.insert (binder bind) (allExprBinders st)})
    TypeDefn (TypeBind binder' _) ->
      modify (\st -> st{allTypeBinders = Set.insert binder' (allTypeBinders st)})
    DataDefn (TypeBind binder' _) ctBinds -> do
      let ctors = NonEmpty.map binder ctBinds
      modify
        ( \st ->
            st
              { allTypeBinders = Set.insert binder' (allTypeBinders st),
                allExprBinders = foldr Set.insert (allExprBinders st) ctors
              }
        )
  initEnv defns

-- TYPE

handleTypeVars :: Maybe Id -> Type Transformed -> SEC Env Error Warning ()
handleTypeVars optBinder type' = do
  let isTFun = case type' of
        TFun _ _ _ -> True
        _ -> False
  tVars <- gets (if isTFun then allTypeBinders else currentTypeBinders)
  let (usedTVars, errs, wrns) = Vars.handleTypeVars tVars type'
  forM_ errs fail'
  forM_ wrns warn
  unless isTFun $ forM_ optBinder $ \binder' ->
    when (Set.member binder' usedTVars) (fail' $ UsageBeforeInit binder')
  modify (\st -> st{usedTypeBinders = Set.union usedTVars (usedTypeBinders st)})

transformTypeBind :: Bind OfType Parsed -> SEC Env Error Warning (Bind OfType Transformed)
transformTypeBind (TypeBind binder' type') = do
  do
    current <- gets currentTypeBinders
    if Set.member binder' current
      then fail' (AlreadyInScope binder')
      else modify (\st -> st{currentTypeBinders = Set.insert binder' current})
  let type'' = transformType type'
  handleTypeVars (Just binder') type''
  return (TypeBind binder' type'')

-- EXPR

transformExpr :: Expr Parsed -> SEC Env Error Warning (Expr Transformed)
transformExpr expr = do
  fixs <- gets fixities
  let (expr', errs, wrns) = runExprTransformer fixs expr
  forM_ errs fail'
  forM_ wrns warn
  return expr'

handleExprVars :: Maybe Id -> (Expr Transformed) -> SEC Env Error Warning ()
handleExprVars optBinder expr = do
  let isFun = isFunction expr
  vars <- gets (if isFun then allExprBinders else currentExprBinders)
  tVars <- gets allTypeBinders
  let (usedVars, usedTVars, errs, wrns) = Vars.handleExprVars vars tVars expr
  forM_ errs fail'
  forM_ wrns warn
  unless isFun $ forM_ optBinder $ \binder' ->
    when (Set.member binder' usedVars) (fail' $ UsageBeforeInit binder')
  modify
    ( \st ->
        st
          { usedExprBinders = Set.union usedVars (usedExprBinders st),
            usedTypeBinders = Set.union usedTVars (usedTypeBinders st)
          }
    )

transformEntryExpr :: Expr Parsed -> SEC Env Error Warning (Expr Transformed)
transformEntryExpr expr = do
  expr' <- transformExpr expr
  handleExprVars Nothing expr'
  return expr'

transformExprBind :: Bind OfExpr Parsed -> SEC Env Error Warning (Bind OfExpr Transformed)
transformExprBind bind = do
  do
    let binder' = binder bind
    current <- gets currentExprBinders
    if Set.member binder' current
      then fail' (AlreadyInScope binder')
      else modify (\st -> st{currentExprBinders = Set.insert binder' current})
  case bind of
    ExprBind binder' type' expr -> do
      let type'' = transformType type'
      handleTypeVars Nothing type''
      expr' <- transformExpr expr
      handleExprVars (Just binder') expr'
      return (ExprBind binder' type'' expr')
    ForeignBind binder' type' code -> do
      let type'' = transformType type'
      handleTypeVars Nothing type''
      return (ForeignBind binder' type'' code)

-- MODULE

transformDefn :: Defn -> SEC Env Error Warning [Either (Bind OfExpr Transformed) (Bind OfType Transformed)]
transformDefn (FixDefn fix@(Fixity _ prec) op) = do
  unless (0 <= prec && prec < 10) (fail' $ InvalidPrecedence 0 10 op) -- TODO: read from some config
  fixities' <- gets fixities
  if Map.member op fixities'
    then fail' (RepeatedFixity op)
    else modify (\ctx -> ctx{fixities = Map.insert op fix fixities'})
  return []
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

warnUnusedTopBinds :: SEC Env e Warning ()
warnUnusedTopBinds = do
  do
    all' <- gets allExprBinders
    used <- gets usedExprBinders
    forM_ (Set.difference all' used) (warn . UnusedVar)
  do
    all' <- gets allTypeBinders
    used <- gets usedTypeBinders
    forM_ (Set.difference all' used) (warn . UnusedVar)

transformModule :: ParsedModule -> SEC Env Error Warning (Module Transformed)
transformModule (ParsedModule defns entry) = do
  initEnv defns
  (exprBinds, typeBinds) <- partitionEithers . concat <$> mapM transformDefn defns
  entry' <- mapM transformEntryExpr entry
  warnUnusedTopBinds
  fixities' <- gets fixities
  return (Module exprBinds typeBinds fixities' entry')

runTransformer :: ParsedModule -> (Either (NonEmpty Error) (Module Transformed), [Warning])
runTransformer mdule =
  let env =
        Env Set.empty Set.empty Set.empty Set.empty Set.empty Set.empty Map.empty
   in runSEC (transformModule mdule) env
