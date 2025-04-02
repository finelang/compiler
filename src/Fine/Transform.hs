module Fine.Transform (runTransformer) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.SW (SW, gets, modify, runSW, tell)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Fine.Error
  ( Error (..),
    Errors (Errors),
    Warning (UnusedVar),
    collectError,
    collectWarning,
  )
import Fine.Syntax
  ( Bind (..),
    Defn (..),
    Expr (..),
    Fixity (Fixity),
    Id,
    LitT (UnitT),
    Module (Module),
    ParsedModule (ParsedModule),
    Pass (Parsed, Transformed),
    Range (NoRange),
    Type (..),
    TypeOfBind (..),
    binder,
  )
import Fine.Transform.Common (Constructors, Fixities)
import Fine.Transform.Terms (runExprTransformer, transformType)
import qualified Fine.Transform.Vars as Vars

data Env = Env
  { allValueBinders :: Set Id, -- all binders to make available for functions
    currentValueBinders :: Set Id,
    usedValueBinders :: Set Id,
    allTypeBinders :: Set Id, -- all binders to make available for type functions
    currentTypeBinders :: Set Id,
    usedTypeBinders :: Set Id,
    typings :: Map Id (Type Parsed),
    fixities :: Fixities,
    constructors :: Constructors
  }

initEnv :: [Defn] -> SW Env Errors ()
initEnv [] = return ()
initEnv (defn : defns) = do
  case defn of
    FixDefn _ _ -> return ()
    Defn binder' _ -> modify (\st -> st {allValueBinders = Set.insert binder' (allValueBinders st)})
    TypingDefn binder' type' -> do
      typings' <- gets typings
      if Map.member binder' typings'
        then tell (collectError $ RepeatedTyping binder')
        else modify (\st -> st {typings = Map.insert binder' type' typings'})
    TypeDefn (Bind binder' _ _) -> modify (\st -> st {allTypeBinders = Set.insert binder' (allTypeBinders st)})
    DataDefn (Bind binder' _ _) ctBinds -> do
      let ctors = NonEmpty.map binder ctBinds
      modify
        ( \st ->
            st
              { allTypeBinders = Set.insert binder' (allTypeBinders st),
                allValueBinders = foldr Set.insert (allValueBinders st) ctors
              }
        )
  initEnv defns

type ParsedExpr = Expr Parsed

type TransformedExpr = Expr Transformed

type ParsedType = Type Parsed

type TransformedType = Type Transformed

transformExpr :: ParsedExpr -> SW Env Errors TransformedExpr
transformExpr expr = do
  fixs <- gets fixities
  cts <- gets constructors
  let (expr', errors) = runExprTransformer fixs cts expr
  tell errors
  return expr'

transformEntryExpr :: ParsedExpr -> SW Env Errors TransformedExpr
transformEntryExpr expr = do
  expr' <- transformExpr expr
  handleExprVars Nothing expr'
  return expr'

handleExprVars :: Maybe Id -> TransformedExpr -> SW Env Errors ()
handleExprVars bound expr = do
  let isFun = case expr of
        Fun _ _ _ -> True
        _ -> False
  available <- gets (if isFun then allValueBinders else currentValueBinders)
  let (exprEnv, errors) = Vars.handleExprVars available expr
  tell errors
  unless isFun $
    forM_
      bound
      (\binder' -> when (Set.member binder' exprEnv) (tell $ collectError $ UsageBeforeInit binder'))
  modify (\st -> st {usedValueBinders = Set.union exprEnv (usedValueBinders st)})

handleTypeVars :: Maybe Id -> TransformedType -> SW Env Errors ()
handleTypeVars bound type' = do
  let isTFun = case type' of
        TFun _ _ _ -> True
        _ -> False
  available <- gets (if isTFun then allTypeBinders else currentTypeBinders)
  let (typeEnv, errors) = Vars.handleTypeVars available type'
  tell errors
  unless isTFun $
    forM_
      bound
      (\binder' -> when (Set.member binder' typeEnv) (tell $ collectError $ UsageBeforeInit binder'))
  modify (\st -> st {usedTypeBinders = Set.union typeEnv (usedTypeBinders st)})

transformValueBind :: Bind OfValue Parsed -> SW Env Errors (Bind OfValue Transformed)
transformValueBind (Bind binder' type' value) = do
  do
    current <- gets currentValueBinders
    if Set.member binder' current
      then tell (collectError $ AlreadyInScope binder')
      else modify (\st -> st {currentValueBinders = Set.insert binder' current})
  value' <- transformExpr value
  handleExprVars (Just binder') value'
  let type'' = transformType type'
  handleTypeVars Nothing type''
  return (Bind binder' type'' value')

transformTypeBind :: Bind OfType Parsed -> SW Env Errors (Bind OfType Transformed)
transformTypeBind (Bind binder' kind type') = do
  do
    current <- gets currentTypeBinders
    if Set.member binder' current
      then tell (collectError $ AlreadyInScope binder')
      else modify (\st -> st {currentTypeBinders = Set.insert binder' current})
  let type'' = transformType type'
  handleTypeVars (Just binder') type''
  return (Bind binder' kind type'')

data AnyBind
  = VBind (Bind OfValue Transformed)
  | TBind (Bind OfType Transformed)

justVBind :: AnyBind -> Maybe (Bind OfValue Transformed)
justVBind (VBind bind) = Just bind
justVBind _ = Nothing

justTBind :: AnyBind -> Maybe (Bind OfType Transformed)
justTBind (TBind bind) = Just bind
justTBind _ = Nothing

errorType :: ParsedType
errorType = LiteralT NoRange UnitT

transformDefn :: Defn -> SW Env Errors [AnyBind]
transformDefn (FixDefn fix@(Fixity _ prec) op) = do
  unless (0 <= prec && prec < 10) (tell $ collectError $ InvalidPrecedence 0 10 op) -- TODO: read from some config
  fixities' <- gets fixities
  if Map.member op fixities'
    then tell (collectError $ RepeatedFixity op)
    else modify (\ctx -> ctx {fixities = Map.insert op fix fixities'})
  return []
transformDefn (Defn binder' value) = do
  type' <- do
    optT <- gets (Map.lookup binder' . typings)
    case optT of
      Just t -> return t
      Nothing -> tell (collectError $ MissingTyping binder') >> return errorType
  bind <- transformValueBind (Bind binder' type' value)
  return [VBind bind]
transformDefn (TypingDefn _ _) = return []
transformDefn (TypeDefn bind) = do
  bind' <- transformTypeBind bind
  return [TBind bind']
transformDefn (DataDefn bind ctBinds) = do
  do
    let ctBinders' = NonEmpty.map binder ctBinds
    modify (\st -> st {constructors = foldr Set.insert (constructors st) ctBinders'})
  ctBinds' <- (mapM transformValueBind ctBinds)
  bind' <- transformTypeBind bind
  return (TBind bind' : (map VBind . NonEmpty.toList) ctBinds')

checkUnusedTopBinds :: SW Env Errors ()
checkUnusedTopBinds = do
  do
    all' <- gets allValueBinders
    used <- gets usedValueBinders
    forM_ (Set.difference all' used) (tell . collectWarning . UnusedVar)
  do
    all' <- gets allTypeBinders
    used <- gets usedTypeBinders
    forM_ (Set.difference all' used) (tell . collectWarning . UnusedVar)

transformModule :: ParsedModule -> SW Env Errors (Module Transformed)
transformModule (ParsedModule defns entry) = do
  initEnv defns
  bindings <- concat <$> mapM transformDefn defns
  entry' <- mapM transformEntryExpr entry
  checkUnusedTopBinds
  let valueBinds = mapMaybe justVBind bindings
  let typeBinds = mapMaybe justTBind bindings
  fixities' <- gets fixities
  return (Module valueBinds typeBinds fixities' entry')

runTransformer :: ParsedModule -> (Either [Error] (Module Transformed), [Warning])
runTransformer mdule =
  let env =
        Env Set.empty Set.empty Set.empty Set.empty Set.empty Set.empty Map.empty Map.empty Set.empty
      (mdule', _, Errors errors warnings) = runSW (transformModule mdule) env
   in (if null errors then Right mdule' else Left errors, warnings)
