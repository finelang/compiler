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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Fine.Error (
  Error (
    AlreadyDefined,
    InvalidBinding,
    MissingTyping,
    RepeatedTyping,
    UsageBeforeInit
  ),
  Warning (DebugKeywordUsage, UnusedVar),
 )
import Fine.Syntax (
  Bind (..),
  BindType (..),
  Block (..),
  Defn (..),
  Expr (..),
  Id,
  Module (Module),
  Name,
  ParsedModule (ParsedModule),
  Phase (Parsed, Transformed),
  Type (..),
 )
import Fine.Syntax.Utils (binder, isFunction, isTFunction, unqualified)
import Fine.Transform.Check (runExprVarChecker, runTypeVarChecker)
import Unsafe.Coerce (unsafeCoerce)

type SEC s e c a = StateT s (ErrorsT e (Collector c)) a

failure :: e -> SEC s e c a
failure = lift . Errors.failure

fromEither :: Either (NonEmpty e) a -> SEC s e c a
fromEither = lift . Errors.fromEither

collect :: c -> SEC s e c ()
collect = lift . lift . Collector.collect

data Env = Env
  { allExprBinders :: Set Name,
    currentExprBinders :: Set Name,
    usedExprBinders :: Set Name,
    --
    validNonFunctionFreeVars :: Set Name,
    --
    allTypeBinders :: Set Name,
    currentTypeBinders :: Set Name,
    usedTypeBinders :: Set Name,
    --
    exprTypings :: Map Id (Type Parsed),
    --
    typeCtors :: Set Id
  }

initEnv :: [Defn] -> SEC Env Error c ()
initEnv = mapM_ go
 where
  go (ValueDefn binder' _) = addExprBinder $ unqualified binder'
  go (ForeignDefn binder' _) = do
    let binder'' = unqualified binder'
    addExprBinder binder''
    modify $ \env@Env{validNonFunctionFreeVars} ->
      env{validNonFunctionFreeVars = Set.insert binder'' validNonFunctionFreeVars}
  go (TypingDefn binder' type') = do
    typings <- gets exprTypings
    if Map.member binder' typings
      then failure (RepeatedTyping binder')
      else modify $ \env -> env{exprTypings = Map.insert binder' type' typings}
  go (TypeDefn (TypeBind binder' _)) = addTypeBinder $ unqualified binder'
  go (DataDefn (TypeBind binder' _) ctBinds) = do
    addTypeBinder $ unqualified binder'
    modify $ \env@Env{typeCtors} -> env{typeCtors = Set.insert binder' typeCtors}
    let ctBinders = NonEmpty.map binder ctBinds
    forM_ ctBinders $ \ctBinder -> do
      addExprBinder ctBinder
      modify $ \env@Env{validNonFunctionFreeVars} ->
        env{validNonFunctionFreeVars = Set.insert ctBinder validNonFunctionFreeVars}
  addExprBinder binder' = do
    binders <- gets allExprBinders
    if Set.member binder' binders
      then failure (AlreadyDefined binder')
      else modify $ \env -> env{allExprBinders = Set.insert binder' binders}
  addTypeBinder binder' = do
    binders <- gets allTypeBinders
    if Set.member binder' binders
      then failure (AlreadyDefined binder')
      else modify $ \env -> env{allTypeBinders = Set.insert binder' binders}

-- TYPE

transformType :: Type Parsed -> Type Transformed
transformType = unsafeCoerce

checkType :: Maybe Id -> Type Transformed -> SEC Env Error Warning ()
checkType optBinder type' = do
  forM_ optBinder $ \binder' ->
    modify $ \env@Env{currentTypeBinders} ->
      env{currentTypeBinders = Set.insert (unqualified binder') currentTypeBinders}
  let isTFun = isTFunction type'
  tVars <- gets (if isTFun then allTypeBinders else currentTypeBinders)
  let (result, wrns) = runTypeVarChecker tVars type'
  forM_ wrns collect
  usedTVars <- fromEither result
  unless isTFun $ forM_ optBinder $ \binder' -> do
    let name = unqualified binder'
    when (Set.member name usedTVars) (failure $ UsageBeforeInit name)
  modify $ \env@Env{usedTypeBinders} ->
    env{usedTypeBinders = Set.union usedTVars usedTypeBinders}

transformTypeBind :: Bind OfType Parsed -> SEC Env Error Warning (Bind OfType Transformed)
transformTypeBind (TypeBind binder' type') = do
  let type'' = transformType type'
  checkType (Just binder') type''
  pure (TypeBind binder' type'')

-- EXPR

transformBlock :: Block Parsed -> SEC s e Warning (Block Transformed)
transformBlock (Return expr) = Return <$> transformExpr expr
transformBlock Void = pure Void
transformBlock (Do action block) =
  Do <$> transformExpr action <*> transformBlock block
transformBlock (Mut var expr block) =
  Mut var <$> transformExpr expr <*> transformBlock block
transformBlock (LetMut binder' value block) =
  LetMut binder' <$> transformExpr value <*> transformBlock block
transformBlock (Let _ pattern value block) =
  Let () pattern <$> transformExpr value <*> transformBlock block
transformBlock (Debug r expr block) = do
  collect $ DebugKeywordUsage r
  Debug r <$> transformExpr expr <*> transformBlock block
transformBlock (Loop cond actions block) =
  Loop <$> transformExpr cond <*> transformBlock actions <*> transformBlock block

transformExpr :: Expr Parsed -> SEC s e Warning (Expr Transformed)
transformExpr (Literal ext r lit) = pure (Literal ext r lit)
transformExpr (Data ext tag exprs) = Data ext tag <$> mapM transformExpr exprs
transformExpr (Record ext r props) = Record ext r <$> (mapM . mapM) transformExpr props
transformExpr (Tuple ext r fst' snd' rest) =
  Tuple ext r <$> transformExpr fst' <*> transformExpr snd' <*> mapM transformExpr rest
transformExpr (Var ext var) = pure (Var ext var)
transformExpr (Bin ext op left right) = Bin ext op <$> transformExpr left <*> transformExpr right
transformExpr (App ext f arg) = App ext <$> transformExpr f <*> transformExpr arg
transformExpr (GenApp ext _ fname types) =
  pure $ GenApp ext () fname (NonEmpty.map transformType types)
transformExpr (Access ext expr' prop) = Access ext <$> transformExpr expr' <*> pure prop
transformExpr (Index ext r expr' ix) = Index ext r <$> transformExpr expr' <*> pure ix
transformExpr (Cond ext r cond yes no) =
  Cond ext r <$> transformExpr cond <*> transformExpr yes <*> transformExpr no
transformExpr (Fun ext param' body) = Fun ext param' <$> transformExpr body
transformExpr (Block ext r block) = Block ext r <$> transformBlock block
transformExpr (PatternMatching ext r _ matched matches) =
  PatternMatching ext r () <$> transformExpr matched <*> (mapM . mapM) transformExpr matches

checkBoundExpr :: Name -> Expr Transformed -> SEC Env Error Warning ()
checkBoundExpr binder' expr = do
  let isFun = isFunction expr
  modify $ \env@Env{currentExprBinders} ->
    env{currentExprBinders = Set.insert binder' currentExprBinders}
  unless isFun $ modify $ \env@Env{validNonFunctionFreeVars} ->
    env{validNonFunctionFreeVars = Set.insert binder' validNonFunctionFreeVars}
  vars <- gets (if isFun then allExprBinders else currentExprBinders)
  tVars <- gets allTypeBinders
  let (result, wrns) = runExprVarChecker vars tVars expr
  forM_ wrns collect
  (usedVars, usedTVars) <- fromEither result
  unless isFun $ do
    when (Set.member binder' usedVars) (failure $ UsageBeforeInit binder')
    invalidFreeVars <- gets (Set.difference usedVars . validNonFunctionFreeVars)
    forM_ invalidFreeVars (failure . InvalidBinding binder')
  modify $ \env@Env{usedExprBinders, usedTypeBinders} ->
    env
      { usedExprBinders = Set.union usedVars usedExprBinders,
        usedTypeBinders = Set.union usedTVars usedTypeBinders
      }

checkEntryExpr :: Expr Transformed -> SEC Env Error Warning ()
checkEntryExpr expr = do
  vars <- gets allExprBinders
  tVars <- gets allTypeBinders
  let (result, wrns) = runExprVarChecker vars tVars expr
  forM_ wrns collect
  (usedVars, usedTVars) <- fromEither result
  modify $ \env@Env{usedExprBinders, usedTypeBinders} ->
    env
      { usedExprBinders = Set.union usedVars usedExprBinders,
        usedTypeBinders = Set.union usedTVars usedTypeBinders
      }

transformExprBind :: Bind OfExpr Parsed -> SEC Env Error Warning (Bind OfExpr Transformed)
transformExprBind (ExprBind binder' type' expr) = do
  let type'' = transformType type'
  checkType Nothing type''
  expr' <- transformExpr expr
  let expr'' = case type'' of
        Forall _ _ tparams _ -> GenFun () () () tparams expr'
        _ -> expr'
  checkBoundExpr binder' expr''
  pure (ExprBind binder' type'' expr'')
transformExprBind (ForeignBind binder' type' code) = do
  let type'' = transformType type'
  checkType Nothing type''
  modify $ \env@Env{currentExprBinders} ->
    env{currentExprBinders = Set.insert (unqualified binder') currentExprBinders}
  pure (ForeignBind binder' type'' code)

-- MODULE

transformExprDefn :: Defn -> SEC Env Error Warning [Bind OfExpr Transformed]
transformExprDefn (ValueDefn binder' value) = do
  optType <- gets (Map.lookup binder' . exprTypings)
  case optType of
    Just type' -> singleton <$> transformExprBind (ExprBind (unqualified binder') type' value)
    Nothing -> failure (MissingTyping binder')
transformExprDefn (ForeignDefn binder' code) = do
  optType <- gets (Map.lookup binder' . exprTypings)
  case optType of
    Just type' -> singleton <$> transformExprBind (ForeignBind binder' type' code)
    Nothing -> failure (MissingTyping binder')
transformExprDefn (TypeDefn _) = pure []
transformExprDefn (DataDefn _ binds) = NonEmpty.toList <$> mapM transformExprBind binds
transformExprDefn (TypingDefn _ _) = pure []

transformTypeDefn :: Defn -> SEC Env Error Warning (Maybe (Bind OfType Transformed))
transformTypeDefn (ValueDefn _ _) = pure Nothing
transformTypeDefn (ForeignDefn _ _) = pure Nothing
transformTypeDefn (TypeDefn bind) = Just <$> transformTypeBind bind
transformTypeDefn (DataDefn bind _) = Just <$> transformTypeBind bind
transformTypeDefn (TypingDefn _ _) = pure Nothing

warnUnusedBinders :: SEC Env e Warning ()
warnUnusedBinders = do
  do
    all' <- gets allExprBinders
    used <- gets usedExprBinders
    forM_ (Set.toList $ all' \\ used) (collect . UnusedVar)
  do
    all' <- gets allTypeBinders
    used <- gets usedTypeBinders
    forM_ (Set.toList $ all' \\ used) (collect . UnusedVar)

transformModule :: ParsedModule -> SEC Env Error Warning (Module Transformed)
transformModule (ParsedModule defns entry) = do
  initEnv defns
  typeBinds <- catMaybes <$> mapM transformTypeDefn defns
  exprBinds <- concat <$> mapM transformExprDefn defns
  entry' <- forM entry $ \expr -> do
    expr' <- transformExpr expr
    checkEntryExpr expr'
    pure expr'
  typeCtors' <- gets typeCtors
  warnUnusedBinders
  pure (Module exprBinds typeBinds entry' typeCtors')

runTransformer :: ParsedModule -> (Either (NonEmpty Error) (Module Transformed), [Warning])
runTransformer mdule =
  let env =
        Env
          Set.empty
          Set.empty
          Set.empty
          Set.empty
          Set.empty
          Set.empty
          Set.empty
          Map.empty
          Set.empty
   in Collector.runCollector $ Errors.runErrorsT $ evalStateT (transformModule mdule) env
