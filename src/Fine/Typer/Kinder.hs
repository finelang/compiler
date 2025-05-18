module Fine.Typer.Kinder (runKindChecker, runKindInferrer) where

import Control.Monad (forM, forM_)
import Control.Monad.RWS.Strict (RWS, asks, gets, local, modify, runRWS, tell)
import Control.Monad.Reader (Reader)
import Control.Monad.Reader qualified as Reader
import Control.Monad.Writer.Strict (Writer)
import Control.Monad.Writer.Strict qualified as Writer
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Fine.Error (Error (BadKindSubstt, CannotUnifyKinds), errorUNREACHABLE)
import Fine.Syntax (
  Id,
  Kind (..),
  Phase (PartiallyTyped, Transformed, Typed),
  Range (NoRange),
  Type (..),
  range,
  typeof,
 )
import Fine.Typer.Common (
  Env,
  SubsttState (..),
  Substts,
  fromEnv,
  initSubsttState,
  newSubsttVar,
  substtVars,
  (#),
  (#.),
 )
import Unsafe.Coerce (unsafeCoerce)

type Substts' = Substts (Kind PartiallyTyped)

type SubsttState' = SubsttState (Kind PartiallyTyped)

type KindEnv = Env (Kind PartiallyTyped)

unify :: Kind PartiallyTyped -> Kind PartiallyTyped -> RWS r [Error] SubsttState' ()
unify kind kind' = do
  s <- gets substts
  unify' (s # kind) (s # kind')

unify' :: Kind PartiallyTyped -> Kind PartiallyTyped -> RWS r [Error] SubsttState' ()
unify' (KLit _) (KLit _) = return ()
unify' k1@(TFunK _ kinds kind) k2@(TFunK _ kinds' kind') =
  if length kinds == length kinds'
    then do
      forM_ (NonEmpty.zip kinds kinds') (uncurry unify)
      unify kind kind'
    else tell [CannotUnifyKinds k1 k2]
unify' (SubsttKVar _ var) kind = unifyVar var kind
unify' kind (SubsttKVar _ var) = unifyVar var kind
unify' kind kind' = tell [CannotUnifyKinds kind kind']

unifyVar :: Id -> Kind PartiallyTyped -> RWS r [Error] SubsttState' ()
unifyVar var (SubsttKVar _ var') | var == var' = return ()
unifyVar var kind
  | Set.member var (substtVars kind) =
      tell [BadKindSubstt var kind]
unifyVar var kind = do
  s <- gets substts
  modify $ \env -> env{substts = (Map.singleton var kind) #. s}

klit :: Kind PartiallyTyped
klit = KLit NoRange

newSubsttKVar :: (Monoid w) => Range -> RWS r w SubsttState' (Kind PartiallyTyped)
newSubsttKVar r = SubsttKVar r <$> newSubsttVar "k" r

infer :: Type Transformed -> RWS KindEnv [Error] SubsttState' (Type PartiallyTyped)
infer (LiteralT r litT) = return $ LiteralT (r, KLit r) litT
infer (VoidT r) = return $ VoidT (r, KLit r)
infer (TupleT r fst' snd' rest) = do
  fst'' <- infer fst'
  unify (typeof fst'') klit
  snd'' <- infer snd'
  unify (typeof snd'') klit
  rest' <- mapM infer rest
  forM_ rest' $ \t -> unify (typeof t) klit
  TupleT (r, KLit r) <$> infer fst' <*> infer snd' <*> mapM infer rest
infer (ListT r type') = do
  type'' <- infer type'
  unify (typeof type'') klit
  return $ ListT (r, KLit r) type''
infer (RecordT r propTypes) = do
  propTypes' <- forM propTypes $ \(prop, type') -> do
    type'' <- infer type'
    unify (typeof type'') klit
    return (prop, type'')
  return $ RecordT (r, KLit r) propTypes'
infer (FunT r argTypes retType) = do
  argTypes' <- forM argTypes $ \type' -> do
    type'' <- infer type'
    unify (typeof type'') klit
    return type''
  retType' <- infer retType
  unify (typeof retType') klit
  return $ FunT (r, KLit r) argTypes' retType'
infer (Forall r vars type') = do
  varKinds <- mapM (newSubsttKVar . range) vars
  let extraCtx = Map.fromList $ NonEmpty.toList $ NonEmpty.zip vars varKinds
  type'' <- local (Map.union extraCtx) (infer type')
  unify (typeof type'') klit
  return $ Forall (r, KLit r) vars type''
infer (TData r tag types) = do
  types' <- forM types $ \type' -> do
    type'' <- infer type'
    unify (typeof type'') klit
    return type''
  return $ TData (r, KLit r) tag types'
infer (TVar r var) = do
  kind <- asks (fromEnv var)
  return $ TVar (r, kind) var
infer (TApp r tfun targs) = do
  tfun' <- infer tfun
  targs' <- mapM infer targs
  kind <- newSubsttKVar r
  unify (typeof tfun') (TFunK NoRange (NonEmpty.map typeof targs') kind)
  return $ TApp (r, kind) tfun' targs'
infer (TFun r tparams tbody) = do
  tparamKinds <- mapM (newSubsttKVar . range) tparams
  let extraCtx = Map.fromList $ NonEmpty.toList $ NonEmpty.zip tparams tparamKinds
  tbody' <- local (Map.union extraCtx) (infer tbody)
  let kind = TFunK r tparamKinds (typeof tbody')
  return $ TFun (r, kind) tparams tbody'

type TypedSubstts = Substts (Kind Typed)

resolveUndecidable :: Substts' -> TypedSubstts
resolveUndecidable s =
  let (s', undecidable) = Writer.runWriter (mapM go s)
   in Map.union s' undecidable
 where
  go :: Kind PartiallyTyped -> Writer TypedSubstts (Kind Typed)
  go (KLit r) = return $ KLit r
  go (TFunK r kinds kind) = TFunK r <$> mapM go kinds <*> go kind
  go (SubsttKVar r var) = do
    Writer.tell (Map.singleton var $ KLit r)
    return $ KLit r

complete :: Type PartiallyTyped -> Reader TypedSubstts (Type Typed)
complete t = case t of
  LiteralT ext litType -> LiteralT <$> go ext <*> return litType
  VoidT ext -> VoidT <$> go ext
  TupleT ext fst' snd' rest ->
    TupleT <$> go ext <*> complete fst' <*> complete snd' <*> mapM complete rest
  ListT ext type' -> ListT <$> go ext <*> complete type'
  RecordT ext propTypes -> RecordT <$> go ext <*> (mapM . mapM) complete propTypes
  FunT ext argTypes retType -> FunT <$> go ext <*> mapM complete argTypes <*> complete retType
  Forall ext univars type' -> Forall <$> go ext <*> return univars <*> complete type'
  TData ext tag types -> TData <$> go ext <*> return tag <*> mapM complete types
  TVar ext var -> TVar <$> go ext <*> return var
  TApp ext tfun targs -> TApp <$> go ext <*> complete tfun <*> mapM complete targs
  TFun ext tparams tbody -> TFun <$> go ext <*> return tparams <*> complete tbody
  SubsttTVar _ _ _ -> errorUNREACHABLE "Kinder should run before any substitution type variable is created."
 where
  go (r, k) = (,) r <$> go' k

  go' :: Kind PartiallyTyped -> Reader TypedSubstts (Kind Typed)
  go' (KLit r) = return (KLit r)
  go' (TFunK r kinds kind) = TFunK r <$> mapM go' kinds <*> go' kind
  go' (SubsttKVar _ var) = do
    optKind <- Reader.asks (Map.lookup var)
    return $ case optKind of
      Just kind -> kind
      -- if there is no substitution for 'var' is because of an unification error
      Nothing -> KLit NoRange

check :: Type Transformed -> RWS KindEnv [Error] SubsttState' (Type Typed)
check type' = do
  type'' <- infer type'
  unify (typeof type'') (KLit NoRange)
  ts <- resolveUndecidable <$> gets substts
  return $ Reader.runReader (complete type'') ts

runKindChecker :: Map Id (Type Typed) -> Type Transformed -> (Type Typed, [Error])
runKindChecker typeEnv type' =
  let kindEnv = Map.map (asPartiallyTyped . typeof) typeEnv
      (type'', _, errs) = runRWS (check type') kindEnv initSubsttState
   in (type'', errs)
 where
  asPartiallyTyped :: Kind Typed -> Kind PartiallyTyped
  asPartiallyTyped = unsafeCoerce

inferMany :: Map Id (Type Transformed) -> RWS KindEnv [Error] SubsttState' (Map Id (Type Typed))
inferMany types = do
  kindEnv <- fmap Map.fromList $ forM (Map.keys types) $ \binder -> do
    kind <- newSubsttKVar (range binder)
    return (binder, kind)
  types' <- local (const kindEnv) $ forM (Map.toList types) $ \(binder, type') -> do
    type'' <- infer type'
    unify (typeof type'') (fromEnv binder kindEnv)
    return (binder, type'')
  ts <- resolveUndecidable <$> gets substts
  return $ Map.map (\t -> Reader.runReader (complete t) ts) (Map.fromList types')

runKindInferrer :: Map Id (Type Transformed) -> (Map Id (Type Typed), [Error])
runKindInferrer types =
  let (types', _, errs) = runRWS (inferMany types) Map.empty initSubsttState
   in (types', errs)
