module Fine.Typer.Kinder.J (runKindChecker, runKindInferrer) where

import Control.Monad (forM, forM_)
import Control.Monad.Errors (Errors)
import Control.Monad.Errors qualified as Errors
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, ReaderT, asks, local, runReader, runReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as State
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Fine.Error (Error (BadKindSub, CannotUnifyKinds))
import Fine.Syntax (
  Id (Id),
  Kind (..),
  Phase (Kinded, PartiallyKinded, Transformed),
  Range (NoRange),
  Type (..),
  range,
  typeof,
 )
import Fine.Typer.Common (Env, fromEnv)
import Unsafe.Coerce (unsafeCoerce)

type RSE r s e a = ReaderT r (StateT s (Errors e)) a

gets :: (s -> a) -> RSE r s e a
gets = lift . State.gets

modify :: (s -> s) -> RSE r s e ()
modify = lift . State.modify

failure :: e -> RSE r s e a
failure = lift . lift . Errors.failure

runRS :: RSE r s e a -> r -> s -> Errors e a
runRS rse r s = State.evalStateT (runReaderT rse r) s

type Sub = Env (Kind PartiallyKinded)

compose :: Sub -> Sub -> Sub
compose s s' = Map.union s (apply s <$> s')

apply :: Sub -> Kind PartiallyKinded -> Kind PartiallyKinded
apply _ k@(KLit _) = k
apply s (TFunK r ks k) = TFunK r (NonEmpty.map (apply s) ks) (apply s k)
apply s k@(SubKVar var) = Map.findWithDefault k var s

subVars :: Kind PartiallyKinded -> Set Id
subVars (KLit _) = Set.empty
subVars (TFunK _ ks k) = Set.unions $ NonEmpty.map subVars (k <| ks)
subVars (SubKVar var) = Set.singleton var

type KindedSub = Env (Kind Kinded)

resolveUndecidable :: Sub -> KindedSub
resolveUndecidable s =
  let (s', undecidable) = runWriter (mapM go s)
   in Map.union s' undecidable
 where
  go :: Kind PartiallyKinded -> Writer KindedSub (Kind Kinded)
  go (KLit r) = return $ KLit r
  go (TFunK r kinds kind) = TFunK r <$> mapM go kinds <*> go kind
  go (SubKVar var) = do
    let r = range var
    tell (Map.singleton var $ KLit r)
    return $ KLit r

data SubCtx = SubCtx {subCount :: Int, sub :: Sub}

initialSubCtx :: SubCtx
initialSubCtx = SubCtx 1 Map.empty

unify :: Kind PartiallyKinded -> Kind PartiallyKinded -> RSE r SubCtx Error ()
unify lk' rk' = do
  s <- gets sub
  unify' (apply s lk') (apply s rk')
 where
  unify' (KLit _) (KLit _) = return ()
  unify' (TFunK _ lks lk) (TFunK _ rks rk) | length lks == length rks = do
    forM_ (NonEmpty.zip lks rks) (uncurry unify)
    unify lk rk
  unify' lk@(TFunK _ _ _) rk@(TFunK _ _ _) = failure (CannotUnifyKinds lk rk)
  unify' (SubKVar var) k = unifyVar var k
  unify' k (SubKVar var) = unifyVar var k
  unify' lk rk = failure (CannotUnifyKinds lk rk)

  unifyVar var (SubKVar var') | var == var' = return ()
  unifyVar var k | Set.member var (subVars k) = failure (BadKindSub var k)
  unifyVar var k = do
    s <- gets sub
    modify $ \ctx -> ctx{sub = compose (Map.singleton var k) s}

unifyType :: Type PartiallyKinded -> Kind PartiallyKinded -> RSE r SubCtx Error ()
unifyType = unify . typeof

newSubVar :: Range -> RSE r SubCtx e (Kind PartiallyKinded)
newSubVar r = do
  n <- gets subCount
  modify $ \ctx -> ctx{subCount = n + 1}
  return $ SubKVar $ Id r [i|k#{n}|]

type KindEnv = Env (Kind PartiallyKinded)

klit :: Kind PartiallyKinded
klit = KLit NoRange

infer :: Type Transformed -> RSE KindEnv SubCtx Error (Type PartiallyKinded)
infer (LiteralT r litT) = return $ LiteralT (r, KLit r) litT
infer (VoidT r) = return $ VoidT (r, KLit r)
infer (TupleT r fst' snd' rest) = do
  fst'' <- infer fst'
  unifyType fst'' klit
  snd'' <- infer snd'
  unifyType snd'' klit
  rest' <- mapM infer rest
  forM_ rest' $ \t -> unifyType t klit
  return $ TupleT (r, KLit r) fst'' snd'' rest'
infer (ListT r type') = do
  type'' <- infer type'
  unifyType type'' klit
  return $ ListT (r, KLit r) type''
infer (RecordT r propTypes) = do
  propTypes' <- forM propTypes $ \(prop, type') -> do
    type'' <- infer type'
    unifyType type'' klit
    return (prop, type'')
  return $ RecordT (r, KLit r) propTypes'
infer (FunT r argTypes retType) = do
  argTypes' <- forM argTypes $ \type' -> do
    type'' <- infer type'
    unifyType type'' klit
    return type''
  retType' <- infer retType
  unifyType retType' klit
  return $ FunT (r, KLit r) argTypes' retType'
infer (Forall r vars type') = do
  varKinds <- mapM (newSubVar . range) vars
  let extraCtx = Map.fromList $ NonEmpty.toList $ NonEmpty.zip vars varKinds
  type'' <- local (Map.union extraCtx) (infer type')
  unifyType type'' klit
  return $ Forall (r, KLit r) (NonEmpty.zip vars varKinds) type''
infer (DataT r tag types) = do
  types' <- forM types $ \type' -> do
    type'' <- infer type'
    unifyType type'' klit
    return type''
  return $ DataT (r, KLit r) tag types'
infer (TVar r var) = do
  kind <- asks (fromEnv var)
  return $ TVar (r, kind) var
infer (TApp r tfun targs) = do
  tfun' <- infer tfun
  targs' <- mapM infer targs
  kind <- newSubVar r
  unifyType tfun' (TFunK NoRange (NonEmpty.map typeof targs') kind)
  return $ TApp (r, kind) tfun' targs'
infer (TFun r tparams tbody) = do
  tparamKinds <- mapM (newSubVar . range) tparams
  let extraCtx = Map.fromList $ NonEmpty.toList $ NonEmpty.zip tparams tparamKinds
  tbody' <- local (Map.union extraCtx) (infer tbody)
  let kind = TFunK r tparamKinds (typeof tbody')
  return $ TFun (r, kind) tparams tbody'

complete :: Type PartiallyKinded -> Reader KindedSub (Type Kinded)
complete t = case t of
  LiteralT ext litType -> LiteralT <$> go ext <*> return litType
  VoidT ext -> VoidT <$> go ext
  TupleT ext fst' snd' rest ->
    TupleT <$> go ext <*> complete fst' <*> complete snd' <*> mapM complete rest
  ListT ext type' -> ListT <$> go ext <*> complete type'
  RecordT ext propTypes -> RecordT <$> go ext <*> (mapM . mapM) complete propTypes
  FunT ext argTypes retType -> FunT <$> go ext <*> mapM complete argTypes <*> complete retType
  Forall ext univars type' -> Forall <$> go ext <*> (mapM . mapM) go' univars <*> complete type'
  DataT ext tag types -> DataT <$> go ext <*> return tag <*> mapM complete types
  TVar ext var -> TVar <$> go ext <*> return var
  TApp ext tfun targs -> TApp <$> go ext <*> complete tfun <*> mapM complete targs
  TFun ext tparams tbody -> TFun <$> go ext <*> return tparams <*> complete tbody
 where
  go (r, k) = (,) r <$> go' k

  go' :: Kind PartiallyKinded -> Reader KindedSub (Kind Kinded)
  go' (KLit r) = return (KLit r)
  go' (TFunK r kinds kind) = TFunK r <$> mapM go' kinds <*> go' kind
  go' (SubKVar var) = asks (fromEnv var)

check :: Type Transformed -> RSE KindEnv SubCtx Error (Type Kinded)
check type' = do
  type'' <- infer type'
  unifyType type'' klit
  ks <- resolveUndecidable <$> gets sub
  return $ runReader (complete type'') ks

runKindChecker :: Env (Kind Kinded) -> Type Transformed -> Errors Error (Type Kinded)
runKindChecker kindEnv type' =
  runRS (check type') (Map.map asPartiallyKinded kindEnv) initialSubCtx
 where
  asPartiallyKinded :: Kind Kinded -> Kind PartiallyKinded
  asPartiallyKinded = unsafeCoerce

inferMany :: Env (Type Transformed) -> RSE KindEnv SubCtx Error (Env (Type Kinded))
inferMany types = do
  kindEnv <- fmap Map.fromList $ forM (Map.keys types) $ \binder -> do
    kind <- newSubVar (range binder)
    return (binder, kind)
  types' <- local (const kindEnv) $ forM (Map.toList types) $ \(binder, type') -> do
    type'' <- infer type'
    unifyType type'' (fromEnv binder kindEnv)
    return (binder, type'')
  ks <- resolveUndecidable <$> gets sub
  return $ Map.map (\t -> runReader (complete t) ks) (Map.fromList types')

runKindInferrer :: Env (Type Transformed) -> Errors Error (Env (Type Kinded))
runKindInferrer types = runRS (inferMany types) Map.empty initialSubCtx
