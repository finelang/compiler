module Fine.Typer.Kinder.J (runKindChecker, runKindInferrer) where

import Control.Monad (forM, forM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Errors (Errors)
import Control.Monad.Trans.Errors qualified as Errors
import Control.Monad.Trans.Reader (Reader, ReaderT, asks, local, runReader, runReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as State
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Map.Strict.Extra (find)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Fine.Error (Error (BadKindSub, CannotUnifyKinds))
import Fine.Syntax (
  Id (Id),
  Kind (..),
  Name,
  Phase (Kinded, PartiallyKinded, Transformed),
  Range (NoRange),
  Type (..),
  range,
  typeof,
 )
import Fine.Syntax.Utils (unqualified)
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

type Sub = Map Id (Kind PartiallyKinded)

compose :: Sub -> Sub -> Sub
compose s s' = Map.union s (apply s <$> s')

apply :: Sub -> Kind PartiallyKinded -> Kind PartiallyKinded
apply _ k@(KLit _) = k
apply s (TFunK ak bk) = TFunK (apply s ak) (apply s bk)
apply s k@(SubKVar var) = Map.findWithDefault k var s

subVars :: Kind PartiallyKinded -> Set Id
subVars (KLit _) = Set.empty
subVars (TFunK ak bk) = Set.union (subVars ak) (subVars bk)
subVars (SubKVar var) = Set.singleton var

type KindedSub = Map Id (Kind Kinded)

resolveUndecidable :: Sub -> KindedSub
resolveUndecidable s =
  let (s', undecidable) = runWriter (mapM go s)
   in Map.union s' undecidable
 where
  go :: Kind PartiallyKinded -> Writer KindedSub (Kind Kinded)
  go (KLit r) = pure $ KLit r
  go (TFunK ak bk) = TFunK <$> go ak <*> go bk
  go (SubKVar var) = do
    let r = range var
    tell (Map.singleton var $ KLit r)
    pure $ KLit r

data SubCtx = SubCtx {subCount :: Int, sub :: Sub}

initialSubCtx :: SubCtx
initialSubCtx = SubCtx 1 Map.empty

unify :: Kind PartiallyKinded -> Kind PartiallyKinded -> RSE r SubCtx Error ()
unify lk' rk' = do
  s <- gets sub
  unify' (apply s lk') (apply s rk')
 where
  unify' (KLit _) (KLit _) = pure ()
  unify' (TFunK lak lbk) (TFunK rak rbk) = do
    unify lak rak
    unify lbk rbk
  unify' (SubKVar var) k = unifyVar var k
  unify' k (SubKVar var) = unifyVar var k
  unify' lk rk = failure (CannotUnifyKinds lk rk)

  unifyVar var (SubKVar var') | var == var' = pure ()
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
  pure $ SubKVar $ Id r [i|k#{n}|]

type KindEnv = Map Name (Kind PartiallyKinded)

klit :: Kind PartiallyKinded
klit = KLit NoRange

infer :: Type Transformed -> RSE KindEnv SubCtx Error (Type PartiallyKinded)
infer (LiteralT _ r litT) = pure $ LiteralT (KLit r) r litT
infer (VoidT _ r) = pure $ VoidT (KLit r) r
infer (TupleT _ r fst' snd' rest) = do
  fst'' <- infer fst'
  unifyType fst'' klit
  snd'' <- infer snd'
  unifyType snd'' klit
  rest' <- mapM infer rest
  forM_ rest' $ \t -> unifyType t klit
  pure $ TupleT (KLit r) r fst'' snd'' rest'
infer (RecordT _ r propTypes) = do
  propTypes' <- forM propTypes $ \(prop, type') -> do
    type'' <- infer type'
    unifyType type'' klit
    pure (prop, type'')
  pure $ RecordT (KLit r) r propTypes'
infer t@(FunT _ at bt) = do
  at' <- infer at
  unifyType at' klit
  bt' <- infer bt
  unifyType bt' klit
  pure $ FunT (KLit $ range t) at' bt'
infer (Forall _ r vars type') = do
  varKinds <- mapM (newSubVar . range) vars
  let vars' = NonEmpty.map unqualified vars
  let extraCtx = Map.fromList $ NonEmpty.toList $ NonEmpty.zip vars' varKinds
  type'' <- local (Map.union extraCtx) (infer type')
  unifyType type'' klit
  pure $ Forall (KLit r) r (NonEmpty.zip vars varKinds) type''
infer (DataT _ tag types) = do
  types' <- forM types $ \type' -> do
    type'' <- infer type'
    unifyType type'' klit
    pure type''
  pure $ DataT (KLit $ range tag) tag types'
infer (TVar _ var) = do
  kind <- asks (find var)
  pure $ TVar kind var
infer t@(TApp _ tf ta) = do
  tf' <- infer tf
  ta' <- infer ta
  kind <- newSubVar (range t)
  unifyType tf' (TFunK (typeof ta') kind)
  pure $ TApp kind tf' ta'
infer (TFun _ tp tb) = do
  tpKind <- newSubVar (range tp)
  tb' <- local (Map.insert (unqualified tp) tpKind) (infer tb)
  let kind = TFunK (typeof tb') tpKind
  pure $ TFun kind tp tb'

complete :: Type PartiallyKinded -> Reader KindedSub (Type Kinded)
complete = \case
  LiteralT k r litType -> LiteralT <$> go k <*> pure r <*> pure litType
  VoidT k r -> VoidT <$> go k <*> pure r
  TupleT k r fst' snd' rest ->
    TupleT <$> go k <*> pure r <*> complete fst' <*> complete snd' <*> mapM complete rest
  RecordT k r propTypes -> RecordT <$> go k <*> pure r <*> (mapM . mapM) complete propTypes
  FunT k at bt -> FunT <$> go k <*> complete at <*> complete bt
  Forall k r univars type' -> Forall <$> go k <*> pure r <*> (mapM . mapM) go univars <*> complete type'
  DataT k tag types -> DataT <$> go k <*> pure tag <*> mapM complete types
  TVar k var -> TVar <$> go k <*> pure var
  TApp k tf ta -> TApp <$> go k <*> complete tf <*> complete ta
  TFun k tp tb -> TFun <$> go k <*> pure tp <*> complete tb
 where
  go :: Kind PartiallyKinded -> Reader KindedSub (Kind Kinded)
  go (KLit r) = pure (KLit r)
  go (TFunK ak bk) = TFunK <$> go ak <*> go bk
  go (SubKVar var) = asks (find var)

check :: Type Transformed -> RSE KindEnv SubCtx Error (Type Kinded)
check type' = do
  type'' <- infer type'
  unifyType type'' klit
  ks <- resolveUndecidable <$> gets sub
  pure $ runReader (complete type'') ks

runKindChecker :: Map Name (Kind Kinded) -> Type Transformed -> Errors Error (Type Kinded)
runKindChecker kindEnv type' =
  runRS (check type') (Map.map asPartiallyKinded kindEnv) initialSubCtx
 where
  asPartiallyKinded :: Kind Kinded -> Kind PartiallyKinded
  asPartiallyKinded = unsafeCoerce

inferMany :: Map Name (Type Transformed) -> RSE KindEnv SubCtx Error (Map Name (Type Kinded))
inferMany types = do
  kindEnv <- fmap Map.fromList $ forM (Map.keys types) $ \binder -> do
    kind <- newSubVar (range binder)
    pure (binder, kind)
  types' <- local (const kindEnv) $ forM (Map.toList types) $ \(binder, type') -> do
    type'' <- infer type'
    unifyType type'' (find binder kindEnv)
    pure (binder, type'')
  ks <- resolveUndecidable <$> gets sub
  pure $ Map.map (\t -> runReader (complete t) ks) (Map.fromList types')

runKindInferrer :: Map Name (Type Transformed) -> Errors Error (Map Name (Type Kinded))
runKindInferrer types = runRS (inferMany types) Map.empty initialSubCtx
