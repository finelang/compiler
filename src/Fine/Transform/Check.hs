module Fine.Transform.Check (alreadyDefined, runTypeVarChecker, runExprVarChecker) where

import Control.Monad (forM_, when)
import Control.Monad.Collector (Collector)
import Control.Monad.Collector qualified as Collector
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Errors (ErrorsT)
import Control.Monad.Trans.Errors qualified as Errors
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, withReaderT)
import Data.List (group, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Fine.Error (
  Error (AlreadyDefined, UndefinedVar, UnusedUniVar, UsageBeforeInit),
  Warning (UnusedVar),
 )
import Fine.Syntax (
  Block (..),
  Expr (..),
  Id,
  Pattern (..),
  Phase (Transformed),
  Type (..),
 )
import Fine.Syntax.Name (isRelevant)
import Fine.Syntax.Utils (isFunction, patternBoundVars)

type REC r e c a = ReaderT r (ErrorsT e (Collector c)) a

failure :: e -> REC r e c a
failure = lift . Errors.failure

collect :: c -> REC r e c ()
collect = lift . lift . Collector.collect

runREC :: REC r e c a -> r -> (Either (NonEmpty e) a, [c])
runREC rec' r = Collector.runCollector $ Errors.runErrorsT $ runReaderT rec' r

alreadyDefined :: [Id] -> [Error]
alreadyDefined xs = (concat . map mkErr . group . sort) xs
 where
  mkErr [] = []
  mkErr [_] = []
  mkErr (y : ys) = map (AlreadyDefined y) ys

checked :: Id -> REC (Set Id) Error c Id
checked var = do
  isDefined <- asks (Set.member var)
  if isDefined
    then pure var
    else failure (UndefinedVar var)

-- TYPE

checkType :: Type Transformed -> REC (Set Id) Error Warning (Set Id)
checkType (LiteralT _ _ _) = pure Set.empty
checkType (VoidT _ _) = pure Set.empty
checkType (TupleT _ _ fst' snd' rest) = Set.unions <$> mapM checkType (fst' : snd' : rest)
checkType (RecordT _ _ propTypes) = Set.unions <$> mapM (checkType . snd) propTypes
checkType (FunT _ at bt) = Set.union <$> checkType at <*> checkType bt
checkType (Forall _ _ univars type') = do
  let univarList = NonEmpty.toList univars
  forM_ (alreadyDefined univarList) failure
  let univars' = Set.fromList univarList
  typeVars <- withReaderT (Set.union univars') (checkType type')
  forM_ (univars' \\ typeVars) (failure . UnusedUniVar)
  pure (typeVars \\ univars')
checkType (DataT _ _ types) = Set.unions <$> mapM checkType types
checkType (TVar _ var) = Set.singleton <$> checked var
checkType (TApp _ tf ta) = Set.union <$> checkType tf <*> checkType ta
checkType (TFun _ tp tb) = do
  tbVars <- withReaderT (Set.insert tp) (checkType tb)
  if Set.member tp tbVars
    then pure (Set.delete tp tbVars)
    else collect (UnusedVar tp) >> pure tbVars

runTypeVarChecker :: Set Id -> Type Transformed -> (Either (NonEmpty Error) (Set Id), [Warning])
runTypeVarChecker vars type' = runREC (checkType type') vars

-- EXPR

data Var = V Id | T Id
  deriving (Eq, Ord)

vVars :: Set Var -> Set Id
vVars = Set.foldr (\var vars -> maybe vars (`Set.insert` vars) (justVVar var)) Set.empty
 where
  justVVar (V var) = Just var
  justVVar _ = Nothing

tVars :: Set Var -> Set Id
tVars = Set.foldr (\var vars -> maybe vars (`Set.insert` vars) (justTVar var)) Set.empty
 where
  justTVar (T var) = Just var
  justTVar _ = Nothing

blockBoundVars :: Block Transformed -> [Id]
blockBoundVars (Return _) = []
blockBoundVars Void = []
blockBoundVars (Do _ block) = blockBoundVars block
blockBoundVars (Mut _ _ block) = blockBoundVars block
blockBoundVars (LetMut binder _ block) = binder : blockBoundVars block
blockBoundVars (Let _ patt _ block) = patternBoundVars patt ++ blockBoundVars block
blockBoundVars (Debug _ _ block) = blockBoundVars block
blockBoundVars (Loop _ _ block) = blockBoundVars block

checkBlock :: Block Transformed -> REC (Set Var) Error Warning (Set Var)
checkBlock (Return expr) = checkExpr expr
checkBlock Void = pure Set.empty
checkBlock (Do expr block) = Set.union <$> checkExpr expr <*> checkBlock block
checkBlock (Mut var expr block) =
  (\var' exprVars blockVars -> Set.insert (V var') $ Set.union exprVars blockVars)
    <$> withReaderT vVars (checked var)
    <*> checkExpr expr
    <*> checkBlock block
checkBlock (LetMut binder expr block) = Set.union <$> goExpr <*> goBlock
 where
  binder' = V binder
  goExpr = do
    exprVars <- withReaderT (Set.insert binder') (checkExpr expr)
    when
      (not (isFunction expr) && Set.member binder' exprVars)
      (failure $ UsageBeforeInit binder)
    pure exprVars
  goBlock = do
    blockVars <- withReaderT (Set.insert binder') (checkBlock block)
    if (Set.member binder' blockVars)
      then pure (Set.delete binder' blockVars)
      else (collect $ UnusedVar binder) >> pure blockVars
checkBlock (Let _ (Capture binder) expr block) =
  checkBlock (LetMut binder expr block) -- check as if it is a mutable var local binding
checkBlock (Let _ patt expr block) =
  Set.union <$> checkExpr expr <*> do
    let boundVars = Set.fromList $ map V $ patternBoundVars patt
    blockVars <- withReaderT (Set.union boundVars) (checkBlock block)
    forM_ (Set.toList $ vVars $ boundVars \\ blockVars) (collect . UnusedVar)
    pure (blockVars \\ boundVars)
checkBlock (Debug _ expr block) = Set.union <$> checkExpr expr <*> checkBlock block
checkBlock (Loop cond actions block) =
  Set.unions <$> sequence [checkExpr cond, checkBlock actions, checkBlock block]

checkPattern :: Pattern -> REC (Set Id) Error Warning (Set Var)
checkPattern (LiteralP _ _) = pure Set.empty
checkPattern (DataP _ tag patts) =
  (\var pattsVars -> Set.insert (V var) (Set.unions pattsVars))
    <$> checked tag
    <*> mapM checkPattern patts
checkPattern (RecordP _ props) = Set.unions <$> mapM (checkPattern . snd) props
checkPattern (TupleP _ fst' snd' rest) = Set.unions <$> mapM checkPattern (fst' : snd' : rest)
checkPattern (Capture _) = pure Set.empty
checkPattern (Discard _) = pure Set.empty

checkMatch :: (Pattern, Expr Transformed) -> REC (Set Var) Error Warning (Set Var)
checkMatch (patt, cont) =
  Set.union <$> withReaderT vVars (checkPattern patt) <*> do
    let boundVars = patternBoundVars patt
    forM_ (alreadyDefined boundVars) failure
    let boundVars' = Set.fromList $ map V boundVars
    contVars <- withReaderT (Set.union boundVars') (checkExpr cont)
    forM_ (Set.toList $ vVars $ boundVars' \\ contVars) (collect . UnusedVar)
    pure (contVars \\ boundVars')

checkExpr :: Expr Transformed -> REC (Set Var) Error Warning (Set Var)
checkExpr (Literal _ _ _) = pure Set.empty
checkExpr (Data _ _ exprs) = Set.unions <$> mapM checkExpr exprs
checkExpr (Record _ _ props) = Set.unions <$> mapM (checkExpr . snd) props
checkExpr (Tuple _ _ fst' snd' rest) = Set.unions <$> mapM checkExpr (fst' : snd' : rest)
checkExpr (Var _ var) = Set.singleton . V <$> withReaderT vVars (checked var)
checkExpr (Bin _ _ _ left right) = Set.union <$> checkExpr left <*> checkExpr right
checkExpr (App _ f arg) = Set.union <$> checkExpr f <*> checkExpr arg
checkExpr (GenApp _ _ fname typeArgs) =
  (\fVar argsVars -> Set.insert (V fVar) (Set.map T $ Set.unions argsVars))
    <$> withReaderT vVars (checked fname)
    <*> withReaderT tVars (mapM checkType typeArgs)
checkExpr (Access _ expr _) = checkExpr expr
checkExpr (Index _ _ expr _) = checkExpr expr
checkExpr (Cond _ _ cond yes no) = Set.unions <$> mapM checkExpr [cond, yes, no]
checkExpr (PatternMatching _ _ _ expr matches) =
  (\exprVars matchesVars -> Set.union exprVars $ Set.unions matchesVars)
    <$> checkExpr expr
    <*> mapM checkMatch matches
checkExpr (Fun _ param body) = do
  let param' = V param
  bodyVars <- withReaderT (Set.insert param') (checkExpr body)
  when (isRelevant param && Set.notMember param' bodyVars) (collect $ UnusedVar param)
  pure (Set.delete param' bodyVars)
checkExpr (GenFun _ _ _ typeParams body) = do
  let typeParamList = NonEmpty.toList typeParams
  forM_ (alreadyDefined typeParamList) failure
  let typeParams' = Set.fromList $ map T typeParamList
  bodyVars <- withReaderT (Set.union typeParams') (checkExpr body)
  pure (bodyVars \\ typeParams')
checkExpr (Block _ _ block) = do
  forM_ (alreadyDefined $ blockBoundVars block) failure
  checkBlock block

runExprVarChecker :: Set Id -> Set Id -> Expr Transformed -> (Either (NonEmpty Error) (Set Id, Set Id), [Warning])
runExprVarChecker vars tvars expr =
  let (result, wrns) = runREC (checkExpr expr) $ Set.union (Set.map V vars) (Set.map T tvars)
   in case result of
        Left errs -> (Left errs, wrns)
        Right allVars -> (Right (vVars allVars, tVars allVars), wrns)
