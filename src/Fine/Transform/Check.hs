module Fine.Transform.Check (alreadyDefined, runTypeVarChecker, runExprVarChecker) where

import Control.Monad (forM_, when)
import Control.Monad.Collector (Collector)
import Control.Monad.Collector qualified as Collector
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Errors (ErrorsT)
import Control.Monad.Trans.Errors qualified as Errors
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, withReaderT)
import Data.List (group, sort)
import Data.List.NonEmpty (NonEmpty, (<|))
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
    then return var
    else failure (UndefinedVar var)

-- TYPE

checkType :: Type Transformed -> REC (Set Id) Error Warning (Set Id)
checkType (LiteralT _ _) = return Set.empty
checkType (VoidT _) = return Set.empty
checkType (TupleT _ fst' snd' rest) = Set.unions <$> mapM checkType (fst' : snd' : rest)
checkType (ListT _ type') = checkType type'
checkType (RecordT _ propTypes) = Set.unions <$> mapM (checkType . snd) propTypes
checkType (FunT _ argTypes bodyType) =
  (\argsVars bodyVars -> Set.union bodyVars $ Set.unions argsVars)
    <$> mapM checkType argTypes
    <*> checkType bodyType
checkType (Forall _ univars type') = do
  let univarList = NonEmpty.toList univars
  forM_ (alreadyDefined univarList) failure
  let univars' = Set.fromList univarList
  typeVars <- withReaderT (Set.union univars') (checkType type')
  forM_ (univars' \\ typeVars) (failure . UnusedUniVar)
  return (typeVars \\ univars')
checkType (DataT _ _ types) = Set.unions <$> mapM checkType types
checkType (TVar _ var) = Set.singleton <$> checked var
checkType (TApp _ typeFun typeArgs) =
  Set.unions <$> mapM checkType (typeFun <| typeArgs)
checkType (TFun _ typeParams typeBody) = do
  let typeParamList = NonEmpty.toList typeParams
  forM_ (alreadyDefined typeParamList) failure
  let typeParams' = Set.fromList typeParamList
  typeVars <- withReaderT (Set.union typeParams') (checkType typeBody)
  forM_ (Set.toList $ typeParams' \\ typeVars) (collect . UnusedVar)
  return (typeVars \\ typeParams')

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
blockBoundVars (Debug _ block) = blockBoundVars block
blockBoundVars (Let _ binder _ block) = binder : blockBoundVars block
blockBoundVars (Loop _ _ block) = blockBoundVars block
blockBoundVars (LetPatt _ patt _ block) = patternBoundVars patt ++ blockBoundVars block

checkBlock :: Block Transformed -> REC (Set Var) Error Warning (Set Var)
checkBlock (Return expr) = checkExpr expr
checkBlock Void = return Set.empty
checkBlock (Do expr block) = Set.union <$> checkExpr expr <*> checkBlock block
checkBlock (Mut var expr block) =
  (\var' exprVars blockVars -> Set.insert (V var') $ Set.union exprVars blockVars)
    <$> withReaderT vVars (checked var)
    <*> checkExpr expr
    <*> checkBlock block
checkBlock (Debug expr block) = Set.union <$> checkExpr expr <*> checkBlock block
checkBlock (Let _ binder expr block) = Set.union <$> goExpr <*> goBlock
 where
  binder' = V binder
  goExpr = do
    exprVars <- withReaderT (Set.insert binder') (checkExpr expr)
    when
      (not (isFunction expr) && Set.member binder' exprVars)
      (failure $ UsageBeforeInit binder)
    return exprVars
  goBlock = do
    blockVars <- withReaderT (Set.insert binder') (checkBlock block)
    if (Set.member binder' blockVars)
      then return (Set.delete binder' blockVars)
      else (collect $ UnusedVar binder) >> return blockVars
checkBlock (Loop cond actions block) =
  Set.unions <$> sequence [checkExpr cond, checkBlock actions, checkBlock block]
checkBlock (LetPatt _ patt expr block) =
  Set.union <$> checkExpr expr <*> do
    let pattBound = Set.fromList $ map V $ patternBoundVars patt
    blockVars <- withReaderT (Set.union pattBound) (checkBlock block)
    forM_ (Set.toList $ vVars $ pattBound \\ blockVars) (collect . UnusedVar)
    return (blockVars \\ pattBound)

checkPattern :: Pattern -> REC (Set Id) Error Warning (Set Var)
checkPattern (LiteralP _ _) = return Set.empty
checkPattern (DataP _ tag patts) =
  (\var pattsVars -> Set.insert (V var) (Set.unions pattsVars))
    <$> checked tag
    <*> mapM checkPattern patts
checkPattern (RecordP _ props) = Set.unions <$> mapM (checkPattern . snd) props
checkPattern (TupleP _ fst' snd' rest) = Set.unions <$> mapM checkPattern (fst' : snd' : rest)
checkPattern (ListP _ patts) = Set.unions <$> mapM checkPattern patts
checkPattern (Capture _) = return Set.empty
checkPattern (Discard _) = return Set.empty

checkMatch :: (Pattern, Expr Transformed) -> REC (Set Var) Error Warning (Set Var)
checkMatch (patt, cont) =
  Set.union <$> withReaderT vVars (checkPattern patt) <*> do
    let pattBound = patternBoundVars patt
    let pattBound' = Set.fromList $ map V pattBound
    contVars <- case cont of
      Block _ block -> do
        forM_ (alreadyDefined $ pattBound ++ blockBoundVars block) failure
        withReaderT (Set.union pattBound') (checkBlock block)
      _ -> do
        forM_ (alreadyDefined pattBound) failure
        withReaderT (Set.union pattBound') (checkExpr cont)
    forM_ (Set.toList $ vVars $ pattBound' \\ contVars) (collect . UnusedVar)
    return (contVars \\ pattBound')

checkExpr :: Expr Transformed -> REC (Set Var) Error Warning (Set Var)
checkExpr (Literal _ _) = return Set.empty
checkExpr (Data _ _ exprs) = Set.unions <$> mapM checkExpr exprs
checkExpr (Record _ props) = Set.unions <$> mapM (checkExpr . snd) props
checkExpr (Tuple _ fst' snd' rest) = Set.unions <$> mapM checkExpr (fst' : snd' : rest)
checkExpr (List _ exprs) = Set.unions <$> mapM checkExpr exprs
checkExpr (Var _ var) = Set.singleton . V <$> withReaderT vVars (checked var)
checkExpr (Bin _ _ _ left right) = Set.union <$> checkExpr left <*> checkExpr right
checkExpr (App _ f args) = Set.unions <$> mapM checkExpr (f <| args)
checkExpr (GenApp _ fname typeArgs) =
  (\fVar argsVars -> Set.insert (V fVar) (Set.map T $ Set.unions argsVars))
    <$> withReaderT vVars (checked fname)
    <*> withReaderT tVars (mapM checkType typeArgs)
checkExpr (Access _ expr _) = checkExpr expr
checkExpr (Index _ expr _) = checkExpr expr
checkExpr (Cond _ cond yes no) = Set.unions <$> mapM checkExpr [cond, yes, no]
checkExpr (PatternMatching _ _ expr matches) =
  (\exprVars matchesVars -> Set.union exprVars $ Set.unions matchesVars)
    <$> checkExpr expr
    <*> mapM checkMatch matches
checkExpr (Fun _ params body) = do
  let paramList = NonEmpty.toList params
  let params' = Set.fromList $ map V paramList
  bodyVars <- case body of
    Block _ block -> do
      forM_ (alreadyDefined $ paramList ++ blockBoundVars block) failure
      withReaderT (Set.union params') (checkBlock block)
    _ -> do
      forM_ (alreadyDefined paramList) failure
      withReaderT (Set.union params') (checkExpr body)
  do
    let unused = (Set.filter isRelevant $ vVars params') \\ (vVars bodyVars)
    forM_ (Set.toList unused) (collect . UnusedVar)
  return (bodyVars \\ params')
checkExpr (GenFun _ _ typeParams body) = do
  let typeParamList = NonEmpty.toList typeParams
  forM_ (alreadyDefined typeParamList) failure
  let typeParams' = Set.fromList $ map T typeParamList
  bodyVars <- withReaderT (Set.union typeParams') (checkExpr body)
  return (bodyVars \\ typeParams')
checkExpr (Block _ block) = do
  forM_ (alreadyDefined $ blockBoundVars block) failure
  checkBlock block

runExprVarChecker :: Set Id -> Set Id -> Expr Transformed -> (Either (NonEmpty Error) (Set Id, Set Id), [Warning])
runExprVarChecker vars tvars expr =
  let (result, wrns) = runREC (checkExpr expr) $ Set.union (Set.map V vars) (Set.map T tvars)
   in case result of
        Left errs -> (Left errs, wrns)
        Right allVars -> (Right (vVars allVars, tVars allVars), wrns)
