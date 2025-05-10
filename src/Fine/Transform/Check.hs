module Fine.Transform.Check (checkType, checkExpr, alreadyDefined) where

import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.RW (RW, asks, runRW, tell, withReader)
import Data.Errors (Errors (Errors), error', warning)

import Data.List (group, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Fine.Error (
  Error (AlreadyDefined, UndefinedVar, UnusedUniVar, UsageBeforeInit),
  Warning (DebugKeywordUsage, UnusedVar),
  errorUNREACHABLE,
 )
import Fine.Syntax (
  Block (..),
  Equation (..),
  Expr (..),
  Id,
  Pattern (..),
  Phase (Parsed),
  Type (..),
  idText,
  range,
 )
import Fine.Syntax.Utils (isFunction, patternBoundVars)

alreadyDefined :: [Id] -> [Error]
alreadyDefined xs = (concat . map mkErr . group . sort) xs
 where
  mkErr [] = []
  mkErr [_] = []
  mkErr (y : ys) = map (AlreadyDefined y) ys

type Errors' = Errors Error Warning

check :: Id -> RW (Set Id) Errors' ()
check var = do
  isDefined <- asks (Set.member var)
  unless isDefined (tell $ error' $ UndefinedVar var)

-- TYPE

type Type' = Type Parsed

checkType' :: Type' -> RW (Set Id) Errors' (Set Id)
checkType' (LiteralT _ _) = return Set.empty
checkType' (VoidT _) = return Set.empty
checkType' (TupleT _ fst' snd' rest) = Set.unions <$> mapM checkType' (fst' : snd' : rest)
checkType' (ListT _ type') = checkType' type'
checkType' (RecordT _ propTypes) = Set.unions <$> mapM (checkType' . snd) propTypes
checkType' (FunT _ argTypes bodyType) = do
  argVars <- Set.unions <$> mapM checkType' argTypes
  bodyVars <- checkType' bodyType
  return (Set.union argVars bodyVars)
checkType' (Forall _ univars type') = do
  let univarList = NonEmpty.toList univars
  forM_ (alreadyDefined univarList) (tell . error')
  let univars' = Set.fromList univarList
  typeVars <- withReader (Set.union univars') (checkType' type')
  forM_ (Set.difference univars' typeVars) (tell . error' . UnusedUniVar)
  return (Set.difference typeVars univars')
checkType' (TData _ _ types) = Set.unions <$> mapM checkType' types
checkType' (TVar _ var) = check var >> return (Set.singleton var)
checkType' (TApp _ typeFun typeArgs) = do
  funVars <- checkType' typeFun
  argsVars <- Set.unions <$> mapM checkType' typeArgs
  return (Set.union funVars argsVars)
checkType' (TFun _ typeParams typeBody) = do
  let typeParamList = NonEmpty.toList typeParams
  forM_ (alreadyDefined typeParamList) (tell . error')
  let typeParams' = Set.fromList typeParamList
  typeVars <- withReader (Set.union typeParams') (checkType' typeBody)
  forM_ (Set.difference typeParams' typeVars) (tell . warning . UnusedVar)
  return (Set.difference typeVars typeParams')

checkType :: Set Id -> Type' -> (Set Id, [Error], [Warning])
checkType vars type' =
  let (free, Errors errs wrns) = runRW (checkType' type') vars
   in (free, errs, wrns)

-- EXPR

data Vars = Vars
  { tVars :: Set Id,
    vars :: Set Id
  }

emptyVars :: Vars
emptyVars = Vars Set.empty Set.empty

union' :: Vars -> Vars -> Vars
union' (Vars tvs vs) (Vars tvs' vs') = Vars (Set.union tvs tvs') (Set.union vs vs')

unions' :: (Foldable t) => t Vars -> Vars
unions' = foldl union' emptyVars
{-# SPECIALIZE unions' :: [Vars] -> Vars #-}
{-# SPECIALIZE unions' :: NonEmpty Vars -> Vars #-}

insertVar :: Id -> Vars -> Vars
insertVar var (Vars tvs vs) = Vars tvs (Set.insert var vs)

unionVars :: Set Id -> Vars -> Vars
unionVars vars (Vars tvs vs) = Vars tvs (Set.union vars vs)

unionTVars :: Set Id -> Vars -> Vars
unionTVars tvars (Vars tvs vs) = Vars (Set.union tvars tvs) vs

differenceVars :: Vars -> Set Id -> Vars
differenceVars (Vars tvs vs) vars = Vars tvs (Set.difference vs vars)

differenceTVars :: Vars -> Set Id -> Vars
differenceTVars (Vars tvs vs) tvars = Vars (Set.difference tvs tvars) vs

singleVar :: Id -> Vars
singleVar var = insertVar var emptyVars

memberVar :: Id -> Vars -> Bool
memberVar var (Vars _ vs) = Set.member var vs

deleteVar :: Id -> Vars -> Vars
deleteVar var (Vars tvs vs) = Vars tvs (Set.delete var vs)

type Block' = Block Parsed

type Expr' = Expr Parsed

blockBoundVars :: Block' -> [Id]
blockBoundVars (Return _) = []
blockBoundVars Void = []
blockBoundVars (Do _ block) = blockBoundVars block
blockBoundVars (Mut _ _ block) = blockBoundVars block
blockBoundVars (Debug _ block) = blockBoundVars block
blockBoundVars (Let _ binder _ block) = binder : blockBoundVars block
blockBoundVars (Loop _ _ block) = blockBoundVars block
blockBoundVars (LetPatt _ patt _ block) = patternBoundVars patt ++ blockBoundVars block

checkEquation :: Equation Expr' -> RW Vars Errors' Vars
checkEquation (Operand expr) = checkExpr' expr
checkEquation (Operation left _ equation) =
  union' <$> checkExpr' left <*> checkEquation equation

checkPartialEquation :: Equation (Either a (Expr Parsed)) -> RW Vars Errors' Vars
checkPartialEquation (Operand xOrExpr) = unions' <$> mapM checkExpr' xOrExpr
checkPartialEquation (Operation xOrExpr _ equation) = do
  exprVars <- unions' <$> mapM checkExpr' xOrExpr
  equationVars <- checkPartialEquation equation
  return (union' exprVars equationVars)

checkBlock :: Block' -> RW Vars Errors' Vars
checkBlock (Return expr) = checkExpr' expr
checkBlock Void = return emptyVars
checkBlock (Do expr block) = union' <$> checkExpr' expr <*> checkBlock block
checkBlock (Mut var expr block) = do
  withReader vars (check var)
  exprVars <- checkExpr' expr
  blockVars <- checkBlock block
  return (union' (insertVar var exprVars) blockVars)
checkBlock (Debug expr block) = do
  tell (warning $ DebugKeywordUsage $ range expr)
  union' <$> checkExpr' expr <*> checkBlock block
checkBlock (Let _ binder' expr block) = do
  exprVars <- withReader (insertVar binder') (checkExpr' expr)
  when
    (not (isFunction expr) && memberVar binder' exprVars)
    (tell $ error' $ UsageBeforeInit binder')
  blockVars <- withReader (insertVar binder') (checkBlock block)
  unless (memberVar binder' blockVars) (tell $ warning $ UnusedVar binder')
  return (union' exprVars (deleteVar binder' blockVars))
checkBlock (Loop cond actions block) =
  unions' <$> sequence [checkExpr' cond, checkBlock actions, checkBlock block]
checkBlock (LetPatt _ patt expr block) = do
  exprVars <- checkExpr' expr
  let pattBound = Set.fromList (patternBoundVars patt)
  blockVars <- withReader (unionVars pattBound) (checkBlock block)
  forM_ (Set.difference pattBound $ vars blockVars) (tell . warning . UnusedVar)
  return (union' exprVars $ differenceVars blockVars pattBound)

checkPattern :: Pattern -> RW (Set Id) Errors' (Set Id)
checkPattern (LiteralP _ _) = return Set.empty
checkPattern (DataP _ tag patts) = do
  pattsVars <- Set.unions <$> mapM checkPattern patts
  check tag
  return (Set.insert tag pattsVars)
checkPattern (RecordP _ props) = Set.unions <$> mapM (checkPattern . snd) props
checkPattern (TupleP _ fst' snd' rest) = Set.unions <$> mapM checkPattern (fst' : snd' : rest)
checkPattern (ListP _ patts) = Set.unions <$> mapM checkPattern patts
checkPattern (Capture _) = return Set.empty
checkPattern (Discard _) = return Set.empty

checkMatch :: (Pattern, Expr') -> RW Vars Errors' Vars
checkMatch (patt, cont) = do
  pattVars <- withReader vars (checkPattern patt)
  let pattBound = patternBoundVars patt
  let pattBound' = Set.fromList pattBound
  contVars <- case cont of
    Block _ block -> do
      forM_ (alreadyDefined $ pattBound ++ blockBoundVars block) (tell . error')
      withReader (unionVars pattBound') (checkBlock block)
    _ -> do
      forM_ (alreadyDefined pattBound) (tell . error')
      withReader (unionVars pattBound') (checkExpr' cont)
  forM_ (Set.difference pattBound' (vars contVars)) (tell . warning . UnusedVar)
  return (unionVars pattVars (differenceVars contVars pattBound'))

checkExpr' :: Expr' -> RW Vars Errors' Vars
checkExpr' (Literal _ _) = return emptyVars
checkExpr' (Data _ _ exprs) = unions' <$> mapM checkExpr' exprs
checkExpr' (Record _ props) = unions' <$> mapM (checkExpr' . snd) props
checkExpr' (Tuple _ fst' snd' rest) = unions' <$> mapM checkExpr' (fst' : snd' : rest)
checkExpr' (List _ exprs) = unions' <$> mapM checkExpr' exprs
checkExpr' (Var _ var) = withReader vars (check var) >> return (singleVar var)
checkExpr' (App _ f args) = do
  fVars <- checkExpr' f
  argsVars <- unions' <$> mapM checkExpr' args
  return (union' fVars argsVars)
checkExpr' (GenApp _ genF typeArgs) = do
  fVars <- checkExpr' genF
  argsVars <- Set.unions <$> withReader tVars (mapM checkType' typeArgs)
  return (unionTVars argsVars fVars)
checkExpr' (Access _ expr _) = checkExpr' expr
checkExpr' (Index _ expr _) = checkExpr' expr
checkExpr' (Cond _ cond yes no) = unions' <$> mapM checkExpr' [cond, yes, no]
checkExpr' (PatternMatching _ expr matches) = do
  exprVars <- checkExpr' expr
  matchesVars <- unions' <$> mapM checkMatch matches
  return (union' exprVars matchesVars)
checkExpr' (Fun _ params body) = do
  let paramList = NonEmpty.toList params
  let params' = Set.fromList paramList
  bodyVars <- case body of
    Block _ block -> do
      forM_ (alreadyDefined $ paramList ++ blockBoundVars block) (tell . error')
      withReader (unionVars params') (checkBlock block)
    _ -> do
      forM_ (alreadyDefined paramList) (tell . error')
      withReader (unionVars params') (checkExpr' body)
  do
    let unused = Set.difference (Set.filter relevant params') (vars bodyVars)
    forM_ unused (tell . warning . UnusedVar)
  return (differenceVars bodyVars params')
 where
  relevant var = case Text.uncons (idText var) of
    Just (ch', _) -> ch' /= '_'
    _ -> errorUNREACHABLE "Found a variable with empty name."
checkExpr' (GenFun _ typeParams body) = do
  let typeParamList = NonEmpty.toList typeParams
  forM_ (alreadyDefined typeParamList) (tell . error')
  let typeParams' = Set.fromList typeParamList
  bodyVars <- withReader (unionTVars typeParams') (checkExpr' body)
  return (differenceTVars bodyVars typeParams')
checkExpr' (Block _ block) = do
  forM_ (alreadyDefined $ blockBoundVars block) (tell . error')
  checkBlock block
checkExpr' (Equation _ equation) = checkEquation equation
checkExpr' (PartialEquation _ equation) = checkPartialEquation equation

checkExpr :: Set Id -> Set Id -> Expr' -> (Set Id, Set Id, [Error], [Warning])
checkExpr vars tVars expr =
  let (Vars tVars' vars', Errors errs wrns) = runRW (checkExpr' expr) (Vars tVars vars)
   in (vars', tVars', errs, wrns)
