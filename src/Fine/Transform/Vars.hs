module Fine.Transform.Vars (handleTypeVars, handleExprVars) where

import Control.Monad (forM_, unless)
import Control.Monad.Trans.RW (RW, asks, runRW, tell, withReader)
import Data.Errors (Errors (Errors), error', warning)
import Data.List.Extra (repeated)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Fine.Error (
  Error (RepeatedCapture, RepeatedVar, UndefinedVar, UnusedUniVar),
  Warning (UnusedVar),
  errorUNREACHABLE,
 )
import Fine.Syntax (
  Block (..),
  Expr (..),
  Id,
  Pass (Transformed),
  Pattern (..),
  Type (..),
  idText,
 )
import Fine.Syntax.Utils (boundVars)

type Errors' = Errors Error Warning

check :: Id -> RW (Set Id) Errors' ()
check var = do
  isDefined <- asks (Set.member var)
  unless isDefined (tell $ error' $ UndefinedVar var)

-- TYPE

type Type' = Type Transformed

typeFreeVars :: Type' -> RW (Set Id) Errors' (Set Id)
typeFreeVars (LiteralT _ _) = return Set.empty
typeFreeVars (VoidT _) = return Set.empty
typeFreeVars (TupleT _ types) = Set.unions <$> mapM typeFreeVars types
typeFreeVars (RecordT _ propTypes) = Set.unions <$> mapM (typeFreeVars . snd) propTypes
typeFreeVars (FunT _ argTypes bodyType) = do
  argsVars <- Set.unions <$> mapM typeFreeVars argTypes
  bodyVars <- typeFreeVars bodyType
  return (Set.union argsVars bodyVars)
typeFreeVars (Forall _ univars type') = do
  let univarList = NonEmpty.toList univars
  forM_ (repeated univarList) (tell . error' . RepeatedVar)
  let univars' = Set.fromList univarList
  typeVars <- withReader (Set.union univars') (typeFreeVars type')
  forM_ (Set.difference univars' typeVars) (tell . error' . UnusedUniVar)
  return (Set.difference typeVars univars')
typeFreeVars (TData _ _ types) = Set.unions <$> mapM typeFreeVars types
typeFreeVars (TVar _ var) = check var >> return (Set.singleton var)
typeFreeVars (TApp _ typeFun typeArgs) = do
  funVars <- typeFreeVars typeFun
  argsVars <- Set.unions <$> mapM typeFreeVars typeArgs
  return (Set.union funVars argsVars)
typeFreeVars (TFun _ typeParams typeBody) = do
  let typeParamList = NonEmpty.toList typeParams
  forM_ (repeated typeParamList) (tell . error' . RepeatedVar)
  let typeParams' = Set.fromList typeParamList
  typeVars <- withReader (Set.union typeParams') (typeFreeVars typeBody)
  forM_ (Set.difference typeParams' typeVars) (tell . warning . UnusedVar)
  return (Set.difference typeVars typeParams')

handleTypeVars :: Set Id -> Type' -> (Set Id, [Error], [Warning])
handleTypeVars vars type' =
  let (free, Errors errs wrns) = runRW (typeFreeVars type') vars
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

type Block' = Block Transformed

type Expr' = Expr Transformed

blockFreeVars :: Block' -> RW Vars Errors' Vars
blockFreeVars (Return expr) = exprFreeVars expr
blockFreeVars (Do expr block) = union' <$> exprFreeVars expr <*> blockFreeVars block
blockFreeVars (Mut var expr block) = do
  withReader vars (check var)
  exprVars <- exprFreeVars expr
  blockVars <- blockFreeVars block
  return (union' (insertVar var exprVars) blockVars)
blockFreeVars (Debug expr block) = union' <$> exprFreeVars expr <*> blockFreeVars block
blockFreeVars (Let _ binder' expr block) = do
  exprVars <- exprFreeVars expr
  blockVars <- withReader (insertVar binder') (blockFreeVars block)
  unless (memberVar binder' blockVars) (tell $ warning $ UnusedVar binder')
  return (union' exprVars (deleteVar binder' blockVars))
blockFreeVars (Loop cond actions block) =
  unions' <$> sequence [exprFreeVars cond, blockFreeVars actions, blockFreeVars block]

patternFreeVars :: Pattern -> RW (Set Id) Errors' (Set Id)
patternFreeVars (LiteralP _ _) = return Set.empty
patternFreeVars (DataP _ tag patts) = do
  pattsVars <- Set.unions <$> mapM patternFreeVars patts
  check tag
  return (Set.insert tag pattsVars)
patternFreeVars (RecordP _ props) = Set.unions <$> mapM (patternFreeVars . snd) props
patternFreeVars (TupleP _ patts) = Set.unions <$> mapM patternFreeVars patts
patternFreeVars (Capture _) = return Set.empty
patternFreeVars (Discard _) = return Set.empty

matchFreeVars :: (Pattern, Expr') -> RW Vars Errors' Vars
matchFreeVars (patt, cont) = do
  pattVars <- withReader vars (patternFreeVars patt)
  pattBound <- do
    let bound = boundVars patt
    forM_ (repeated bound) (tell . error' . RepeatedCapture)
    return (Set.fromList bound)
  contVars <- withReader (unionVars pattBound) (exprFreeVars cont)
  forM_ (Set.difference pattBound (vars contVars)) (tell . warning . UnusedVar)
  return (unionVars pattVars (differenceVars contVars pattBound))

exprFreeVars :: Expr' -> RW Vars Errors' Vars
exprFreeVars (Literal _ _) = return emptyVars
exprFreeVars (Data _ _ exprs) = unions' <$> mapM exprFreeVars exprs
exprFreeVars (Record _ props) = unions' <$> mapM (exprFreeVars . snd) props
exprFreeVars (Tuple _ exprs) = unions' <$> mapM exprFreeVars exprs
exprFreeVars (Var _ var) = withReader vars (check var) >> return (singleVar var)
exprFreeVars (App _ f args) = do
  fVars <- exprFreeVars f
  argsVars <- unions' <$> mapM exprFreeVars args
  return (union' fVars argsVars)
exprFreeVars (GenApp _ genF typeArgs) = do
  fVars <- exprFreeVars genF
  argsVars <- Set.unions <$> withReader tVars (mapM typeFreeVars typeArgs)
  return (unionTVars argsVars fVars)
exprFreeVars (Access _ expr _) = exprFreeVars expr
exprFreeVars (Index _ expr _) = exprFreeVars expr
exprFreeVars (Cond _ cond yes no) = unions' <$> mapM exprFreeVars [cond, yes, no]
exprFreeVars (PatternMatch _ expr matches) = do
  exprVars <- exprFreeVars expr
  matchesVars <- unions' <$> mapM matchFreeVars matches
  return (union' exprVars matchesVars)
exprFreeVars (Fun _ params body) = do
  let paramList = NonEmpty.toList params
  forM_ (repeated paramList) (tell . error' . RepeatedVar)
  let params' = Set.fromList paramList
  bodyVars <- withReader (unionVars params') (exprFreeVars body)
  do
    let unused = Set.difference (Set.filter relevant params') (vars bodyVars)
    forM_ unused (tell . warning . UnusedVar)
  return (differenceVars bodyVars params')
 where
  relevant var = case Text.uncons (idText var) of
    Just (ch', _) -> ch' /= '_'
    _ -> errorUNREACHABLE
exprFreeVars (GenFun _ typeParams body) = do
  let typeParamList = NonEmpty.toList typeParams
  forM_ (repeated typeParamList) (tell . error' . RepeatedVar)
  let typeParams' = Set.fromList typeParamList
  bodyVars <- withReader (unionTVars typeParams') (exprFreeVars body)
  forM_ (Set.difference typeParams' (tVars bodyVars)) (tell . warning . UnusedVar)
  return (differenceTVars bodyVars typeParams')
exprFreeVars (Block _ block) = blockFreeVars block

handleExprVars :: Set Id -> Set Id -> Expr' -> (Set Id, Set Id, [Error], [Warning])
handleExprVars vars tVars expr =
  let (Vars tVars' vars', Errors errs wrns) = runRW (exprFreeVars expr) (Vars tVars vars)
   in (vars', tVars', errs, wrns)
