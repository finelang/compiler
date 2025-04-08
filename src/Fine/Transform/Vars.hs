module Fine.Transform.Vars (handleExprVars, handleTypeVars) where

import Control.Monad (forM_, unless)
import Control.Monad.Trans.RW (RW, asks, runRW, tell, withReader)
import Data.List.Extra (repeated)
import Data.Set (Set)
import Data.Set qualified as Set
import Fine.Error (
  Error (..),
  Errors,
  Warning (UnusedVar),
  collectError,
  collectWarning,
 )
import Fine.Syntax (
  Block (..),
  Expr (..),
  Id,
  Pass (Transformed),
  Pattern (..),
  Type (..),
 )
import Fine.Syntax.Utils (boundVars)

type Env = Set Id

type FreeVars = Set Id

checkDefined :: Id -> RW Env Errors ()
checkDefined var = do
  isDefined <- asks (Set.member var)
  unless isDefined (tell $ collectError $ UndefinedVar var)

-- TYPE

type Type' = Type Transformed

typeFreeVars :: Type' -> RW Env Errors FreeVars
typeFreeVars (LiteralT _ _) = return Set.empty
typeFreeVars (TupleT _ types) = Set.unions <$> mapM typeFreeVars types
typeFreeVars (RecordT _ propTypes) = Set.unions <$> mapM (typeFreeVars . snd) propTypes
typeFreeVars (FunT _ argt bodyt) = Set.union <$> typeFreeVars argt <*> typeFreeVars bodyt
typeFreeVars (Forall _ (var, _) type') = do
  typeVars <- withReader (Set.insert var) (typeFreeVars type')
  unless (Set.member var typeVars) (tell $ collectWarning $ UnusedVar var)
  return (Set.delete var typeVars)
typeFreeVars (TData _ _ types) = Set.unions <$> mapM typeFreeVars types
typeFreeVars (TVar _ var) = checkDefined var >> return (Set.singleton var)
typeFreeVars (TApp _ tfun targ) = Set.union <$> typeFreeVars tfun <*> typeFreeVars targ
typeFreeVars (TFun _ param tbody) = do
  typeVars <- withReader (Set.insert param) (typeFreeVars tbody)
  unless (Set.member param typeVars) (tell $ collectWarning $ UnusedVar param)
  return (Set.delete param typeVars)

handleTypeVars :: Env -> Type' -> (FreeVars, Errors)
handleTypeVars vars type' = runRW (typeFreeVars type') vars

-- EXPR

type Block' = Block Transformed

type Expr' = Expr Transformed

blockFreeVars :: Block' -> RW Env Errors FreeVars
blockFreeVars (Return expr) = exprFreeVars expr
blockFreeVars (Do expr block) = Set.union <$> exprFreeVars expr <*> blockFreeVars block
blockFreeVars (Let _ binder expr block) = do
  exprVars <- exprFreeVars expr
  blockVars <- withReader (Set.insert binder) (blockFreeVars block)
  unless (Set.member binder blockVars) (tell $ collectWarning $ UnusedVar binder)
  return (Set.union exprVars (Set.delete binder blockVars))
blockFreeVars (Loop cond actions block) =
  Set.unions <$> sequence [exprFreeVars cond, blockFreeVars actions, blockFreeVars block]
blockFreeVars (Void _) = return Set.empty

patternFreeVars :: Pattern -> RW Env Errors FreeVars
patternFreeVars (LiteralP _ _) = return Set.empty
patternFreeVars (DataP _ tag patts) = do
  pattsVars <- Set.unions <$> mapM patternFreeVars patts
  checkDefined tag
  return (Set.insert tag pattsVars)
patternFreeVars (RecordP _ props) = Set.unions <$> mapM (patternFreeVars . snd) props
patternFreeVars (TupleP _ patts) = Set.unions <$> mapM patternFreeVars patts
patternFreeVars (Capture _ _) = return Set.empty
patternFreeVars (Discard _) = return Set.empty

matchFreeVars :: (Pattern, Expr') -> RW Env Errors FreeVars
matchFreeVars (patt, cont) = do
  pattVars <- patternFreeVars patt
  pattBound <- do
    let bound = boundVars patt
    forM_ (repeated bound) (tell . collectError . RepeatedCapture)
    return (Set.fromList bound)
  contVars <- withReader (Set.union pattBound) (exprFreeVars cont)
  forM_ (Set.difference pattBound contVars) (tell . collectWarning . UnusedVar)
  return (Set.union pattVars (Set.difference contVars pattBound))

exprFreeVars :: Expr' -> RW Env Errors FreeVars
exprFreeVars (Literal _ _) = return Set.empty
exprFreeVars (Data _ _ exprs) = Set.unions <$> mapM exprFreeVars exprs
exprFreeVars (Record _ props) = Set.unions <$> mapM (exprFreeVars . snd) props
exprFreeVars (Tuple _ exprs) = Set.unions <$> mapM exprFreeVars exprs
exprFreeVars (Var _ var) = checkDefined var >> return (Set.singleton var)
exprFreeVars (Mut _ var expr) = checkDefined var >> Set.insert var <$> exprFreeVars expr
exprFreeVars (App _ f arg) = Set.union <$> exprFreeVars f <*> exprFreeVars arg
exprFreeVars (Access _ expr _) = exprFreeVars expr
exprFreeVars (Index _ expr _) = exprFreeVars expr
exprFreeVars (Cond _ cond yes no) = Set.unions <$> mapM exprFreeVars [cond, yes, no]
exprFreeVars (PatternMatch _ expr matches) = do
  exprVars <- exprFreeVars expr
  matchesVars <- Set.unions <$> mapM matchFreeVars matches
  return (Set.union exprVars matchesVars)
exprFreeVars (Fun _ param body) = do
  bodyVars <- withReader (Set.insert param) (exprFreeVars body)
  unless (Set.member param bodyVars) (tell $ collectWarning $ UnusedVar param)
  return (Set.delete param bodyVars)
exprFreeVars (Block _ block) = blockFreeVars block
exprFreeVars (Debug _ expr) = exprFreeVars expr
exprFreeVars (External _ _ _) = return Set.empty

handleExprVars :: Env -> Expr' -> (FreeVars, Errors)
handleExprVars vars expr = runRW (exprFreeVars expr) vars
