module Fine.Reduce (reduce, replace) where

import Control.Monad.Trans.State.Strict (State, gets, modify, runState)
import Control.Monad.Trans.State.Strict.Extra (withTempState)
import Data.List.Extra (toNonEmptyPARTIAL)
import Data.List.NonEmpty (toList)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (snoc)
import Fine.Error (errorTODO)
import Fine.FreeVars (freeVars)
import Fine.Syntax.Abstract
  ( Block (..),
    Expr (..),
    Pattern (..),
    boundVars,
  )
import Fine.Syntax.Common (Id (Id))

type FreeVars = Set Id

data Substt = Substt
  { substt :: Expr,
    substtFreeVars :: Maybe FreeVars
  }

getSubsttFreeVars :: State Substt FreeVars
getSubsttFreeVars = do
  optVars <- gets substtFreeVars
  case optVars of
    Just vars -> return vars
    Nothing -> do
      vars <- gets (freeVars . substt)
      modify (\s -> s {substtFreeVars = Just vars})
      return vars

convert :: [Id] -> Expr -> State Substt ([Id], Expr)
convert vars expr = go vars [] expr
  where
    freshVar var moreVars =
      let (Id name r) = fromMaybe var (max var <$> S.lookupMax moreVars)
       in Id (snoc name '_') r

    go [] bvs' expr' = return (reverse bvs', expr')
    go (bv : bvs) bvs' expr' = do
      vars' <- getSubsttFreeVars
      if S.notMember bv vars'
        then go bvs (bv : bvs') expr'
        else do
          let bv' = freshVar bv $ S.union vars' (S.fromList $ bvs ++ bvs')
          expr'' <- withTempState (const $ Substt (Var bv') Nothing) (replace' bv expr')
          go bvs (bv' : bvs') expr''

replaceBoundVar :: Id -> Id -> Pattern -> Pattern
replaceBoundVar old new patt = case patt of
  LiteralP _ _ -> patt
  DataP tag patts r -> DataP tag (map (replaceBoundVar old new) patts) r
  RecordP props r -> RecordP ((fmap . fmap) (replaceBoundVar old new) props) r
  TupleP patts r -> TupleP (fmap (replaceBoundVar old new) patts) r
  Capture var -> if var == old then Capture new else patt

replaceMatch :: Id -> (Pattern, Expr) -> State Substt (Pattern, Expr)
replaceMatch x (patt, expr) = do
  let bvs = boundVars patt
  (bvs', expr') <- convert bvs expr
  let boundVarReplacements = filter (uncurry (/=)) (zip bvs bvs')
  let patt' = foldr (uncurry replaceBoundVar) patt boundVarReplacements
  expr'' <- replace' x expr'
  return (patt', expr'')

replaceBlock :: Id -> Block -> State Substt Block
replaceBlock x (Return expr) = Return <$> replace' x expr
replaceBlock x (Do expr block) = Do <$> replace' x expr <*> replaceBlock x block
replaceBlock x (Debug expr block) = Debug <$> replace' x expr <*> replaceBlock x block
replaceBlock _ (Let _ _ _ _ _) = errorTODO

replace' :: Id -> Expr -> State Substt Expr
replace' _ expr@(Literal _ _) = return expr
replace' x (Data tag exprs r) = do
  exprs' <- mapM (replace' x) exprs
  return (Data tag exprs' r)
replace' x (Record props r) = do
  props' <- (mapM . mapM) (replace' x) props
  return (Record props' r)
replace' x (Tuple exprs r) = do
  exprs' <- mapM (replace' x) exprs
  return (Tuple exprs' r)
replace' x expr@(Var var) = if var == x then gets substt else return expr
replace' _ (Mut _ _) = errorTODO
replace' x (App f args r) = do
  f' <- replace' x f
  args' <- mapM (replace' x) args
  return (App f' args' r)
replace' x (Access expr r) = do
  expr' <- replace' x expr
  return (Access expr' r)
replace' x (Index expr ix r) = do
  expr' <- replace' x expr
  return (Index expr' ix r)
replace' x (Cond cond yes no r) = do
  cond' <- replace' x cond
  yes' <- replace' x yes
  no' <- replace' x no
  return (Cond cond' yes' no' r)
replace' x (PatternMatch expr matches r) = do
  expr' <- replace' x expr
  matches' <- mapM (replaceMatch x) matches
  return (PatternMatch expr' matches' r)
replace' x fun@(Fun params body r) =
  if x `elem` params
    then return fun
    else do
      (params', body') <- convert (toList params) body
      body'' <- replace' x body'
      return (Fun (toNonEmptyPARTIAL params') body'' r)
replace' x (Block block r) = do
  block' <- replaceBlock x block
  return (Block block' r)
replace' _ expr@(ExtExpr _) = return expr
replace' _ expr@(Closure _ _ _) = return expr

replace :: Id -> Expr -> Expr -> Expr
replace var with in_ = fst $ runState (replace' var in_) (Substt with Nothing)

reduce :: a
reduce = errorTODO
