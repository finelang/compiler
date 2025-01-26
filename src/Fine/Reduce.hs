module Fine.Reduce (reduce, replace) where

import Control.Monad.Trans.State.Strict (State, gets, modify, runState)
import Control.Monad.Trans.State.Strict.Extra (withTempState)
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
    PropsPattern (PropsPattern),
    boundVars,
  )
import Fine.Syntax.Common (Prop (..), Var (Var))

type FreeVars = Set Var

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

convert :: [Var] -> Expr -> State Substt ([Var], Expr)
convert vars expr = go vars [] expr
  where
    freshVar var moreVars =
      let (Var name r) = fromMaybe var (max var <$> S.lookupMax moreVars)
       in Var (snoc name '_') r

    go [] bvs' expr' = return (reverse bvs', expr')
    go (bv : bvs) bvs' expr' = do
      vars' <- getSubsttFreeVars
      if S.notMember bv vars'
        then go bvs (bv : bvs') expr'
        else do
          let bv' = freshVar bv $ S.union vars' (S.fromList $ bvs ++ bvs')
          expr'' <- withTempState (const $ Substt (Id bv') Nothing) (replace' bv expr')
          go bvs (bv' : bvs') expr''

replacePropsPatternBoundVar :: Var -> Var -> PropsPattern -> PropsPattern
replacePropsPatternBoundVar old new (PropsPattern named optSpread) =
  let named' = map (fmap $ replaceBoundVar old new) named
      optSpread' = fmap (\var -> if var == old then new else var) optSpread
   in PropsPattern named' optSpread'

replaceBoundVar :: Var -> Var -> Pattern -> Pattern
replaceBoundVar _ _ patt@(LiteralPatt _ _) = patt
replaceBoundVar old new (ObjPatt props r) =
  ObjPatt (replacePropsPatternBoundVar old new props) r
replaceBoundVar old new (VariantPatt tag props r) =
  VariantPatt tag (replacePropsPatternBoundVar old new props) r
replaceBoundVar old new (TuplePatt fst' snd' rest r) =
  TuplePatt
    (replaceBoundVar old new fst')
    (replaceBoundVar old new snd')
    (map (replaceBoundVar old new) rest)
    r
replaceBoundVar old new patt@(Capture var) =
  if var == old then Capture new else patt

replaceMatch :: Var -> (Pattern, Expr) -> State Substt (Pattern, Expr)
replaceMatch x (patt, expr) = do
  let bvs = boundVars patt
  (bvs', expr') <- convert bvs expr
  let boundVarReplacements = filter (uncurry (/=)) (zip bvs bvs')
  let patt' = foldr (uncurry replaceBoundVar) patt boundVarReplacements
  expr'' <- replace' x expr'
  return (patt', expr'')

replaceProp :: Var -> Prop Expr -> State Substt (Prop Expr)
replaceProp x (NamedProp name expr) = NamedProp name <$> replace' x expr
replaceProp x (SpreadProp expr) = SpreadProp <$> replace' x expr

replaceBlock :: Var -> Block -> State Substt Block
replaceBlock x (Return expr) = Return <$> replace' x expr
replaceBlock x (Do expr block) = Do <$> replace' x expr <*> replaceBlock x block
replaceBlock _ (Let _ _ _ _) = errorTODO

replace' :: Var -> Expr -> State Substt Expr
replace' _ expr@(Literal _ _) = return expr
replace' x (Obj props r) = do
  props' <- mapM (replaceProp x) props
  return (Obj props' r)
replace' x (Variant tag props r) = do
  props' <- mapM (replaceProp x) props
  return (Variant tag props' r)
replace' x (Tuple fst' snd' rest r) = do
  fst'' <- replace' x fst'
  snd'' <- replace' x snd'
  rest' <- mapM (replace' x) rest
  return (Tuple fst'' snd'' rest' r)
replace' x expr@(Id var) = if var == x then gets substt else return expr
replace' x (App f args r) = do
  f' <- replace' x f
  args' <- mapM (replace' x) args
  return (App f' args' r)
replace' x (Access expr r) = do
  expr' <- replace' x expr
  return (Access expr' r)
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
      (params', body') <- convert params body
      body'' <- replace' x body'
      return (Fun params' body'' r)
replace' x (Block block r) = do
  block' <- replaceBlock x block
  return (Block block' r)
replace' _ expr@(ExtExpr _) = return expr
replace' x (Debug expr r) = do
  expr' <- replace' x expr
  return (Debug expr' r)
replace' _ expr@(Closed _) = return expr

replace :: Var -> Expr -> Expr -> Expr
replace var with_ in_ = fst $ runState (replace' var in_) (Substt with_ Nothing)

reduce :: a
reduce = errorTODO
