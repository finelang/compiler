module Fine.Codegen.TailRec (tryOptimize) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, ReaderT (runReaderT), ask, asks, runReader)
import Data.Functor qualified as Functor
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Text (cons)
import Fine.Syntax (
  Block (..),
  Expr (..),
  Id (Id),
  Kind (KLit),
  Lit (Bool),
  LitT (UnitT),
  Pass (Typed),
  Range (NoRange),
  Type (LiteralT),
 )
import Fine.Syntax.Utils (boundVars, flattenApp, flattenFun)

type Expr' = Expr Typed

type Block' = Block Typed

data Substt = Substt
  { oldVar :: Id,
    _newVar :: Id
  }

replaceIn :: Expr' -> Reader Substt Expr'
replaceIn expr@(Literal{}) = return expr
replaceIn (Data ext tag exprs) = Data ext tag <$> mapM replaceIn exprs
replaceIn (Record ext props) = Record ext <$> (mapM . mapM) replaceIn props
replaceIn (Tuple ext exprs) = Tuple ext <$> mapM replaceIn exprs
replaceIn expr@(Var ext var) = do
  (Substt old new) <- ask
  return (if var == old then Var ext new else expr)
replaceIn (Mut ext var expr) = do
  (Substt old new) <- ask
  Mut ext (if var == old then new else var) <$> replaceIn expr
replaceIn (App ext f arg) = App ext <$> replaceIn f <*> replaceIn arg
replaceIn (Access ext expr prop) = Access ext <$> replaceIn expr <*> return prop
replaceIn (Index ext expr ix) = Index ext <$> replaceIn expr <*> return ix
replaceIn (Cond ext cond yes no) =
  Cond ext <$> replaceIn cond <*> replaceIn yes <*> replaceIn no
replaceIn fun@(Fun ext param body) = do
  old <- asks oldVar
  if param == old
    then return fun
    else Fun ext param <$> replaceIn body
replaceIn (Block ext' block') = Block ext' <$> inBlock block'
 where
  inBlock (Return expr) = Return <$> replaceIn expr
  inBlock (Do action block) = Do <$> replaceIn action <*> inBlock block
  inBlock (Let isMut binder expr block) = do
    old <- asks oldVar
    let blockAction = if old == binder then return else inBlock
    Let isMut binder <$> replaceIn expr <*> blockAction block
  inBlock block@(Void _) = return block
  inBlock (Loop cond actions block) =
    Loop <$> replaceIn cond <*> inBlock actions <*> inBlock block
replaceIn (PatternMatch ext expr matches) =
  PatternMatch ext <$> replaceIn expr <*> mapM inMatch matches
 where
  inMatch (patt, cont) = do
    old <- asks oldVar
    if old `elem` boundVars patt
      then return (patt, cont)
      else (,) patt <$> replaceIn cont
replaceIn (Debug ext expr) = Debug ext <$> replaceIn expr
replaceIn expr@(External _ _ _) = return expr

replaceVar :: Substt -> Expr' -> Expr'
replaceVar substt expr = runReader (replaceIn expr) substt

data TransformCtx = TransformCtx
  { fBinder :: Id,
    _fParams :: (NonEmpty Id)
  }

type RM t = ReaderT TransformCtx Maybe t

tryTransformBlock :: (Expr' -> RM Expr') -> Block' -> RM Block'
tryTransformBlock f (Return expr) = Return <$> f expr
tryTransformBlock f (Do expr block) = Do expr <$> tryTransformBlock f block
tryTransformBlock f (Let isMut binder expr block) = do
  isBound <- asks $ (binder ==) . fBinder
  if isBound
    then lift Nothing
    else Let isMut binder expr <$> tryTransformBlock f block
tryTransformBlock _ (Void _) = lift Nothing
tryTransformBlock f (Loop cond actions block) = Loop cond actions <$> tryTransformBlock f block

invalidX :: (Range, Type Typed)
invalidX = (NoRange, LiteralT (NoRange, KLit NoRange) UnitT)

tryTransformRecBranch :: Expr' -> RM Expr'
tryTransformRecBranch app@(App ext _ _) = do
  (TransformCtx binder params) <- ask
  case flattenApp app of
    Just (Var _ name, args) | name == binder && length args == length params -> do
      let muts =
            NonEmpty.zipWith
              (\(Id r' name') arg -> Mut invalidX (Id r' (cons '$' name')) arg)
              params
              args
      let block = foldr Do (Void ()) muts
      return (Block ext block)
    _ -> lift Nothing
tryTransformRecBranch (Block ext block) =
  Block ext <$> tryTransformBlock (tryTransformRecBranch) block
tryTransformRecBranch _ = lift Nothing

resultVar :: Id
resultVar = Id NoRange "$$result"

nonstopVar :: Id
nonstopVar = Id NoRange "$$nonstop"

stop :: Expr Typed
stop = Mut invalidX nonstopVar (Literal invalidX (Bool False))

transformNonRecBranch :: Expr' -> Expr'
transformNonRecBranch expr =
  let setResult = Mut invalidX resultVar expr
      block = Do setResult $ Do stop $ Void ()
   in Block invalidX block

tryTransformBranches :: Expr' -> RM Expr'
tryTransformBranches (PatternMatch ext expr matches) = do
  ctx <- ask
  let (patts, branches) = Functor.unzip matches
  let recsTransformed = fmap (\cont -> runReaderT (tryTransformRecBranch cont) ctx) branches
  if null (catMaybes $ NonEmpty.toList recsTransformed)
    then lift Nothing
    else
      let allTransformed =
            NonEmpty.zipWith
              (\cont optCont -> fromMaybe (transformNonRecBranch cont) optCont)
              branches
              recsTransformed
       in return (PatternMatch ext expr (NonEmpty.zip patts allTransformed))
tryTransformBranches (Cond ext cond yes no) = do
  ctx <- ask
  let branches = [yes, no]
  let recsTransformed = map (\expr -> runReaderT (tryTransformRecBranch expr) ctx) branches
  if null (catMaybes recsTransformed)
    then lift Nothing
    else
      let allTransformed =
            zipWith
              (\expr optExpr -> fromMaybe (transformNonRecBranch expr) optExpr)
              branches
              recsTransformed
       in return (Cond ext cond (allTransformed !! 0) (allTransformed !! 1))
tryTransformBranches (Block ext block) =
  Block ext <$> tryTransformBlock tryTransformBranches block
tryTransformBranches _ = lift Nothing

tryOptimize :: Id -> Expr' -> Maybe Expr'
tryOptimize binder fun@(Fun _ _ _) = do
  let (body, params) = fromJust (flattenFun fun)
  body' <- runReaderT (tryTransformBranches body) (TransformCtx binder params)
  let varSubstts = NonEmpty.map (\old@(Id r name) -> Substt old (Id r (cons '$' name))) params
  let body'' = foldr replaceVar body' varSubstts
  let retResult = Return (Var invalidX resultVar)
  let loop = Loop (Var invalidX nonstopVar) (Do body'' (Void ())) retResult
  let letNonStop = Let True nonstopVar (Literal invalidX (Bool True)) loop
  let letResult = Let True resultVar (External invalidX [] "null") letNonStop
  let block = foldr (\(Substt old new) block' -> Let True new (Var invalidX old) block') letResult varSubstts
  return (foldr (Fun invalidX) (Block invalidX block) params)
tryOptimize _ _ = Nothing
