module Fine.Codegen.TailRec (optimize) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, ReaderT (runReaderT), ask, asks, runReader)
import qualified Data.Functor as F
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Text (cons)
import Fine.Syntax.Abstract (Block (..), Expr (..), apply, boundVars, flattenApp, flattenFun)
import Fine.Syntax.Common (Ext (Ext), HasRange (range), Id (Id), Lit (Bool), Range (InvalidRange))

replaceBlockVar :: (Id, Id) -> Block -> Block
replaceBlockVar vars (Return expr) = Return (replaceVar vars expr)
replaceBlockVar vars (Do expr block) = Do (replaceVar vars expr) (replaceBlockVar vars block)
replaceBlockVar vars@(old, _) (Let isMut binder typ expr block) =
  let expr' = replaceVar vars expr
      block' = if binder /= old then replaceBlockVar vars block else block
   in Let isMut binder typ expr' block'
replaceBlockVar _ Void = Void
replaceBlockVar vars (Loop cond actions block) =
  Loop (replaceVar vars cond) (replaceBlockVar vars actions) (replaceBlockVar vars block)

replaceVar :: (Id, Id) -> Expr -> Expr
replaceVar (old, new) expr@(Var var) = if var == old then Var new else expr
replaceVar vars@(old, new) (Mut var expr) =
  let expr' = replaceVar vars expr
   in Mut (if var == old then new else var) expr'
replaceVar vars@(old, _) (PatternMatch expr matches r) =
  let expr' = replaceVar vars expr
      matches' =
        fmap
          (\(patt, cont) -> (patt, if old `elem` boundVars patt then cont else replaceVar vars cont))
          matches
   in PatternMatch expr' matches' r
replaceVar vars@(old, _) expr@(Fun param body) =
  if old == param then expr else Fun param (replaceVar vars body)
replaceVar vars (Block block r) = Block (replaceBlockVar vars block) r
replaceVar vars other = apply (replaceVar vars) other

data TransformCtx = TransformCtx
  { fBinder :: Id,
    fParams :: (NonEmpty Id)
  }

type RM t = ReaderT TransformCtx Maybe t

tryTransformBlock :: (Expr -> RM Expr) -> Block -> RM Block
tryTransformBlock f (Return expr) = Return <$> f expr
tryTransformBlock f (Do expr block) = Do expr <$> tryTransformBlock f block
tryTransformBlock f (Let isMut binder typ expr block) = do
  isBound <- asks ((binder ==) . fBinder)
  if isBound
    then lift Nothing
    else Let isMut binder typ expr <$> tryTransformBlock f block
tryTransformBlock _ Void = lift Nothing
tryTransformBlock f (Loop cond actions block) = Loop cond actions <$> tryTransformBlock f block

tryTransformRecBranch :: Expr -> RM Expr
tryTransformRecBranch app@(App _ _) = do
  binder <- asks fBinder
  params <- asks fParams
  case flattenApp app of
    Just (Var name, args) | name == binder && length args == length params -> do
      let muts = NEL.zipWith (\(Id name' r') arg -> Mut (Id (cons '$' name') r') arg) params args
      let block = foldr (\mut block' -> Do mut block') Void muts
      return (Block block (range app))
    _ -> lift Nothing
tryTransformRecBranch (Block block r) = do
  block' <- tryTransformBlock (tryTransformRecBranch) block
  return (Block block' r)
tryTransformRecBranch _ = lift Nothing

resultVar :: Id
resultVar = Id "$$result" InvalidRange

nonstopVar :: Id
nonstopVar = Id "$$nonstop" InvalidRange

stop :: Expr
stop = Mut nonstopVar (Literal (Bool False) InvalidRange)

transformNonRecBranch :: Expr -> Reader TransformCtx Expr
transformNonRecBranch expr =
  let setResult = Mut resultVar expr
      block = Do setResult $ Do stop $ Void
   in return (Block block InvalidRange)

tryTransformBranches :: Expr -> RM Expr
tryTransformBranches (PatternMatch expr' matches r) = do
  ctx <- ask
  let (patts, branches) = F.unzip matches
  let recsTransformed = fmap (\expr -> runReaderT (tryTransformRecBranch expr) ctx) branches
  if null (catMaybes $ NEL.toList recsTransformed)
    then lift Nothing
    else
      let allTransformed =
            NEL.zipWith
              (\expr optExpr -> fromMaybe (runReader (transformNonRecBranch expr) ctx) optExpr)
              branches
              recsTransformed
       in return (PatternMatch expr' (NEL.zip patts allTransformed) r)
tryTransformBranches (Cond cond yes no r) = do
  ctx <- ask
  let branches = [yes, no]
  let recsTransformed = map (\expr -> runReaderT (tryTransformRecBranch expr) ctx) branches
  if null (catMaybes recsTransformed)
    then lift Nothing
    else
      let allTransformed =
            zipWith
              (\expr optExpr -> fromMaybe (runReader (transformNonRecBranch expr) ctx) optExpr)
              branches
              recsTransformed
       in return (Cond cond (allTransformed !! 0) (allTransformed !! 1) r)
tryTransformBranches (Block block r) = do
  block' <- tryTransformBlock tryTransformBranches block
  return (Block block' r)
tryTransformBranches _ = lift Nothing

optimize :: Id -> Expr -> Maybe Expr
optimize binder fun@(Fun _ _) = do
  let (body, params) = fromJust (flattenFun fun)
  body' <- runReaderT (tryTransformBranches body) (TransformCtx binder params)
  let varSubstts = NEL.map (\old@(Id name r) -> (old, Id (cons '$' name) r)) params
  let body'' = foldr replaceVar body' varSubstts
  let retResult = Return (Var resultVar)
  let loop = Loop (Var nonstopVar) (Do body'' Void) retResult
  let letNonStop = Let True nonstopVar () (Literal (Bool True) InvalidRange) loop
  let letResult = Let True resultVar () (ExtExpr $ Ext "null" InvalidRange) letNonStop
  let block = foldr (\(old, new) block' -> Let True new () (Var old) block') letResult varSubstts
  return (foldr Fun (Block block InvalidRange) params)
optimize _ _ = Nothing
