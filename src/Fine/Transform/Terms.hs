module Fine.Transform.Terms (transformType, runExprTransformer) where

import Control.Monad.Trans.RW (RW, asks, runRW, tell, withReader)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Fine.Error (
  Errors,
  Warning (DebugKeywordUsage),
  collectWarning,
 )
import Fine.Syntax (
  Block (..),
  Expr (..),
  Pass (Parsed, Transformed),
  Pattern (..),
  Type (..),
  range,
 )
import Fine.Transform.Common (Constructors, Fixities)
import Fine.Transform.ShuntingYard (runShuntingYard)

-- TYPE

transformType :: Type Parsed -> Type Transformed
transformType (LiteralT ext lit) = LiteralT ext lit
transformType (TupleT ext types) = TupleT ext (NonEmpty.map transformType types)
transformType (RecordT ext propTypes) =
  RecordT ext $ (NonEmpty.map . fmap) transformType propTypes
transformType (FunT ext argt bodyt) = FunT ext (transformType argt) (transformType bodyt)
transformType (Forall ext var type') = Forall ext var (transformType type')
transformType (TData ext tag types) = TData ext tag (map transformType types)
transformType (TVar ext var) = TVar ext var
transformType (TApp ext tfun targ) = TApp ext (transformType tfun) (transformType targ)
transformType (TFun ext param type') = TFun ext param (transformType type')

-- EXPR

data Ctx = Ctx
  { fixities :: Fixities,
    constructors :: Constructors
  }

transformBlock :: Block Parsed -> RW Ctx Errors (Block Transformed)
transformBlock (Return expr) = Return <$> transformExpr expr
transformBlock (Do action block) =
  Do <$> transformExpr action <*> transformBlock block
transformBlock (Let isMut binder value block) =
  Let isMut binder <$> transformExpr value <*> transformBlock block
transformBlock (Loop cond actions block) =
  Loop <$> transformExpr cond <*> transformBlock actions <*> transformBlock block

transformPatt :: Pattern -> RW Constructors Errors Pattern
transformPatt patt@(LiteralP _ _) = return patt
transformPatt (DataP r tag patts) = do
  isNotCt <- asks (Set.notMember tag)
  if null patts && isNotCt
    then return (Capture r tag)
    else DataP r tag <$> mapM transformPatt patts
transformPatt (RecordP r props) = RecordP r <$> (mapM . mapM) transformPatt props
transformPatt (TupleP r patts) = TupleP r <$> mapM transformPatt patts
transformPatt patt@(Capture _ _) = return patt
transformPatt patt@(Discard _) = return patt

transformExpr :: Expr Parsed -> RW Ctx Errors (Expr Transformed)
transformExpr (Literal ext lit) = return (Literal ext lit)
transformExpr (Data ext tag exprs) = Data ext tag <$> mapM transformExpr exprs
transformExpr (Record ext props) = Record ext <$> (mapM . mapM) transformExpr props
transformExpr (Tuple ext exprs) = Tuple ext <$> mapM transformExpr exprs
transformExpr (Var ext var) = return (Var ext var)
transformExpr (Mut ext var expr) = Mut ext var <$> transformExpr expr
transformExpr (App ext f arg) = App ext <$> transformExpr f <*> transformExpr arg
transformExpr (Access ext expr prop) = do
  expr' <- transformExpr expr
  return (Access ext expr' prop)
transformExpr (Index ext expr ix) = do
  expr' <- transformExpr expr
  return (Index ext expr' ix)
transformExpr (Cond ext cond yes no) =
  Cond ext <$> transformExpr cond <*> transformExpr yes <*> transformExpr no
transformExpr (PatternMatch ext expr matches) = do
  expr' <- transformExpr expr
  patts' <- withReader constructors (mapM (transformPatt . fst) matches)
  conts' <- mapM (transformExpr . snd) matches
  return (PatternMatch ext expr' (NonEmpty.zip patts' conts'))
transformExpr (Fun ext param body) = Fun ext param <$> transformExpr body
transformExpr (Block ext block') = Block ext <$> transformBlock block'
transformExpr debg@(Debug ext expr) = do
  tell (collectWarning $ DebugKeywordUsage $ range debg)
  Debug ext <$> transformExpr expr
transformExpr (Chain _ _ chain) = do
  ctx <- asks fixities
  let (expr, errors) = runShuntingYard ctx chain
  tell errors
  transformExpr expr
transformExpr (External ext params body) = return (External ext params body)

runExprTransformer :: Fixities -> Constructors -> Expr Parsed -> (Expr Transformed, Errors)
runExprTransformer fixs cts expr = runRW (transformExpr expr) (Ctx fixs cts)
