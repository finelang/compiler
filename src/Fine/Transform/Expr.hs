module Fine.Transform.Expr (runExprTransformer) where

import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.Errors (Errors (Errors), warning)
import Data.List.NonEmpty qualified as NonEmpty
import Fine.Error (Error, Warning (DebugKeywordUsage))
import Fine.Syntax (
  Block (..),
  Expr (..),
  Phase (Parsed, Transformed),
  range,
 )
import Fine.Transform.Type (transformType)

type Errors' = Errors Error Warning

transformBlock :: Block Parsed -> Writer Errors' (Block Transformed)
transformBlock (Return expr) = Return <$> transformExpr expr
transformBlock Void = return Void
transformBlock (Do action block) =
  Do <$> transformExpr action <*> transformBlock block
transformBlock (Mut var expr block) =
  Mut var <$> transformExpr expr <*> transformBlock block
transformBlock (Debug expr block) =
  (tell $ warning $ DebugKeywordUsage $ range expr)
    >> Debug <$> transformExpr expr <*> transformBlock block
transformBlock (Let isMut binder value block) =
  Let isMut binder <$> transformExpr value <*> transformBlock block
transformBlock (Loop cond actions block) =
  Loop <$> transformExpr cond <*> transformBlock actions <*> transformBlock block
transformBlock (LetPatt _ patt expr block) =
  LetPatt () patt <$> transformExpr expr <*> transformBlock block

transformExpr :: Expr Parsed -> Writer Errors' (Expr Transformed)
transformExpr (Literal ext lit) = return (Literal ext lit)
transformExpr (Data ext tag exprs) = Data ext tag <$> mapM transformExpr exprs
transformExpr (Record ext props) = Record ext <$> (mapM . mapM) transformExpr props
transformExpr (Tuple ext exprs) = Tuple ext <$> mapM transformExpr exprs
transformExpr (Var ext var) = return (Var ext var)
transformExpr (Bin ext op left right) =
  Bin ext op <$> transformExpr left <*> transformExpr right
transformExpr (App ext f args) = App ext <$> transformExpr f <*> mapM transformExpr args
transformExpr (GenApp ext f typeArgs) = do
  f' <- transformExpr f
  let typeArgs' = NonEmpty.map transformType typeArgs
  return (GenApp ext f' typeArgs')
transformExpr (Access ext expr prop) = do
  expr' <- transformExpr expr
  return (Access ext expr' prop)
transformExpr (Index ext expr ix) = do
  expr' <- transformExpr expr
  return (Index ext expr' ix)
transformExpr (Cond ext cond yes no) =
  Cond ext <$> transformExpr cond <*> transformExpr yes <*> transformExpr no
transformExpr (PatternMatching ext expr matches) = do
  expr' <- transformExpr expr
  let patts = NonEmpty.map fst matches
  conts' <- mapM (transformExpr . snd) matches
  return (PatternMatching ext expr' (NonEmpty.zip patts conts'))
transformExpr (Fun ext params body) = Fun ext params <$> transformExpr body
transformExpr (GenFun ext typeParams body) = GenFun ext typeParams <$> transformExpr body
transformExpr (Block ext block') = Block ext <$> transformBlock block'

runExprTransformer :: Expr Parsed -> (Expr Transformed, [Error], [Warning])
runExprTransformer expr =
  let (expr', Errors errs wrns) = runWriter (transformExpr expr)
   in (expr', errs, wrns)
