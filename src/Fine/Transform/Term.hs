module Fine.Transform.Term (transformType, runExprTransformer) where

import Control.Monad.Trans.State.Strict (gets, modify, runState)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Fine.Error (Error)
import Fine.Syntax (
  Block (..),
  Equation (..),
  Expr (..),
  Phase (Parsed, Transformed),
  Range,
  Type (..),
 )
import Fine.Syntax.Name (irrelevant, param)
import Fine.Transform.ShuntingYard (runShuntingYard)

transformType :: Type Parsed -> Type Transformed
transformType (LiteralT r lit) = LiteralT r lit
transformType (VoidT r) = VoidT r
transformType (TupleT r fst' snd' rest) =
  TupleT r (transformType fst') (transformType snd') (map transformType rest)
transformType (ListT r type') = ListT r (transformType type')
transformType (RecordT r propTypes) =
  RecordT r $ (map . fmap) transformType propTypes
transformType (FunT r argTypes bodyType) =
  FunT r (NonEmpty.map transformType argTypes) (transformType bodyType)
transformType (Forall r univars type') = Forall r univars (transformType type')
transformType (TData r tag types) = TData r tag (map transformType types)
transformType (TVar r var) = TVar r var
transformType (TApp r typeFun typeArgs) =
  TApp r (transformType typeFun) (NonEmpty.map transformType typeArgs)
transformType (TFun r typeParams typeBody) = TFun r typeParams (transformType typeBody)

transformEquation :: Equation (Expr Parsed) -> Writer [Error] (Expr Transformed)
transformEquation equation' = do
  equation'' <- go equation'
  let (expr', errs) = runShuntingYard equation''
  tell errs
  return expr'
 where
  go (Operand expr) = Operand <$> transformExpr expr
  go (Operation left op equation) =
    Operation <$> transformExpr left <*> return op <*> go equation

transformPartialEquation ::
  Range -> Equation (Either Range (Expr Parsed)) -> Writer [Error] (Expr Transformed)
transformPartialEquation r' equation' = do
  let (equation'', (_, params)) = runState (go equation') (0 :: Int, [])
  body <- transformEquation equation''
  return $ case reverse params of
    [] -> Fun r' (irrelevant :| []) body
    (p : ps) -> Fun r' (p :| ps) body
 where
  go (Operand (Right expr)) = return (Operand expr)
  go (Operand (Left r)) = do
    var <- newParam r
    return (Operand (Var r var))
  go (Operation (Right expr) op equation) = Operation expr op <$> go equation
  go (Operation (Left r) op equation) = do
    var <- newParam r
    Operation (Var r var) op <$> go equation
  newParam r = do
    n <- gets fst
    let p = param r n
    modify $ \(_, ps) -> (n + 1, p : ps)
    return p

transformBlock :: Block Parsed -> Writer [Error] (Block Transformed)
transformBlock (Return expr) = Return <$> transformExpr expr
transformBlock Void = return Void
transformBlock (Do action block) =
  Do <$> transformExpr action <*> transformBlock block
transformBlock (Mut var expr block) =
  Mut var <$> transformExpr expr <*> transformBlock block
transformBlock (Debug expr block) =
  Debug <$> transformExpr expr <*> transformBlock block
transformBlock (Let isMut binder value block) =
  Let isMut binder <$> transformExpr value <*> transformBlock block
transformBlock (Loop cond actions block) =
  Loop <$> transformExpr cond <*> transformBlock actions <*> transformBlock block
transformBlock (LetPatt _ pattern value block) =
  LetPatt () pattern <$> transformExpr value <*> transformBlock block

transformExpr :: Expr Parsed -> Writer [Error] (Expr Transformed)
transformExpr (Literal r lit) = return (Literal r lit)
transformExpr (Data r tag exprs) = Data r tag <$> mapM transformExpr exprs
transformExpr (Record r props) = Record r <$> (mapM . mapM) transformExpr props
transformExpr (Tuple r fst' snd' rest) =
  Tuple r <$> transformExpr fst' <*> transformExpr snd' <*> mapM transformExpr rest
transformExpr (List r exprs) = List r <$> mapM transformExpr exprs
transformExpr (Var r var) = return (Var r var)
transformExpr (App r f args) = App r <$> transformExpr f <*> mapM transformExpr args
transformExpr (GenApp r f types) =
  GenApp r <$> transformExpr f <*> return (NonEmpty.map transformType types)
transformExpr (Access r expr' prop) = Access r <$> transformExpr expr' <*> return prop
transformExpr (Index r expr' ix) = Index r <$> transformExpr expr' <*> return ix
transformExpr (Cond r cond yes no) =
  Cond r <$> transformExpr cond <*> transformExpr yes <*> transformExpr no
transformExpr (Fun r params body) = Fun r params <$> transformExpr body
transformExpr (Block r block) = Block r <$> transformBlock block
transformExpr (PatternMatching r _ matched matches) =
  PatternMatching r () <$> transformExpr matched <*> (mapM . mapM) transformExpr matches
transformExpr (Equation _ _ equation) = transformEquation equation
transformExpr (PartialEquation r _ equation) = transformPartialEquation r equation

runExprTransformer :: Expr Parsed -> (Expr Transformed, [Error])
runExprTransformer expr = runWriter (transformExpr expr)
