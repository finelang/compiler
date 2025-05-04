module Fine.Transform.Term (transformType, runExprTransformer) where

import Control.Monad.Trans.RW (RW, runRW, tell, withReader)
import Data.List.NonEmpty qualified as NonEmpty
import Fine.Error (Error)
import Fine.Syntax (
  Block (..),
  Equation (..),
  Expr (..),
  Phase (Parsed, Transformed),
  Type (..),
 )
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

type ParentExpr = Expr Parsed

transformEquation :: Equation Parsed -> RW (Maybe ParentExpr) [Error] (Equation Transformed)
transformEquation (Operand expr) = Operand <$> transformExpr expr
transformEquation (Operation left op equation) = do
  left' <- transformExpr left
  equation' <- transformEquation equation
  return (Operation left' op equation')

transformBlock :: Block Parsed -> RW (Maybe ParentExpr) [Error] (Block Transformed)
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
transformBlock (LetPatt x pattern value block) =
  LetPatt x pattern <$> transformExpr value <*> transformBlock block

transformExpr :: Expr Parsed -> RW (Maybe ParentExpr) [Error] (Expr Transformed)
transformExpr expr = withReader (const $ Just expr) $ case expr of
  Literal r lit -> return (Literal r lit)
  Data r tag exprs -> Data r tag <$> mapM transformExpr exprs
  Record r props -> Record r <$> (mapM . mapM) transformExpr props
  Tuple r fst' snd' rest ->
    Tuple r <$> transformExpr fst' <*> transformExpr snd' <*> mapM transformExpr rest
  List r exprs -> List r <$> mapM transformExpr exprs
  Var r var -> return (Var r var)
  App r f args -> App r <$> transformExpr f <*> mapM transformExpr args
  GenApp r f types ->
    GenApp r <$> transformExpr f <*> return (NonEmpty.map transformType types)
  Access r expr' prop -> Access r <$> transformExpr expr' <*> return prop
  Index r expr' ix -> Index r <$> transformExpr expr' <*> return ix
  Cond r cond yes no ->
    Cond r <$> transformExpr cond <*> transformExpr yes <*> transformExpr no
  Fun r params body -> Fun r params <$> transformExpr body
  GenFun r tparams body -> GenFun r tparams <$> transformExpr body
  Block r block -> Block r <$> transformBlock block
  PatternMatching r matched matches ->
    PatternMatching r <$> transformExpr matched <*> (mapM . mapM) transformExpr matches
  Equation _ equation -> do
    equation' <- transformEquation equation
    let (expr', errs) = runShuntingYard equation'
    tell errs
    return expr'
  Grouping _ expr' -> transformExpr expr'

runExprTransformer :: Expr Parsed -> (Expr Transformed, [Error])
runExprTransformer expr = runRW (transformExpr expr) Nothing
