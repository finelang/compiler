module Fine.Typer (runTyper) where

import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.RW (RW, runRW, withReader)
import Control.Monad.Writer.Class (tell)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Fine.Error (Error, Warning)
import Fine.Syntax (
  Bind (..),
  BindType (..),
  Block (..),
  Expr (..),
  Id,
  Kind (KLit),
  LitT (UnitT),
  Module (Module),
  Phase (Transformed, Typed),
  Range (NoRange),
  Type (..),
 )
import Fine.Typer.Kinder (runKindChecker, runKindInferrer)

tempKind :: Kind Typed
tempKind = KLit NoRange

kinded :: Type Transformed -> Type Typed
kinded (LiteralT r litT) = LiteralT (r, tempKind) litT
kinded (VoidT r) = VoidT (r, tempKind)
kinded (TupleT r fst' snd' rest) =
  TupleT (r, tempKind) (kinded fst') (kinded snd') (map kinded rest)
kinded (ListT r type') = ListT (r, tempKind) (kinded type')
kinded (RecordT r propTypes) = RecordT (r, tempKind) $ (map . fmap) kinded propTypes
kinded (FunT r argTypes retType) =
  FunT (r, tempKind) (NonEmpty.map kinded argTypes) (kinded retType)
kinded (Forall r univars type') =
  Forall (r, tempKind) univars (kinded type')
kinded (TData r tag types) = TData (r, tempKind) tag (map kinded types)
kinded (TVar r var) = TVar (r, tempKind) var
kinded (TApp r tfun targs) = TApp (r, tempKind) (kinded tfun) (NonEmpty.map kinded targs)
kinded (TFun r tparams tbody) = TFun (r, tempKind) tparams (kinded tbody)

tempType :: Type Typed
tempType = LiteralT (NoRange, tempKind) UnitT

typedBlock :: Block Transformed -> Block Typed
typedBlock (Return expr) = Return $ typedExpr expr
typedBlock Void = Void
typedBlock (Do action block) = Do (typedExpr action) (typedBlock block)
typedBlock (Mut var expr block) =
  Mut var (typedExpr expr) (typedBlock block)
typedBlock (Debug expr block) = Debug (typedExpr expr) (typedBlock block)
typedBlock (Let isMut binder value block) =
  Let isMut binder (typedExpr value) (typedBlock block)
typedBlock (Loop cond actions block) =
  Loop (typedExpr cond) (typedBlock actions) (typedBlock block)
typedBlock (LetPatt _ patt expr block) =
  LetPatt () patt (typedExpr expr) (typedBlock block)

typedExpr :: Expr Transformed -> Expr Typed
typedExpr (Literal r lit) = Literal (r, tempType) lit
typedExpr (Data r tag exprs) = Data (r, tempType) tag (map typedExpr exprs)
typedExpr (Record r props) = Record (r, tempType) $ (map . fmap) typedExpr props
typedExpr (Tuple r fst' snd' rest) =
  Tuple (r, tempType) (typedExpr fst') (typedExpr snd') (map typedExpr rest)
typedExpr (List r exprs) = List (r, tempType) (map typedExpr exprs)
typedExpr (Var r var) = Var (r, tempType) var
typedExpr (Bin r _ op left right) = Bin (r, tempType) () op (typedExpr left) (typedExpr right)
typedExpr (App r f args) = App (r, tempType) (typedExpr f) (NonEmpty.map typedExpr args)
typedExpr (GenApp r f targs) = GenApp (r, tempType) (typedExpr f) (NonEmpty.map kinded targs)
typedExpr (Access r expr prop) = Access (r, tempType) (typedExpr expr) prop
typedExpr (Index r expr ix) = Index (r, tempType) (typedExpr expr) ix
typedExpr (Cond r cond yes no) =
  Cond (r, tempType) (typedExpr cond) (typedExpr yes) (typedExpr no)
typedExpr (PatternMatching r _ expr matches) =
  PatternMatching (r, tempType) () (typedExpr expr) $ (NonEmpty.map . fmap) typedExpr matches
typedExpr (Fun r params body) = Fun (r, tempType) params (typedExpr body)
typedExpr (GenFun r _ tparams body) = GenFun (r, tempType) () tparams (typedExpr body)
typedExpr (Block r block) = Block (r, tempType) (typedBlock block)

typedExprBind :: Bind OfExpr Transformed -> RW (Map Id (Type Typed)) [Error] (Bind OfExpr Typed)
typedExprBind (ExprBind binder type' expr) = do
  typeEnv <- ask
  let (type'', errs) = runKindChecker typeEnv type'
  tell errs
  return $ ExprBind binder type'' (typedExpr expr)
typedExprBind (ForeignBind binder type' code) = do
  typeEnv <- ask
  let (type'', errs) = runKindChecker typeEnv type'
  tell errs
  return $ ForeignBind binder type'' code

collectTypeEnv :: [Bind OfType Transformed] -> Map Id (Type Transformed)
collectTypeEnv binds = Map.fromList $ map (\(TypeBind binder type') -> (binder, type')) binds

typedModule :: Module Transformed -> RW () [Error] (Module Typed)
typedModule (Module exprBinds typeBinds entry) = do
  let typeEnv = collectTypeEnv typeBinds
  let (kindedTypes, errs) = runKindInferrer typeEnv
  tell errs
  let typeBinds' = (flip map) typeBinds $
        \(TypeBind binder _) -> TypeBind binder (kindedTypes ! binder)
  exprBinds' <- withReader (const kindedTypes) (mapM typedExprBind exprBinds)
  return $ Module exprBinds' typeBinds' (fmap typedExpr entry)

runTyper :: Module Transformed -> (Either (NonEmpty Error) (Module Typed), [Warning])
runTyper mdule =
  let (mdule', errs) = runRW (typedModule mdule) ()
      result = case errs of
        err : errs' -> Left (err :| errs')
        [] -> Right mdule'
   in (result, [])
