module Fine.Typer (runTyper) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Fine.Error (Error, Warning)
import Fine.Syntax (
  Bind (..),
  BindType (..),
  Block (..),
  Expr (..),
  Kind (KLit),
  LitT (UnitT),
  Module (Module),
  Phase (Transformed, Typed),
  Range (NoRange),
  Type (..),
 )

tempKind :: Kind
tempKind = KLit

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
  Forall (r, tempKind) (NonEmpty.map (\v -> (v, tempKind)) univars) (kinded type')
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

kindedTypeBind :: Bind OfType Transformed -> Bind OfType Typed
kindedTypeBind (TypeBind binder type') = TypeBind binder (kinded type')

typedExprBind :: Bind OfExpr Transformed -> Bind OfExpr Typed
typedExprBind (ExprBind binder type' expr) = ExprBind binder (kinded type') (typedExpr expr)
typedExprBind (ForeignBind binder type' code) = ForeignBind binder (kinded type') code

runTyper :: Module Transformed -> (Either (NonEmpty Error) (Module Typed), [Warning])
runTyper (Module values types entry) =
  let mdule = Module (map typedExprBind values) (map kindedTypeBind types) (fmap typedExpr entry)
   in (Right mdule, [])
