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
  Phase (Kinded, Typed),
  Range (NoRange),
  Type (..),
 )
import Unsafe.Coerce (unsafeCoerce)

asTyped :: Type Kinded -> Type Typed
asTyped = unsafeCoerce

tempType :: Type Typed
tempType = LiteralT (KLit NoRange) NoRange UnitT

typedBlock :: Block Kinded -> Block Typed
typedBlock (Return expr) = Return $ typedExpr expr
typedBlock Void = Void
typedBlock (Do action block) = Do (typedExpr action) (typedBlock block)
typedBlock (Mut var expr block) =
  Mut var (typedExpr expr) (typedBlock block)
typedBlock (LetMut binder value block) =
  LetMut binder (typedExpr value) (typedBlock block)
typedBlock (Let _ patt expr block) =
  Let () patt (typedExpr expr) (typedBlock block)
typedBlock (Debug r expr block) = Debug r (typedExpr expr) (typedBlock block)
typedBlock (Loop cond actions block) =
  Loop (typedExpr cond) (typedBlock actions) (typedBlock block)

typedExpr :: Expr Kinded -> Expr Typed
typedExpr (Literal _ r lit) = Literal tempType r lit
typedExpr (Data _ tag exprs) = Data tempType tag (map typedExpr exprs)
typedExpr (Record _ r props) = Record tempType r $ (map . fmap) typedExpr props
typedExpr (Tuple _ r fst' snd' rest) =
  Tuple tempType r (typedExpr fst') (typedExpr snd') (map typedExpr rest)
typedExpr (Var _ var) = Var tempType var
typedExpr (Bin _ op left right) = Bin tempType op (typedExpr left) (typedExpr right)
typedExpr (App _ f arg) = App tempType (typedExpr f) (typedExpr arg)
typedExpr (GenApp _ _ fname targs) = GenApp tempType () fname (NonEmpty.map asTyped targs)
typedExpr (Access _ expr prop) = Access tempType (typedExpr expr) prop
typedExpr (Index _ r expr ix) = Index tempType r (typedExpr expr) ix
typedExpr (Cond _ r cond yes no) =
  Cond tempType r (typedExpr cond) (typedExpr yes) (typedExpr no)
typedExpr (PatternMatching _ r _ expr matches) =
  PatternMatching tempType r () (typedExpr expr) $ (NonEmpty.map . fmap) typedExpr matches
typedExpr (Fun _ param body) = Fun tempType param (typedExpr body)
typedExpr (GenFun _ _ _ tparams body) = GenFun tempType () () tparams (typedExpr body)
typedExpr (Block _ r block) = Block tempType r (typedBlock block)

typedExprBind :: Bind OfExpr Kinded -> Bind OfExpr Typed
typedExprBind (ExprBind binder type' expr) = ExprBind binder (asTyped type') (typedExpr expr)
typedExprBind (ForeignBind binder type' code) = ForeignBind binder (asTyped type') code

typedTypeBind :: Bind OfType Kinded -> Bind OfType Typed
typedTypeBind (TypeBind binder type') = TypeBind binder (asTyped type')

typedModule :: Module Kinded -> Module Typed
typedModule (Module exprBinds typeBinds entry) = do
  Module (map typedExprBind exprBinds) (map typedTypeBind typeBinds) (fmap typedExpr entry)

runTyper :: Module Kinded -> (Either (NonEmpty Error) (Module Typed), [Warning])
runTyper mdule = (Right (typedModule mdule), [])
