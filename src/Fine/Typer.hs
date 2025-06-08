module Fine.Typer (runTyper) where

import Control.Monad.Errors (Errors)
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
tempType = LiteralT (NoRange, KLit NoRange) UnitT

typedBlock :: Block Kinded -> Block Typed
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

typedExpr :: Expr Kinded -> Expr Typed
typedExpr (Literal r lit) = Literal (r, tempType) lit
typedExpr (Data r tag exprs) = Data (r, tempType) tag (map typedExpr exprs)
typedExpr (Record r props) = Record (r, tempType) $ (map . fmap) typedExpr props
typedExpr (Tuple r fst' snd' rest) =
  Tuple (r, tempType) (typedExpr fst') (typedExpr snd') (map typedExpr rest)
typedExpr (List r exprs) = List (r, tempType) (map typedExpr exprs)
typedExpr (Var r var) = Var (r, tempType) var
typedExpr (Bin r _ op left right) = Bin (r, tempType) () op (typedExpr left) (typedExpr right)
typedExpr (App r f args) = App (r, tempType) (typedExpr f) (NonEmpty.map typedExpr args)
typedExpr (GenApp r f targs) = GenApp (r, tempType) (typedExpr f) (NonEmpty.map asTyped targs)
typedExpr (Access r expr prop) = Access (r, tempType) (typedExpr expr) prop
typedExpr (Index r expr ix) = Index (r, tempType) (typedExpr expr) ix
typedExpr (Cond r cond yes no) =
  Cond (r, tempType) (typedExpr cond) (typedExpr yes) (typedExpr no)
typedExpr (PatternMatching r _ expr matches) =
  PatternMatching (r, tempType) () (typedExpr expr) $ (NonEmpty.map . fmap) typedExpr matches
typedExpr (Fun r params body) = Fun (r, tempType) params (typedExpr body)
typedExpr (GenFun r _ tparams body) = GenFun (r, tempType) () tparams (typedExpr body)
typedExpr (Block r block) = Block (r, tempType) (typedBlock block)

typedExprBind :: Bind OfExpr Kinded -> Bind OfExpr Typed
typedExprBind (ExprBind binder type' expr) = ExprBind binder (asTyped type') (typedExpr expr)
typedExprBind (ForeignBind binder type' code) = ForeignBind binder (asTyped type') code

typedTypeBind :: Bind OfType Kinded -> Bind OfType Typed
typedTypeBind (TypeBind binder type') = TypeBind binder (asTyped type')

typedModule :: Module Kinded -> Module Typed
typedModule (Module exprBinds typeBinds entry) = do
  Module (map typedExprBind exprBinds) (map typedTypeBind typeBinds) (fmap typedExpr entry)

runTyper :: Module Kinded -> Errors Error (Module Typed, [Warning])
runTyper mdule = return (typedModule mdule, [])
