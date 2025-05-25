module Fine.Typer.J (
  boolType,
  litType,
  runTypeChecker,
) where

import Control.Monad.RWS.Strict (RWS)
import Control.Monad.Reader (Reader)
import Fine.Error (Error, errorTODO)
import Fine.Syntax (
  Expr (..),
  Id,
  Kind (KLit),
  Lit (..),
  LitT (..),
  Phase (PartiallyTyped, Transformed, Typed),
  Range (NoRange),
  Type (..),
 )
import Fine.Typer.Common (Env, SubsttState, Substts, newSubsttVar)

boolType :: Range -> Type Typed
boolType r = LiteralT (r, KLit r) BoolT

litType :: Range -> Lit -> Type Typed
litType r (Bool _) = boolType r
litType r (Str _) = LiteralT (r, KLit r) StrT
litType r (Int _) = LiteralT (r, KLit r) IntT
litType r (Float _) = LiteralT (r, KLit r) FloatT
litType r Unit = LiteralT (r, KLit r) UnitT

type Substts' = Substts (Type PartiallyTyped)

type SubsttState' = SubsttState (Type PartiallyTyped)

type TypeEnv = Env (Type PartiallyTyped)

unify :: Type PartiallyTyped -> Type PartiallyTyped -> RWS r [Error] SubsttState' ()
unify = errorTODO

unifyVar :: Id -> Type PartiallyTyped -> RWS r [Error] SubsttState' ()
unifyVar = errorTODO

newSubsttTVar :: (Monoid w) => Range -> RWS r w SubsttState' (Type PartiallyTyped)
newSubsttTVar r = SubsttTVar (r, KLit NoRange) () <$> newSubsttVar "t" r

infer :: Expr Transformed -> RWS TypeEnv [Error] SubsttState' (Expr PartiallyTyped)
infer = errorTODO

type TypedSubstts = Substts (Type Typed)

resolveUndecidable :: Substts' -> TypedSubstts
resolveUndecidable = errorTODO

complete :: Expr PartiallyTyped -> Reader TypedSubstts (Expr Typed)
complete = errorTODO

runTypeChecker :: Env (Type Typed) -> Type Typed -> Expr Transformed -> Expr Typed
runTypeChecker = errorTODO
