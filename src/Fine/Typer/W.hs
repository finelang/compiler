module Fine.Typer.W (
  boolType,
  litType,
) where

import Fine.Syntax (
  Kind (KLit),
  Lit (..),
  LitT (..),
  Phase (Typed),
  Range,
  Type (LiteralT),
 )

boolType :: Range -> Type Typed
boolType r = LiteralT (r, KLit r) BoolT

litType :: Range -> Lit -> Type Typed
litType r (Bool _) = boolType r
litType r (Str _) = LiteralT (r, KLit r) StrT
litType r (Int _) = LiteralT (r, KLit r) IntT
litType r (Float _) = LiteralT (r, KLit r) FloatT
litType r Unit = LiteralT (r, KLit r) UnitT
