module Fine.Syntax.Pattern (Pattern (..)) where

import Data.Text (Text)
import Fine.Syntax.Common (Data, HasRange (..), Range, Var (Var))

data Pattern
  = Int Int Range
  | Float Float Range
  | Str Text Range
  | Unit Range
  | Obj (Data Pattern) Range
  | Variant Var (Data Pattern) Range
  | Tuple Pattern Pattern [Pattern] Range
  | Capture Var
  deriving (Show)

instance HasRange Pattern where
  getRange :: Pattern -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Str _ r) = r
  getRange (Unit r) = r
  getRange (Obj _ r) = r
  getRange (Variant _ _ r) = getRange r
  getRange (Tuple _ _ _ r) = r
  getRange (Capture (Var _ r)) = r
