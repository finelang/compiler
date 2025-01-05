module Fine.Syntax.Pattern (Pattern (..), boundVars) where

import Data.Text (Text)
import Fine.Syntax.Common (Data (Data), HasRange (..), Range, Var (Var))

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

dataBoundVars :: Data Pattern -> [Var]
dataBoundVars (Data members) = concat $ map (boundVars . snd) members

boundVars :: Pattern -> [Var]
boundVars (Int _ _) = []
boundVars (Float _ _) = []
boundVars (Str _ _) = []
boundVars (Unit _) = []
boundVars (Obj d _) = dataBoundVars d
boundVars (Variant _ d _) = dataBoundVars d
boundVars (Tuple fst' snd' rest _) = concat $ map boundVars (fst' : snd' : rest)
boundVars (Capture var) = [var]
