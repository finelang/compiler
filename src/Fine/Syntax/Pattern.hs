module Fine.Syntax.Pattern (Pattern (..), boundVars) where

import Data.Text (Text)
import Fine.Syntax.Common (HasRange (..), Prop (..), Range, Var (Var))

data Pattern
  = Int Int Range
  | Float Float Range
  | Str Text Range
  | Unit Range
  | Obj [Prop Pattern] Range
  | Variant Var [Prop Pattern] Range
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

propBoundVars :: Prop Pattern -> [Var]
propBoundVars (NamedProp _ patt) = boundVars patt
propBoundVars (SelfProp name) = [name]
propBoundVars (SpreadProp patt) = case patt of
  (Capture name) -> [name]
  _ -> []

boundVars :: Pattern -> [Var]
boundVars (Int _ _) = []
boundVars (Float _ _) = []
boundVars (Str _ _) = []
boundVars (Unit _) = []
boundVars (Obj props _) = concat (map propBoundVars props)
boundVars (Variant _ props _) = concat (map propBoundVars props)
boundVars (Tuple fst' snd' rest _) = concat $ map boundVars (fst' : snd' : rest)
boundVars (Capture var) = [var]
