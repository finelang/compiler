module Fine.Syntax.Pattern (Pattern (..), boundVars) where

import Data.Text (Text)
import Fine.Syntax.Common (HasRange (..), Range, Var (Var))

data Pattern
  = Int Int Range
  | Float Float Range
  | Str Text Range
  | Unit Range
  | Obj [(Var, Pattern)] (Maybe Var) Range
  | Variant Var [(Var, Pattern)] (Maybe Var) Range
  | Tuple Pattern Pattern [Pattern] Range
  | Capture Var
  deriving (Show)

instance HasRange Pattern where
  getRange :: Pattern -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Str _ r) = r
  getRange (Unit r) = r
  getRange (Obj _ _ r) = r
  getRange (Variant _ _ _ r) = getRange r
  getRange (Tuple _ _ _ r) = r
  getRange (Capture (Var _ r)) = r

propsBoundVars :: [(Var, Pattern)] -> Maybe Var -> [Var]
propsBoundVars named spread =
  let vars = concat (map (boundVars . snd) named)
   in case spread of
        Nothing -> vars
        Just v -> v : vars

boundVars :: Pattern -> [Var]
boundVars (Int _ _) = []
boundVars (Float _ _) = []
boundVars (Str _ _) = []
boundVars (Unit _) = []
boundVars (Obj named spread _) = propsBoundVars named spread
boundVars (Variant _ named spread _) = propsBoundVars named spread
boundVars (Tuple fst' snd' rest _) = concat $ map boundVars (fst' : snd' : rest)
boundVars (Capture var) = [var]
