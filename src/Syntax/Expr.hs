module Syntax.Expr (Expr (..)) where

import Data.Text (Text)
import Syntax.Common (Binder, HasRange (..), Range)

data Expr
  = Int Text Range
  | Float Text Range
  | Id Text Range
  | App Expr [Expr] Range
  | Fun [Binder] Expr Range
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Id _ r) = r
  getRange (App _ _ r) = r
  getRange (Fun _ _ r) = r
