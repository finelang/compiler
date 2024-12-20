module Syntax.Expr (Expr (..), Module (..)) where

import Data.Map (Map)
import Data.Text (Text)
import Syntax.Common (Binder, Binding, Fixity, HasRange (..), Range)

data Expr
  = Int Int Range
  | Float Float Range
  | Var Text Range
  | App Expr [Expr] Range
  | Fun [Binder] Expr Range
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Var _ r) = r
  getRange (App _ _ r) = r
  getRange (Fun _ _ r) = r

data Module = Module
  { bindings :: [Binding () Expr],
    fixities :: Map Text Fixity
  }
  deriving (Show)
