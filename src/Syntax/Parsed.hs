module Syntax.Parsed (Expr (..), Module (..)) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Syntax.Common (Binder, Binding, HasRange (..), OpChain, Range)

data Expr
  = Int Int Range
  | Float Float Range
  | Var Text Range
  | App Expr [Expr] Range
  | Fun [Binder] Expr Range
  | Parens Expr Range
  | Chain (OpChain Expr)
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Var _ r) = r
  getRange (App _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Parens _ r) = r
  getRange (Chain chain) = getRange chain

data Module = Module
  { bindings :: NonEmpty (Binding () Expr)
  }
  deriving (Show)
