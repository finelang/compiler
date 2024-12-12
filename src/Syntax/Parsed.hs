module Syntax.Parsed (Expr (..), OpChain (..), Operator (..)) where

import Data.Text (Text)
import Syntax.Common (Binder, HasRange (..), Range)

data Operator
  = Operator Text Range
  deriving (Show)

data OpChain
  = Operand Expr
  | Operation OpChain Operator Expr
  deriving (Show)

instance HasRange OpChain where
  getRange :: OpChain -> Range
  getRange (Operand expr) = getRange expr
  getRange (Operation chain _ r) = getRange (chain, r)

data Expr
  = Int Int Range
  | Float Float Range
  | Id Text Range
  | App Expr [Expr] Range
  | Fun [Binder] Expr Range
  | Parens Expr Range
  | Chain OpChain
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Id _ r) = r
  getRange (App _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Parens _ r) = r
  getRange (Chain chain) = getRange chain
