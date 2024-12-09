{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}

module AST (Expr (..), OpChain (..)) where

import Data.Text (Text)
import Error (HasRange (getRange), Range (..))

data OpChain
  = Operand Expr
  | Operation OpChain Expr Expr
  deriving (Show)

instance HasRange OpChain where
  getRange :: OpChain -> Range
  getRange (Operand expr) = getRange expr
  getRange (Operation chain _ r) = getRange (chain, r)

data Expr
  = Int Text Range
  | Float Text Range
  | Id Text Range
  | App Expr [Expr] Range
  | Fun [Text] Expr Range -- 3rd: range of 'fn' keyword
  | Parens Expr Range
  | Chain OpChain -- meant to be transformed into a tree of App
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Id _ r) = r
  getRange (App _ _ r) = r
  getRange (Fun _ body r) = getRange (r, body)
  getRange (Parens _ r) = r
  getRange (Chain chain) = getRange chain
