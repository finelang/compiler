{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}

module AST
  ( Expr (..),
    OpChain (..),
    Binder (..),
    Type (..),
    Binding (..),
    Module (..),
  )
where

import Data.Text (Text)
import Error (HasRange (getRange), Range (..))

data Binder = Binder
  { binderName :: Text,
    binderRange :: Range
  }

instance Show Binder where
  show :: Binder -> String
  show (Binder name _) = show name

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
  | Fun [Binder] Expr Range -- 3rd: range of 'fn' keyword
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

data Type
  = NoType
  deriving (Show)

data Binding
  = Binding Binder Type Expr Range
  deriving (Show)

data Module
  = Module [Binding]
  deriving (Show)
