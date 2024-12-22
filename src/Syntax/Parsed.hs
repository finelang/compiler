module Syntax.Parsed (Defn (..), Expr (..), Module (..)) where

import Syntax.Common (Bind, HasRange (..), OpChain, Range, Var (Var))

data Expr
  = Int Int Range
  | Float Float Range
  | Id Var
  | App Expr [Expr] Range
  | Fun [Var] Expr Range
  | Parens Expr Range
  | Chain (OpChain Expr)
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Id (Var _ r)) = r
  getRange (App _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Parens _ r) = r
  getRange (Chain chain) = getRange chain

data Defn
  = BindDefn (Bind () Expr)
  deriving (Show)

data Module = Module
  { definitions :: [Defn]
  }
  deriving (Show)
