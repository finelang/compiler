module Syntax.Parsed (Defn (..), Expr (..), Module (..), defnBind) where

import Data.List.NonEmpty (NonEmpty)
import Syntax.Common (Bind, Fixity, HasRange (..), OpChain, Range, Var (Var))

data Expr
  = Int Int Range
  | Float Float Range
  | Id Var
  | App Expr [Expr] Range
  | Fun [Var] Expr Range
  | Parens Expr
  | Block (NonEmpty Expr) Range
  | Chain (OpChain Expr)
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Id (Var _ r)) = r
  getRange (App _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Parens expr) = getRange expr
  getRange (Block _ r) = r
  getRange (Chain chain) = getRange chain

data Defn
  = Defn (Bind () Expr)
  | OpDefn (Bind () Expr) Fixity
  deriving (Show)

defnBind :: Defn -> Bind () Expr
defnBind (Defn b) = b
defnBind (OpDefn b _) = b

data Module = Module
  { definitions :: [Defn]
  }
  deriving (Show)
