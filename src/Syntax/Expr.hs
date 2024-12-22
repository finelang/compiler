module Syntax.Expr (Expr (..), Closure (..), Module (..), Fixities) where

import Data.Map (Map)
import Syntax.Common (Bind, Fixity, HasRange (..), Range, Var (Var))

data Expr
  = Int Int Range
  | Float Float Range
  | Id Var
  | App Expr [Expr] Range
  | Fun [Var] Expr Range
  | Parens Expr
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Id (Var _ r)) = r
  getRange (App _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Parens expr) = getRange expr

data Closure ctx v = Closure
  { closureVars :: ctx,
    closureValue :: v
  }
  deriving (Show)

type Fixities = Map Var Fixity

data Module = Module
  { bindings :: [Bind () (Closure [Var] Expr)],
    fixities :: Fixities
  }
  deriving (Show)
