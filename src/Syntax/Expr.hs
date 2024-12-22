module Syntax.Expr (Expr (..), Module (..)) where

import Data.Map (Map)
import Data.Text (Text)
import Syntax.Common (Bind, Fixity, HasRange (..), Range, Var (Var))

data Expr
  = Int Int Range
  | Float Float Range
  | Id Var
  | App Expr [Expr] Range
  | Fun [Var] Expr Range
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Id (Var _ r)) = r
  getRange (App _ _ r) = r
  getRange (Fun _ _ r) = r

data Module = Module
  { bindings :: [Bind () Expr],
    fixities :: Map Text Fixity
  }
  deriving (Show)
