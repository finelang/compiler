{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}

module AST (Expr (..)) where

import Data.Text (Text)
import Error (HasRange (getRange), Range)

data Expr
  = Int Text Range
  | Id Text Range
  | App Expr [Expr] Range
  | Fun [Text] Expr Range
  | Parens Expr Range
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ m) = m
  getRange (Id _ m) = m
  getRange (App _ _ m) = m
  getRange (Fun _ _ m) = m
  getRange (Parens _ m) = m
