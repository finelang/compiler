{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}

module AST (Expr (..)) where

import Data.Text (Text)
import Error (HasMetadata (metadata), Metadata)

data Expr
  = Int Text Metadata
  | Id Text Metadata
  | App Expr [Expr] Metadata
  | Fun [Text] Expr Metadata
  | Parens Expr Metadata
  deriving (Show)

instance HasMetadata Expr where
  metadata :: Expr -> Metadata
  metadata (Int _ m) = m
  metadata (Id _ m) = m
  metadata (App _ _ m) = m
  metadata (Fun _ _ m) = m
  metadata (Parens _ m) = m
