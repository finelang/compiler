{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}

module AST (Expr (..), Metadata (..)) where

import Data.Text (Text)

data Metadata = Metadata
  { startIndex :: Int,
    endIndex :: Int
  }

instance Show Metadata where
  show :: Metadata -> String
  show (Metadata si ei) = "[" ++ show si ++ ", " ++ show ei ++ ")"

data Expr
  = Int Text Metadata
  | Id Text Metadata
  | Bin Expr Text Expr
  deriving (Show)
