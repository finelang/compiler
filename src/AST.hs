{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}

module AST (Expr (..), Metadata (..), metadata) where

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
  | Bin Expr Expr Expr  -- left op right
  deriving (Show)

metadata :: Expr -> Metadata
metadata (Int _ m) = m
metadata (Id _ m) = m
metadata (Bin l _ r) = Metadata (startIndex $ metadata l) (endIndex $ metadata r)
