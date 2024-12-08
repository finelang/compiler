{-# LANGUAGE InstanceSigs #-}

module Error (Range (..), HasRange (..)) where

data Range = Range
  { startIndex :: Int,
    endIndex :: Int
  }

instance Show Range where
  show :: Range -> String
  show (Range si ei) = "[" ++ show si ++ ", " ++ show ei ++ ")"

class HasRange t where
  getRange :: t -> Range
