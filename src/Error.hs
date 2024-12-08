{-# LANGUAGE InstanceSigs #-}

module Error (Metadata (..), HasMetadata (..)) where

data Metadata = Metadata
  { startIndex :: Int,
    endIndex :: Int
  }

instance Show Metadata where
  show :: Metadata -> String
  show (Metadata si ei) = "[" ++ show si ++ ", " ++ show ei ++ ")"

class HasMetadata t where
  metadata :: t -> Metadata
