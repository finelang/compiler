module Syntax.Common (module Syntax.Common) where

import Data.Nat (Nat)
import Data.Text (Text)

data Range = Range
  { startIndex :: Int,
    endIndex :: Int
  }

instance Show Range where
  show :: Range -> String
  show (Range si ei) = "[" ++ show si ++ ", " ++ show ei ++ ")"

class HasRange t where
  getRange :: t -> Range

instance HasRange Range where
  getRange :: Range -> Range
  getRange = id

instance (HasRange p, HasRange q) => HasRange (p, q) where
  getRange :: (p, q) -> Range
  getRange (l, r) = Range (startIndex $ getRange l) (endIndex $ getRange r)

data Binder = Binder
  { binderName :: Text,
    binderRange :: Range
  }

instance Show Binder where
  show :: Binder -> String
  show (Binder name _) = show name

data Binding t v
  = Binding Binder t v Range
  deriving (Show)

data Fixity
  = LeftAssoc Nat Range
  | RightAssoc Nat Range
  | NonAssoc Nat Range
  deriving (Show)

data Operator
  = Operator Text Range
  deriving (Show)

data OpChain t
  = Operand t
  | Operation (OpChain t) Operator t
  deriving (Show)

instance (HasRange t) => HasRange (OpChain t) where
  getRange :: OpChain t -> Range
  getRange (Operand expr) = getRange expr
  getRange (Operation chain _ r) = getRange (chain, r)
