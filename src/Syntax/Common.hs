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

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq, Show)

data Fixity = Fixity Assoc Nat
  deriving (Show)

data Operator
  = Operator Text Range
  deriving (Show)

-- left-recursive operation chain to leverage left-recursive parsing
data OpChain' t
  = Operand' t
  | Operation' (OpChain' t) Operator t
  deriving (Show)

-- right-recursive operation chain for shunting yard algorithm
data OpChain t
  = Operand t
  | Operation t Operator (OpChain t)
  deriving (Show)

extendChain :: OpChain t -> Operator -> t -> OpChain t
extendChain (Operand left) op right = Operation left op (Operand right)
extendChain (Operation left firstOp chain) op right = Operation left firstOp (extendChain chain op right)

fromLRChain :: OpChain' t -> OpChain t
fromLRChain (Operand' expr) = Operand expr
fromLRChain (Operation' chain op right) = extendChain (fromLRChain chain) op right

instance (HasRange t) => HasRange (OpChain t) where
  getRange :: OpChain t -> Range
  getRange (Operand expr) = getRange expr
  getRange (Operation l _ chain) = getRange (l, chain)
