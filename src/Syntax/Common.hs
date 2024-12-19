{-# LANGUAGE QuasiQuotes #-}

module Syntax.Common (module Syntax.Common) where

import Data.Function (on)
import Data.Nat (Nat)
import Data.String.Interpolate (i)
import Data.Text (Text)

data Range = Range
  { startIndex :: Int,
    startColumn :: Int,
    startLine :: Int,
    endIndex :: Int,
    endColumn :: Int,
    endLine :: Int
  }

instance Show Range where
  show :: Range -> String
  show range = [i|[#{startIndex range}, #{endIndex range})|]

class HasRange t where
  getRange :: t -> Range

instance HasRange Range where
  getRange :: Range -> Range
  getRange = id

instance (HasRange p, HasRange q) => HasRange (p, q) where
  getRange :: (p, q) -> Range
  getRange (l, r) =
    let (Range si sc sl _ _ _) = getRange l
        (Range _ _ _ ei ec el) = getRange r
     in Range si sc sl ei ec el

data Binder = Binder
  { binderName :: Text,
    binderRange :: Range
  }

instance HasRange Binder where
  getRange :: Binder -> Range
  getRange = binderRange

instance Eq Binder where
  (==) :: Binder -> Binder -> Bool
  (==) = (==) `on` binderName

instance Ord Binder where
  compare :: Binder -> Binder -> Ordering
  compare = compare `on` binderName

instance Show Binder where
  show :: Binder -> String
  show (Binder name _) = show name

data Binding t v
  = Binding Binder t v Range
  deriving (Show)

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq)

instance Show Assoc where
  show :: Assoc -> String
  show LeftAssoc = "infixl"
  show RightAssoc = "infixr"
  show NonAssoc = "infix"

data Fixity = Fixity Assoc Nat

instance Show Fixity where
  show :: Fixity -> String
  show (Fixity assoc prec) = [i|#{assoc} #{prec}|]

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
