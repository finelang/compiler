{-# LANGUAGE QuasiQuotes #-}

module Syntax.Common (module Syntax.Common) where

import Data.Function (on)
import Data.Map (Map)
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)

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

data Var = Var {varName :: Text, varRange :: Range}

instance Eq Var where
  (==) :: Var -> Var -> Bool
  (==) = (==) `on` varName

instance Ord Var where
  compare :: Var -> Var -> Ordering
  compare = compare `on` varName

instance HasRange Var where
  getRange :: Var -> Range
  getRange = varRange

instance Show Var where
  show :: Var -> String
  show (Var name _) = unpack name

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq)

instance Show Assoc where
  show :: Assoc -> String
  show LeftAssoc = "infixl"
  show RightAssoc = "infixr"
  show NonAssoc = "infix"

data Fixity = Fixity Assoc Int

instance Show Fixity where
  show :: Fixity -> String
  show (Fixity assoc prec) = [i|#{assoc} #{prec}|]

type Fixities = Map Var Fixity

data Bind t v = Bind
  { binder :: Var,
    boundType :: t,
    boundValue :: v
  }
  deriving (Show)

-- left-recursive operation chain to leverage left-recursive parsing
data OpChain' t
  = Operand' t
  | Operation' (OpChain' t) Var t
  deriving (Show)

-- right-recursive operation chain for shunting yard algorithm
data OpChain t
  = Operand t
  | Operation t Var (OpChain t)
  deriving (Show)

extendChain :: OpChain t -> Var -> t -> OpChain t
extendChain (Operand left) op right = Operation left op (Operand right)
extendChain (Operation left firstOp chain) op right = Operation left firstOp (extendChain chain op right)

fromLRChain :: OpChain' t -> OpChain t
fromLRChain (Operand' expr) = Operand expr
fromLRChain (Operation' chain op right) = extendChain (fromLRChain chain) op right

instance (HasRange t) => HasRange (OpChain t) where
  getRange :: OpChain t -> Range
  getRange (Operand expr) = getRange expr
  getRange (Operation l _ chain) = getRange (l, chain)

newtype Data t = Data {dataMembers :: [(Var, t)]}
  deriving (Show)

data Ext = Ext Text Range
  deriving (Show)

instance HasRange Ext where
  getRange :: Ext -> Range
  getRange (Ext _ r) = r

data VariantSpec = VariantSpec
  { variantTag :: Var,
    variantProps :: [Var],
    variantExtValue :: Maybe Ext
  }
  deriving (Show)

type VariantSpecs = Map Var VariantSpec
