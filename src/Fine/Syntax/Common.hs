module Fine.Syntax.Common (module Fine.Syntax.Common) where

import Data.Function (on)
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

data Id = Id {idName :: Text, idRange :: Range}

instance Eq Id where
  (==) :: Id -> Id -> Bool
  (==) = (==) `on` idName

instance Ord Id where
  compare :: Id -> Id -> Ordering
  compare = compare `on` idName

instance HasRange Id where
  getRange :: Id -> Range
  getRange = idRange

instance Show Id where
  show :: Id -> String
  show (Id name _) = unpack name

data Lit
  = Int Int
  | Float Float
  | Bool Bool
  | Str Text
  | Unit

instance Show Lit where
  show :: Lit -> String
  show (Int x) = show x
  show (Float x) = show x
  show (Bool x) = show x
  show (Str x) = show x
  show (Unit) = show ()

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

data Bind t v = Bind
  { binder :: Id,
    boundType :: t,
    boundValue :: v
  }
  deriving (Show)

-- left-recursive operation chain to leverage left-recursive parsing
data OpChain' t
  = Operand' t
  | Operation' (OpChain' t) Id t
  deriving (Show)

-- right-recursive operation chain for shunting yard algorithm
data OpChain t
  = Operand t
  | Operation t Id (OpChain t)
  deriving (Show)

extendChain :: OpChain t -> Id -> t -> OpChain t
extendChain (Operand left) op right = Operation left op (Operand right)
extendChain (Operation left firstOp chain) op right = Operation left firstOp (extendChain chain op right)

fromLRChain :: OpChain' t -> OpChain t
fromLRChain (Operand' expr) = Operand expr
fromLRChain (Operation' chain op right) = extendChain (fromLRChain chain) op right

instance (HasRange t) => HasRange (OpChain t) where
  getRange :: OpChain t -> Range
  getRange (Operand expr) = getRange expr
  getRange (Operation l _ chain) = getRange (l, chain)

data Ext = Ext Text Range
  deriving (Show)

instance HasRange Ext where
  getRange :: Ext -> Range
  getRange (Ext _ r) = r
