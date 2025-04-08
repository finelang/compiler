module Fine.Syntax (
  Range (..),
  HasRange (..),
  Id (..),
  Pass (..),
  Kind (..),
  LitT (..),
  Type (..),
  typeExt,
  Chain (..),
  Lit (..),
  Block (..),
  Expr (..),
  exprExt,
  Pattern (..),
  TypeOfBind (..),
  Bind (..),
  Assoc (..),
  Fixity (..),
  Defn (..),
  ParsedModule (..),
  Module (..),
)
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.String.Interpolate (i)
import Data.Text (Text, unpack)
import Data.Void (Void)

-- RANGE

data Range
  = NoRange -- for AST created out of nothing
  | Range
      Int -- start index
      Int -- start column
      Int -- start line
      Int -- end index
      Int -- end column
      Int -- end line

instance Show Range where
  show :: Range -> String
  show NoRange = "[,)"
  show (Range si _ _ ei _ _) = [i|[#{si}, #{ei})|]

instance Semigroup Range where
  (<>) :: Range -> Range -> Range
  NoRange <> r = r
  r <> NoRange = r
  (Range si sc sl _ _ _) <> (Range _ _ _ ei ec el) = Range si sc sl ei ec el

class HasRange t where
  range :: t -> Range

-- IDENTIFIER

data Id = Id Range Text

instance Eq Id where
  (==) :: Id -> Id -> Bool
  (Id _ x) == (Id _ y) = x == y

instance Ord Id where
  compare :: Id -> Id -> Ordering
  compare (Id _ x) (Id _ y) = compare x y

instance HasRange Id where
  range :: Id -> Range
  range (Id r _) = r

instance Show Id where
  show :: Id -> String
  show (Id _ name) = unpack name

-- PASS

data Pass = Parsed | Transformed | Typed

-- KIND

data Kind
  = KLit Range
  | FunK Kind Kind

instance HasRange Kind where
  range :: Kind -> Range
  range (KLit r) = r
  range (FunK left right) = range left <> range right

-- TYPE

data LitT = IntT | FloatT | BoolT | StrT | UnitT

type family TypeX (p :: Pass) where
  TypeX Typed = (Range, Kind)
  TypeX _ = Range

data Type (p :: Pass)
  = LiteralT (TypeX p) LitT
  | TupleT (TypeX p) (NonEmpty (Type p))
  | RecordT (TypeX p) (NonEmpty (Id, Type p))
  | FunT (TypeX p) (Type p) (Type p) -- type of a function from value -> value
  | Forall (TypeX p) (Id, Kind) (Type p) -- type of function from type -> value
  | TData (TypeX p) Id [Type p]
  | TVar (TypeX p) Id
  | TApp (TypeX p) (Type p) (Type p)
  | TFun (TypeX p) Id (Type p) -- type function (for type constructors and type aliases)

typeExt :: Type p -> TypeX p
typeExt (LiteralT ext _) = ext
typeExt (TupleT ext _) = ext
typeExt (RecordT ext _) = ext
typeExt (FunT ext _ _) = ext
typeExt (Forall ext _ _) = ext
typeExt (TData ext _ _) = ext
typeExt (TVar ext _) = ext
typeExt (TApp ext _ _) = ext
typeExt (TFun ext _ _) = ext

instance HasRange (Type Parsed) where
  range :: Type Parsed -> Range
  range = typeExt

-- EXPR

data Chain
  = Operand (Expr Parsed)
  | Operation (Expr Parsed) Id Chain

instance HasRange Chain where
  range :: Chain -> Range
  range (Operand expr) = range expr
  range (Operation expr _ chain) = range expr <> range chain

data Lit
  = Int Int
  | Float Float
  | Bool Bool
  | Str Text
  | Unit

type family VoidX (p :: Pass) where
  VoidX Parsed = Void
  VoidX _ = ()

data Block (p :: Pass)
  = Return (Expr p)
  | Do (Expr p) (Block p)
  | Let Bool Id (Expr p) (Block p)
  | Loop (Expr p) (Block p) (Block p)
  | Void (VoidX p)

type family ExprX (p :: Pass) where
  ExprX Typed = (Range, Type Typed)
  ExprX _ = Range

type family CrtExprX (p :: Pass) where
  CrtExprX Parsed = ()
  CrtExprX _ = Void

data Expr (p :: Pass)
  = Literal (ExprX p) Lit
  | Data (ExprX p) Id [Expr p]
  | Record (ExprX p) (NonEmpty (Id, Expr p))
  | Tuple (ExprX p) (NonEmpty (Expr p))
  | Var (ExprX p) Id
  | Mut (ExprX p) Id (Expr p)
  | App (ExprX p) (Expr p) (Expr p)
  | Access (ExprX p) (Expr p) Id
  | Index (ExprX p) (Expr p) Int
  | Cond (ExprX p) (Expr p) (Expr p) (Expr p)
  | Fun (ExprX p) Id (Expr p)
  | Block (ExprX p) (Block p)
  | PatternMatch (ExprX p) (Expr p) (NonEmpty (Pattern, Expr p))
  | Debug (ExprX p) (Expr p)
  | External (ExprX p) [Id] Text
  | -- Concrete only
    Chain (ExprX p) (CrtExprX p) Chain

exprExt :: Expr p -> ExprX p
exprExt (Literal ext _) = ext
exprExt (Data ext _ _) = ext
exprExt (Record ext _) = ext
exprExt (Tuple ext _) = ext
exprExt (Var ext _) = ext
exprExt (Mut ext _ _) = ext
exprExt (App ext _ _) = ext
exprExt (Access ext _ _) = ext
exprExt (Index ext _ _) = ext
exprExt (Cond ext _ _ _) = ext
exprExt (Fun ext _ _) = ext
exprExt (Block ext _) = ext
exprExt (PatternMatch ext _ _) = ext
exprExt (Debug ext _) = ext
exprExt (External ext _ _) = ext
exprExt (Chain ext _ _) = ext

instance HasRange (Expr Parsed) where
  range :: Expr Parsed -> Range
  range = exprExt

-- PATTERN

data Pattern
  = LiteralP Range Lit
  | DataP Range Id [Pattern]
  | RecordP Range (NonEmpty (Id, Pattern))
  | TupleP Range (NonEmpty Pattern)
  | Capture Range Id
  | Discard Range

instance HasRange Pattern where
  range :: Pattern -> Range
  range (LiteralP r _) = r
  range (DataP r _ _) = r
  range (RecordP r _) = r
  range (TupleP r _) = r
  range (Capture r _) = r
  range (Discard r) = r

-- MODULE

data TypeOfBind = OfValue | OfType

type family BoundType (t :: TypeOfBind) (p :: Pass) where
  BoundType OfValue p = Type p
  BoundType OfType _ = Kind

type family BoundValue (t :: TypeOfBind) (p :: Pass) where
  BoundValue OfValue p = Expr p
  BoundValue OfType p = Type p

data Bind (t :: TypeOfBind) (p :: Pass) = Bind
  { binder :: Id,
    boundType :: BoundType t p,
    boundValue :: BoundValue t p
  }

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

data Defn
  = Defn Id (Expr Parsed)
  | TypingDefn Id (Type Parsed)
  | TypeDefn (Bind OfType Parsed)
  | DataDefn (Bind OfType Parsed) (NonEmpty (Bind OfValue Parsed))
  | FixDefn Fixity Id

data ParsedModule
  = ParsedModule [Defn] (Maybe (Expr Parsed))

data Module (p :: Pass)
  = Module
  { moduleValues :: [Bind OfValue p],
    moduleTypes :: [Bind OfType p],
    moduleFixities :: Map Id Fixity,
    moduleEntry :: Maybe (Expr p)
  }
