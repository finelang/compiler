module Fine.Syntax (
  Range (..),
  HasRange (..),
  Id (Id, Op, idText),
  Phase (..),
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
  BindType (..),
  Bind (..),
  binder,
  Assoc (..),
  Fixity (..),
  Defn (..),
  ParsedModule (..),
  Module (..),
)
where

import Data.Function (on)
import Data.Kind qualified as HsKind
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text

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

data Id
  = Id {idRange :: Range, idText :: Text}
  | Op {idRange :: Range, idText :: Text}

instance Eq Id where
  (==) :: Id -> Id -> Bool
  (==) = (==) `on` idText

instance Ord Id where
  compare :: Id -> Id -> Ordering
  compare = compare `on` idText

instance HasRange Id where
  range :: Id -> Range
  range = idRange

instance Show Id where
  show :: Id -> String
  show = Text.unpack . idText

-- PASS

data Phase
  = Parsed -- after parsing
  | Transformed -- after transformation and semantic checking
  | Typed -- after type checking/inference

-- KIND

data Kind
  = KLit Range
  | FunK (NonEmpty Kind) Kind -- type of type functions

instance HasRange Kind where
  range :: Kind -> Range
  range (KLit r) = r
  range (FunK left right) = range (NonEmpty.head left) <> range right

-- TYPE

data LitT = IntT | FloatT | BoolT | StrT | UnitT

type family TypeX (p :: Phase) where
  TypeX Typed = (Range, Kind)
  TypeX _ = Range

type family UniVar (p :: Phase) where
  UniVar Typed = (Id, Kind)
  UniVar _ = Id

data Type (p :: Phase)
  = LiteralT (TypeX p) LitT
  | VoidT (TypeX p)
  | TupleT (TypeX p) (NonEmpty (Type p))
  | RecordT (TypeX p) (NonEmpty (Id, Type p))
  | FunT (TypeX p) (NonEmpty (Type p)) (Type p) -- type of a normal function
  | Forall (TypeX p) (NonEmpty (UniVar p)) (Type p) -- type of a generic function
  | TData (TypeX p) Id [Type p]
  | TVar (TypeX p) Id
  | TApp (TypeX p) (Type p) (NonEmpty (Type p))
  | TFun (TypeX p) (NonEmpty Id) (Type p) -- type function (for type constructors and type aliases)

typeExt :: Type p -> TypeX p
typeExt (LiteralT ext _) = ext
typeExt (VoidT ext) = ext
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

instance HasRange (Type Transformed) where
  range :: Type Transformed -> Range
  range = typeExt

instance HasRange (Type Typed) where
  range :: Type Typed -> Range
  range = fst . typeExt

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

data Block (p :: Phase)
  = Return (Expr p)
  | Void
  | Do (Expr p) (Block p)
  | Mut Id (Expr p) (Block p)
  | Debug (Expr p) (Block p)
  | Let Bool Id (Expr p) (Block p)
  | Loop (Expr p) (Block p) (Block p)

type family ExprX (p :: Phase) where
  ExprX Typed = (Range, Type Typed)
  ExprX _ = Range

data Expr (p :: Phase) where
  Literal :: (ExprX p) -> Lit -> Expr p
  Data :: (ExprX p) -> Id -> [Expr p] -> Expr p
  Record :: (ExprX p) -> (NonEmpty (Id, Expr p)) -> Expr p
  Tuple :: (ExprX p) -> (NonEmpty (Expr p)) -> Expr p
  Var :: (ExprX p) -> Id -> Expr p
  App :: (ExprX p) -> (Expr p) -> (NonEmpty (Expr p)) -> Expr p
  GenApp :: (ExprX p) -> (Expr p) -> (NonEmpty (Type p)) -> Expr p
  Access :: (ExprX p) -> (Expr p) -> Id -> Expr p
  Index :: (ExprX p) -> (Expr p) -> Int -> Expr p
  Cond :: (ExprX p) -> (Expr p) -> (Expr p) -> (Expr p) -> Expr p
  Fun :: (ExprX p) -> (NonEmpty Id) -> (Expr p) -> Expr p
  GenFun :: (ExprX p) -> (NonEmpty Id) -> (Expr p) -> Expr p
  Block :: (ExprX p) -> (Block p) -> Expr p
  PatternMatching :: (ExprX p) -> (Expr p) -> (NonEmpty (Pattern, Expr p)) -> Expr p
  Chain :: Range -> Chain -> Expr Parsed

exprExt :: Expr p -> ExprX p
exprExt (Literal ext _) = ext
exprExt (Data ext _ _) = ext
exprExt (Record ext _) = ext
exprExt (Tuple ext _) = ext
exprExt (Var ext _) = ext
exprExt (App ext _ _) = ext
exprExt (GenApp ext _ _) = ext
exprExt (Access ext _ _) = ext
exprExt (Index ext _ _) = ext
exprExt (Cond ext _ _ _) = ext
exprExt (Fun ext _ _) = ext
exprExt (GenFun ext _ _) = ext
exprExt (Block ext _) = ext
exprExt (PatternMatching ext _ _) = ext
exprExt (Chain ext _) = ext

instance HasRange (Expr Parsed) where
  range :: Expr Parsed -> Range
  range = exprExt

instance HasRange (Expr Transformed) where
  range :: Expr Transformed -> Range
  range = exprExt

instance HasRange (Expr Typed) where
  range :: Expr Typed -> Range
  range = fst . exprExt

-- PATTERN

data Pattern
  = LiteralP Range Lit
  | DataP Range Id [Pattern]
  | RecordP Range (NonEmpty (Id, Pattern))
  | TupleP Range (NonEmpty Pattern)
  | Capture Id
  | Discard Range

instance HasRange Pattern where
  range :: Pattern -> Range
  range (LiteralP r _) = r
  range (DataP r _ _) = r
  range (RecordP r _) = r
  range (TupleP r _) = r
  range (Capture var) = range var
  range (Discard r) = r

-- MODULE

data BindType = OfExpr | OfType

data Bind :: BindType -> Phase -> HsKind.Type where
  ExprBind :: Id -> Type p -> Expr p -> Bind OfExpr p
  TypeBind :: Id -> Type p -> Bind OfType p
  -- binding for external code
  ForeignBind :: Id -> Type p -> Text -> Bind OfExpr p

binder :: Bind t p -> Id
binder (ExprBind idn _ _) = idn
binder (TypeBind idn _) = idn
binder (ForeignBind idn _ _) = idn

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
  = Defn (Bind OfExpr Parsed)
  | TypeDefn (Bind OfType Parsed)
  | DataDefn (Bind OfType Parsed) (NonEmpty (Bind OfExpr Parsed))
  | FixDefn Fixity Id

data ParsedModule
  = ParsedModule [Defn] (Maybe (Expr Parsed))

data Module (p :: Phase)
  = Module
  { moduleExprs :: [Bind OfExpr p],
    moduleTypes :: [Bind OfType p],
    moduleFixities :: Map Id Fixity,
    moduleEntry :: Maybe (Expr p)
  }
