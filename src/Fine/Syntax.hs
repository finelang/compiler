{-# LANGUAGE UndecidableInstances #-}

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
  | Ready -- ready for codegen
  deriving (Show)

-- KIND

data Kind
  = KLit Range
  | FunK (NonEmpty Kind) Kind -- type of type functions
  deriving (Show)

instance HasRange Kind where
  range :: Kind -> Range
  range (KLit r) = r
  range (FunK left right) = range (NonEmpty.head left) <> range right

-- TYPE

data LitT = IntT | FloatT | BoolT | StrT | UnitT
  deriving (Show)

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

deriving instance (Show (TypeX p), Show (UniVar p)) => Show (Type p)

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

deriving instance (Show (Expr Parsed)) => Show Chain

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
  deriving (Show)

type family ReadyX (p :: Phase) where
  ReadyX Ready = ()
  ReadyX _ = Void

type family NonReadyX (p :: Phase) where
  NonReadyX Ready = Void
  NonReadyX _ = ()

data Block (p :: Phase)
  = Return (Expr p)
  | Void
  | Do (Expr p) (Block p)
  | Mut Id (Expr p) (Block p)
  | Debug (Expr p) (Block p)
  | Let Bool Id (Expr p) (Block p)
  | Loop (Expr p) (Block p) (Block p)
  | If (ReadyX p) (Expr p) (Block p) (Block p)
  | LetPatt (NonReadyX p) Pattern (Expr p) (Block p)

deriving instance (Show (ReadyX p), Show (NonReadyX p), Show (Expr p)) => Show (Block p)

type family ExprX (p :: Phase) where
  ExprX Ready = ()
  ExprX Typed = (Range, Type Typed)
  ExprX _ = Range

type family NonReadyExprX (p :: Phase) where
  NonReadyExprX Ready = Void
  NonReadyExprX Typed = (Range, Type Typed)
  NonReadyExprX _ = Range

-- for CST only
type family ParsedX (p :: Phase) where
  ParsedX Parsed = Range
  ParsedX _ = Void

data Expr (p :: Phase)
  = Literal (ExprX p) Lit
  | Data (ExprX p) Id [Expr p]
  | Record (ExprX p) (NonEmpty (Id, Expr p))
  | Tuple (ExprX p) (NonEmpty (Expr p))
  | Var (ExprX p) Id
  | App (ExprX p) (Expr p) (NonEmpty (Expr p))
  | GenApp (NonReadyExprX p) (Expr p) (NonEmpty (Type p))
  | Access (ExprX p) (Expr p) Id
  | Index (ExprX p) (Expr p) Int
  | Cond (ExprX p) (Expr p) (Expr p) (Expr p)
  | Fun (ExprX p) (NonEmpty Id) (Expr p)
  | GenFun (NonReadyExprX p) (NonEmpty Id) (Expr p)
  | Block (ExprX p) (Block p)
  | PatternMatching (NonReadyExprX p) (Expr p) (NonEmpty (Pattern, Expr p))
  | Chain (ParsedX p) Chain
  | Conj (ReadyX p) (NonEmpty (Expr p))
  | Equals (ReadyX p) (Expr p) (Expr p)

deriving instance
  ( Show (ExprX p),
    Show (NonReadyExprX p),
    Show (ParsedX p),
    Show (ReadyX p),
    Show (NonReadyX p),
    Show (Type p)
  ) =>
  Show (Expr p)

instance HasRange (Expr Parsed) where
  range :: Expr Parsed -> Range
  range (Literal r _) = r
  range (Data r _ _) = r
  range (Record r _) = r
  range (Tuple r _) = r
  range (Var r _) = r
  range (App r _ _) = r
  range (GenApp r _ _) = r
  range (Access r _ _) = r
  range (Index r _ _) = r
  range (Cond r _ _ _) = r
  range (Fun r _ _) = r
  range (GenFun r _ _) = r
  range (Block r _) = r
  range (PatternMatching r _ _) = r
  range (Chain r _) = r

-- PATTERN

data Pattern
  = LiteralP Range Lit
  | DataP Range Id [Pattern]
  | RecordP Range (NonEmpty (Id, Pattern))
  | TupleP Range (NonEmpty Pattern)
  | Capture Id
  | Discard Range
  deriving (Show)

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
  deriving (Show)

type family BoundType (p :: Phase) where
  BoundType Ready = ()
  BoundType p = Type p

data Bind :: BindType -> Phase -> HsKind.Type where
  ExprBind :: Id -> BoundType p -> Expr p -> Bind OfExpr p
  TypeBind :: Id -> BoundType p -> Bind OfType p
  -- binding for external code
  ForeignBind :: Id -> BoundType p -> Text -> Bind OfExpr p

deriving instance (Show (BoundType p), Show (Expr p)) => Show (Bind t p)

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

type family ModuleTypes (p :: Phase) where
  ModuleTypes Ready = ()
  ModuleTypes p = [Bind OfType p]

type family ModuleFixities (p :: Phase) where
  ModuleFixities Ready = ()
  ModuleFixities _ = Map Id Fixity

data Module (p :: Phase)
  = Module
  { moduleExprs :: [Bind OfExpr p],
    moduleTypes :: ModuleTypes p,
    moduleFixities :: ModuleFixities p,
    moduleEntry :: Maybe (Expr p)
  }

deriving instance
  ( Show (ModuleFixities p),
    Show (ModuleTypes p),
    Show (BoundType p),
    Show (Expr p)
  ) =>
  Show (Module p)
