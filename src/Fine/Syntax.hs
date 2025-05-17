{-# LANGUAGE UndecidableInstances #-}

module Fine.Syntax (
  Range (..),
  HasRange (..),
  Id (..),
  idText,
  Phase (..),
  Kind (..),
  typeof,
  LitT (..),
  Type (..),
  Op (..),
  Equation (..),
  Lit (..),
  Block (..),
  Expr (..),
  Pattern (..),
  BindType (..),
  Bind (..),
  binder,
  Defn (..),
  ParsedModule (..),
  Module (..),
)
where

import Data.Function (on)
import Data.Kind qualified as HsKind
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
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

data Id = Id Range Text

idText :: Id -> Text
idText (Id _ text) = text

instance Eq Id where
  (==) :: Id -> Id -> Bool
  (Id _ text) == (Id _ text') = text == text'

instance Ord Id where
  compare :: Id -> Id -> Ordering
  compare = compare `on` idText

instance HasRange Id where
  range :: Id -> Range
  range (Id r _) = r

instance Show Id where
  show :: Id -> String
  show (Id _ text) = Text.unpack text

-- PASS

data Phase
  = Parsed -- after parsing
  | Transformed
  | PartiallyTyped
  | Typed -- after type checking/inference
  | Ready
  deriving (Show)

type family ReadyOnly (p :: Phase) where
  ReadyOnly Ready = ()
  ReadyOnly _ = Void

type family NotReady (p :: Phase) where
  NotReady Ready = Void
  NotReady _ = ()

type family PartiallyTypedOnly (p :: Phase) where
  PartiallyTypedOnly PartiallyTyped = ()
  PartiallyTypedOnly _ = Void

type family ParsedOnly (p :: Phase) where
  ParsedOnly Parsed = ()
  ParsedOnly _ = Void

type family NotParsed (p :: Phase) where
  NotParsed Parsed = Void
  NotParsed _ = ()

-- KIND

-- data Kind
--   = KLit
--   | FunK (NonEmpty Kind) Kind -- type of type functions
--   deriving (Show)

data Kind :: Phase -> HsKind.Type where
  KLit :: Range -> Kind p
  TFunK :: Range -> NonEmpty (Kind p) -> Kind p -> Kind p
  --
  SubsttKVar :: Range -> Id -> Kind PartiallyTyped

instance Show (Kind p) where
  show :: Kind p -> String
  show (KLit _) = "*"
  show (TFunK _ kinds kind) =
    let argsText = intercalate ", " $ map show $ NonEmpty.toList kinds
     in [i|(#{argsText}) -> #{show kind}|]
  show (SubsttKVar _ var) = Text.unpack (idText var)

instance HasRange (Kind p) where
  range :: Kind p -> Range
  range (KLit r) = r
  range (TFunK r _ _) = r
  range (SubsttKVar r _) = r

-- TYPE

class HasType v t where
  typeof :: v -> t

data LitT = IntT | FloatT | BoolT | StrT | UnitT
  deriving (Show)

type family TypeX (p :: Phase) where
  TypeX Ready = ()
  TypeX Typed = (Range, Kind Typed)
  TypeX PartiallyTyped = (Range, Kind PartiallyTyped)
  TypeX _ = Range

data Type (p :: Phase)
  = LiteralT (TypeX p) LitT
  | VoidT (TypeX p)
  | TupleT (TypeX p) (Type p) (Type p) [Type p]
  | ListT (TypeX p) (Type p)
  | RecordT (TypeX p) [(Id, Type p)]
  | FunT (TypeX p) (NonEmpty (Type p)) (Type p) -- type of a normal function
  | Forall (TypeX p) (NonEmpty Id) (Type p)
  | TData (TypeX p) Id [Type p]
  | TVar (TypeX p) Id
  | TApp (TypeX p) (Type p) (NonEmpty (Type p))
  | TFun (TypeX p) (NonEmpty Id) (Type p) -- type function (for type constructors and type aliases)
  --
  | SubsttTVar (TypeX p) (PartiallyTypedOnly p) Id

deriving instance (Show (TypeX p), Show (PartiallyTypedOnly p)) => Show (Type p)

typeExt :: Type p -> TypeX p
typeExt (LiteralT ext _) = ext
typeExt (VoidT ext) = ext
typeExt (TupleT ext _ _ _) = ext
typeExt (ListT ext _) = ext
typeExt (RecordT ext _) = ext
typeExt (FunT ext _ _) = ext
typeExt (Forall ext _ _) = ext
typeExt (TData ext _ _) = ext
typeExt (TVar ext _) = ext
typeExt (TApp ext _ _) = ext
typeExt (TFun ext _ _) = ext
typeExt (SubsttTVar ext _ _) = ext

instance HasRange (Type Parsed) where
  range :: Type Parsed -> Range
  range = typeExt

instance HasRange (Type Transformed) where
  range :: Type Transformed -> Range
  range = typeExt

instance HasRange (Type Typed) where
  range :: Type Typed -> Range
  range = fst . typeExt

instance HasType (Type Typed) (Kind Typed) where
  typeof :: Type Typed -> Kind Typed
  typeof = snd . typeExt

instance HasType (Type PartiallyTyped) (Kind PartiallyTyped) where
  typeof :: Type PartiallyTyped -> Kind PartiallyTyped
  typeof = snd . typeExt

-- EXPR

data Op
  = Add
  | Sub
  | Mult
  | Div
  | Rest
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | And
  | Or
  | Concat
  | Pipe
  | RPipe
  deriving (Show)

data Equation t
  = Operand t
  | Operation t Op (Equation t)
  deriving (Show)

data Lit
  = Int Int
  | Float Float
  | Bool Bool
  | Str Text
  | Unit
  deriving (Show)

data Block (p :: Phase)
  = Return (Expr p)
  | Void
  | Do (Expr p) (Block p)
  | Mut Id (Expr p) (Block p)
  | Debug (Expr p) (Block p)
  | Let Bool Id (Expr p) (Block p)
  | Loop (Expr p) (Block p) (Block p)
  | LetPatt (NotReady p) Pattern (Expr p) (Block p)
  | If (ReadyOnly p) (Expr p) (Block p) (Block p)

deriving instance (Show (Expr p), Show (ReadyOnly p), Show (NotReady p)) => Show (Block p)

type family ExprX (p :: Phase) where
  ExprX Ready = Type Ready
  ExprX Typed = (Range, Type Typed)
  ExprX PartiallyTyped = (Range, Type PartiallyTyped)
  ExprX _ = Range

data Expr (p :: Phase)
  = Literal (ExprX p) Lit
  | Data (ExprX p) Id [Expr p]
  | Record (ExprX p) [(Id, Expr p)]
  | Tuple (ExprX p) (Expr p) (Expr p) [Expr p]
  | List (ExprX p) [Expr p]
  | Var (ExprX p) Id
  | Bin (ExprX p) (NotParsed p) Op (Expr p) (Expr p)
  | App (ExprX p) (Expr p) (NonEmpty (Expr p))
  | GenApp (ExprX p) (Expr p) (NonEmpty (Type p))
  | Access (ExprX p) (Expr p) Id
  | Index (ExprX p) (Expr p) Int
  | Cond (ExprX p) (Expr p) (Expr p) (Expr p)
  | Fun (ExprX p) (NonEmpty Id) (Expr p)
  | GenFun (ExprX p) (NotParsed p) (NonEmpty Id) (Expr p)
  | Block (ExprX p) (Block p)
  | PatternMatching (ExprX p) (NotReady p) (Expr p) (NonEmpty (Pattern, Expr p))
  | Equation (ExprX p) (ParsedOnly p) (Equation (Expr p))
  | PartialEquation (ExprX p) (ParsedOnly p) (Equation (Either Range (Expr p)))

deriving instance
  ( Show (ExprX p),
    Show (Type p),
    Show (NotParsed p),
    Show (ParsedOnly p),
    Show (NotReady p),
    Show (Block p)
  ) =>
  Show (Expr p)

exprExt :: Expr p -> ExprX p
exprExt (Literal ext _) = ext
exprExt (Data ext _ _) = ext
exprExt (Record ext _) = ext
exprExt (Tuple ext _ _ _) = ext
exprExt (List ext _) = ext
exprExt (Var ext _) = ext
exprExt (Bin ext _ _ _ _) = ext
exprExt (App ext _ _) = ext
exprExt (GenApp ext _ _) = ext
exprExt (Access ext _ _) = ext
exprExt (Index ext _ _) = ext
exprExt (Cond ext _ _ _) = ext
exprExt (Fun ext _ _) = ext
exprExt (GenFun ext _ _ _) = ext
exprExt (Block ext _) = ext
exprExt (PatternMatching ext _ _ _) = ext
exprExt (Equation ext _ _) = ext
exprExt (PartialEquation ext _ _) = ext

instance HasRange (Expr Parsed) where
  range :: Expr Parsed -> Range
  range = exprExt

instance HasRange (Expr Transformed) where
  range :: Expr Transformed -> Range
  range = exprExt

instance HasRange (Expr Typed) where
  range :: Expr Typed -> Range
  range = fst . exprExt

instance HasType (Expr Typed) (Type Typed) where
  typeof :: Expr Typed -> Type Typed
  typeof = snd . exprExt

instance HasType (Expr PartiallyTyped) (Type PartiallyTyped) where
  typeof :: Expr PartiallyTyped -> Type PartiallyTyped
  typeof = snd . exprExt

instance HasType (Expr Ready) (Type Ready) where
  typeof :: Expr Ready -> Type Ready
  typeof = exprExt

-- PATTERN

data Pattern
  = LiteralP Range Lit
  | DataP Range Id [Pattern]
  | RecordP Range [(Id, Pattern)]
  | TupleP Range Pattern Pattern [Pattern]
  | ListP Range [Pattern]
  | Capture Id
  | Discard Range
  deriving (Show)

instance HasRange Pattern where
  range :: Pattern -> Range
  range (LiteralP r _) = r
  range (DataP r _ _) = r
  range (RecordP r _) = r
  range (TupleP r _ _ _) = r
  range (ListP r _) = r
  range (Capture var) = range var
  range (Discard r) = r

-- MODULE

data BindType = OfExpr | OfType
  deriving (Show)

data Bind :: BindType -> Phase -> HsKind.Type where
  ExprBind :: Id -> Type p -> Expr p -> Bind OfExpr p
  TypeBind :: Id -> Type p -> Bind OfType p
  -- binding for external code
  ForeignBind :: Id -> Type p -> Text -> Bind OfExpr p

deriving instance (Show (Expr p), Show (Type p)) => Show (Bind t p)

binder :: Bind t p -> Id
binder (ExprBind idn _ _) = idn
binder (TypeBind idn _) = idn
binder (ForeignBind idn _ _) = idn

data Defn
  = Defn (Bind OfExpr Parsed)
  | TypeDefn (Bind OfType Parsed)
  | DataDefn (Bind OfType Parsed) (NonEmpty (Bind OfExpr Parsed))
  | MutRecDefns (NonEmpty (Bind OfExpr Parsed))

data ParsedModule
  = ParsedModule [Defn] (Maybe (Expr Parsed))

type family ModuleTypes (p :: Phase) where
  ModuleTypes Ready = ()
  ModuleTypes p = [Bind OfType p]

data Module (p :: Phase)
  = Module
  { moduleExprs :: [Bind OfExpr p],
    moduleTypes :: ModuleTypes p,
    moduleEntry :: Maybe (Expr p)
  }

deriving instance (Show (Expr p), Show (Type p), Show (ModuleTypes p)) => Show (Module p)
