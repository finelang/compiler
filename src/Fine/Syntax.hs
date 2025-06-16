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
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Err.Extra (errorUNREACHABLE)

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
  | PartiallyKinded
  | Kinded
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

type family ParsedOnly (p :: Phase) where
  ParsedOnly Parsed = ()
  ParsedOnly _ = Void

type family NotParsed (p :: Phase) where
  NotParsed Parsed = Void
  NotParsed _ = ()

-- KIND

data Kind :: Phase -> HsKind.Type where
  KLit :: Range -> Kind p
  TFunK :: Kind p -> Kind p -> Kind p
  --
  SubKVar :: Id -> Kind PartiallyKinded

instance Show (Kind p) where
  show :: Kind p -> String
  show (KLit _) = "*"
  show (TFunK argK bodyK) =
    let arg = show argK
        body = show bodyK
     in case argK of
          TFunK _ _ -> [i|(#{arg}) -> #{body}|]
          _ -> [i|#{arg} -> #{body}|]
  show (SubKVar var) = Text.unpack (idText var)

instance HasRange (Kind p) where
  range :: Kind p -> Range
  range (KLit r) = r
  range (TFunK ak bk) = range ak <> range bk
  range (SubKVar var) = range var

-- TYPE

class HasType v t where
  typeof :: v -> t

data LitT = IntT | FloatT | BoolT | StrT | UnitT
  deriving (Show)

type family KindX (p :: Phase) where
  KindX Ready = Void
  KindX Transformed = ()
  KindX Parsed = ()
  KindX p = Kind p

type family UniVar (p :: Phase) where
  UniVar Ready = Id
  UniVar Transformed = Id
  UniVar Parsed = Id
  UniVar p = (Id, Kind p)

data Type :: Phase -> HsKind.Type where
  LiteralT :: KindX p -> Range -> LitT -> Type p
  VoidT :: KindX p -> Range -> Type p
  TupleT :: KindX p -> Range -> Type p -> Type p -> [Type p] -> Type p
  RecordT :: KindX p -> Range -> [(Id, Type p)] -> Type p
  FunT :: KindX p -> Type p -> Type p -> Type p
  Forall :: KindX p -> Range -> NonEmpty (UniVar p) -> Type p -> Type p
  DataT :: KindX p -> Id -> [Type p] -> Type p
  TVar :: KindX p -> Id -> Type p
  TApp :: KindX p -> Type p -> Type p -> Type p
  TFun :: KindX p -> Id -> Type p -> Type p
  --
  SubTVar :: Id -> Type PartiallyTyped

deriving instance (Show (KindX p), Show (UniVar p)) => Show (Type p)

instance HasRange (Type p) where
  range :: Type p -> Range
  range (LiteralT _ r _) = r
  range (VoidT _ r) = r
  range (TupleT _ r _ _ _) = r
  range (RecordT _ r _) = r
  range (FunT _ at bt) = range at <> range bt
  range (Forall _ r _ _) = r
  range (DataT _ _ _) = NoRange
  range (TVar _ var) = range var
  range (TApp _ tf ta) = range tf <> range ta
  range (TFun _ tp tb) = range tp <> range tb
  range (SubTVar var) = range var

kindExt :: Type p -> KindX p
kindExt (LiteralT k _ _) = k
kindExt (VoidT k _) = k
kindExt (TupleT k _ _ _ _) = k
kindExt (RecordT k _ _) = k
kindExt (FunT k _ _) = k
kindExt (Forall k _ _ _) = k
kindExt (DataT k _ _) = k
kindExt (TVar k _) = k
kindExt (TApp k _ _) = k
kindExt (TFun k _ _) = k
kindExt (SubTVar _) =
  errorUNREACHABLE "Substitution type var AST doesn't store its kind because it represents an unknown type."

instance HasType (Type PartiallyKinded) (Kind PartiallyKinded) where
  typeof :: Type PartiallyKinded -> Kind PartiallyKinded
  typeof = kindExt

instance HasType (Type Kinded) (Kind Kinded) where
  typeof :: Type Kinded -> Kind Kinded
  typeof = kindExt

-- EXPR

data Op (p :: Phase) where
  Add :: Op p
  Sub :: Op p
  Mult :: Op p
  Div :: Op p
  Rest :: Op p
  Eq :: Op p
  Neq :: Op p
  Lt :: Op p
  Gt :: Op p
  Le :: Op p
  Ge :: Op p
  And :: Op p
  Or :: Op p
  Concat :: Op p
  Pipe :: Op Parsed
  RPipe :: Op Parsed

deriving instance (Show (Op p))

data Equation t
  = Operand t
  | Operation t (Op Parsed) (Equation t)
  deriving (Show)

instance (HasRange t) => HasRange (Equation t) where
  range :: (HasRange t) => Equation t -> Range
  range (Operand x) = range x
  range (Operation x _ equation) = range x <> range equation

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
  | LetMut Id (Expr p) (Block p)
  | Let (NotReady p) Pattern (Expr p) (Block p)
  | Debug Range (Expr p) (Block p)
  | Loop (Expr p) (Block p) (Block p)
  | If (ReadyOnly p) (Expr p) (Block p) (Block p)
  | LetImmut (ReadyOnly p) Id (Expr p) (Block p)

deriving instance (Show (Expr p), Show (ReadyOnly p), Show (NotReady p)) => Show (Block p)

type family TypeX (p :: Phase) where
  TypeX Typed = Type Typed
  TypeX PartiallyTyped = Type PartiallyTyped
  TypeX _ = ()

data Expr (p :: Phase)
  = Literal (TypeX p) Range Lit
  | Data (TypeX p) Id [Expr p]
  | Record (TypeX p) Range [(Id, Expr p)]
  | Tuple (TypeX p) Range (Expr p) (Expr p) [Expr p]
  | Var (TypeX p) Id
  | Bin (TypeX p) (NotParsed p) (Op Ready) (Expr p) (Expr p)
  | App (TypeX p) (Expr p) (Expr p)
  | GenApp (TypeX p) (NotReady p) Id (NonEmpty (Type p))
  | Access (TypeX p) (Expr p) Id
  | Index (TypeX p) Range (Expr p) Int
  | Cond (TypeX p) Range (Expr p) (Expr p) (Expr p)
  | Fun (TypeX p) Id (Expr p)
  | GenFun (TypeX p) (NotParsed p) (NotReady p) (NonEmpty Id) (Expr p)
  | Block (TypeX p) Range (Block p)
  | PatternMatching (TypeX p) Range (NotReady p) (Expr p) (NonEmpty (Pattern, Expr p))
  | Equation Range (ParsedOnly p) (Equation (Expr p))

deriving instance
  ( Show (TypeX p),
    Show (Type p),
    Show (NotParsed p),
    Show (ParsedOnly p),
    Show (NotReady p),
    Show (Block p)
  ) =>
  Show (Expr p)

instance HasRange (Expr p) where
  range :: Expr p -> Range
  range (Literal _ r _) = r
  range (Data _ _ _) = NoRange
  range (Record _ r _) = r
  range (Tuple _ r _ _ _) = r
  range (Var _ var) = range var
  range (Bin _ _ _ left right) = range left <> range right
  range (App _ f arg) = range f <> range arg
  range (GenApp _ _ fname types) = range fname <> range (NonEmpty.last types)
  range (Access _ obj prop) = range obj <> range prop
  range (Index _ r _ _) = r
  range (Cond _ r _ _ _) = r
  range (Fun _ param body) = range param <> range body
  range (GenFun _ _ _ tparams body) = range (NonEmpty.head tparams) <> range body
  range (Block _ r _) = r
  range (PatternMatching _ r _ _ _) = r
  range (Equation r _ _) = r

typeExt :: Expr p -> TypeX p
typeExt (Literal t _ _) = t
typeExt (Data t _ _) = t
typeExt (Record t _ _) = t
typeExt (Tuple t _ _ _ _) = t
typeExt (Var t _) = t
typeExt (Bin t _ _ _ _) = t
typeExt (App t _ _) = t
typeExt (GenApp t _ _ _) = t
typeExt (Access t _ _) = t
typeExt (Index t _ _ _) = t
typeExt (Cond t _ _ _ _) = t
typeExt (Fun t _ _) = t
typeExt (GenFun t _ _ _ _) = t
typeExt (Block t _ _) = t
typeExt (PatternMatching t _ _ _ _) = t
typeExt (Equation _ _ _) =
  errorUNREACHABLE "Equation AST doesn't store its type because it only exists until transformation phase."

instance HasType (Expr Typed) (Type Typed) where
  typeof :: Expr Typed -> Type Typed
  typeof = typeExt

instance HasType (Expr PartiallyTyped) (Type PartiallyTyped) where
  typeof :: Expr PartiallyTyped -> Type PartiallyTyped
  typeof = typeExt

-- PATTERN

data Pattern
  = LiteralP Range Lit
  | DataP Range Id [Pattern]
  | RecordP Range [(Id, Pattern)]
  | TupleP Range Pattern Pattern [Pattern]
  | Capture Id
  | Discard Range
  deriving (Show)

instance HasRange Pattern where
  range :: Pattern -> Range
  range (LiteralP r _) = r
  range (DataP r _ _) = r
  range (RecordP r _) = r
  range (TupleP r _ _ _) = r
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

deriving instance (Show (Expr p), Show (Type p), Show (BoundType p)) => Show (Bind t p)

binder :: Bind t p -> Id
binder (ExprBind idn _ _) = idn
binder (TypeBind idn _) = idn
binder (ForeignBind idn _ _) = idn

data Defn
  = ValueDefn Id (Expr Parsed)
  | ForeignDefn Id Text
  | TypingDefn Id (Type Parsed)
  | TypeDefn (Bind OfType Parsed)
  | DataDefn (Bind OfType Parsed) (NonEmpty (Bind OfExpr Parsed))

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

deriving instance
  ( Show (Expr p),
    Show (Type p),
    Show (BoundType p),
    Show (ModuleTypes p)
  ) =>
  Show (Module p)
