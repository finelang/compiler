module Fine.Syntax.Abstract
  ( Pattern (..),
    Block (..),
    Expr (..),
    Bind (..),
    Module (..),
    boundVars,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty2 (NonEmpty2)
import Data.Map.Strict (Map)
import Fine.Syntax.Common
  ( Ext,
    Fixity,
    HasRange (..),
    Id (Id),
    Lit,
    Range,
  )

data Pattern
  = LiteralP Lit Range
  | DataP Id [Pattern] Range
  | RecordP (NonEmpty (Id, Pattern)) Range
  | TupleP (NonEmpty2 Pattern) Range
  | Capture Id
  | DiscardP Range
  deriving (Show)

instance HasRange Pattern where
  range :: Pattern -> Range
  range (LiteralP _ r) = r
  range (DataP _ _ r) = r
  range (RecordP _ r) = r
  range (TupleP _ r) = r
  range (Capture (Id _ r)) = r
  range (DiscardP r) = r

boundVars :: Pattern -> [Id]
boundVars (LiteralP _ _) = []
boundVars (DataP _ patts _) = concatMap boundVars patts
boundVars (RecordP props _) = foldMap (boundVars . snd) props
boundVars (TupleP patts _) = foldMap boundVars patts
boundVars (Capture idn) = [idn]
boundVars (DiscardP _) = []

data Block
  = Return Expr
  | Do Expr Block
  | Let Bool Id () Expr Block
  | Void -- internal
  | Loop Expr Block Block -- internal
  deriving (Show)

data Expr
  = Literal Lit Range
  | Data Id [Expr] Range
  | Record (NonEmpty (Id, Expr)) Range
  | Tuple (NonEmpty2 Expr) Range
  | Var Id
  | Mut Id Expr
  | App Expr [Expr] Range
  | Access Expr Id
  | Index Expr Int Range
  | PatternMatch Expr (NonEmpty (Pattern, Expr)) Range
  | Cond Expr Expr Expr Range
  | Fun [Id] Expr Range
  | Block Block Range
  | ExtExpr Ext
  | Closure (Map Id Expr) Expr (Maybe Id)
  | Debug Expr Range
  deriving (Show)

instance HasRange Expr where
  range :: Expr -> Range
  range (Literal _ r) = r
  range (Data _ _ r) = r
  range (Record _ r) = r
  range (Tuple _ r) = r
  range (Var var) = range var
  range (Mut var expr) = range var <> range expr
  range (App _ _ r) = r
  range (Access expr prop) = range expr <> range prop
  range (Index _ _ r) = r
  range (Cond _ _ _ r) = r
  range (PatternMatch _ _ r) = r
  range (Fun _ _ r) = r
  range (Block _ r) = r
  range (ExtExpr ext) = range ext
  range (Closure _ expr _) = range expr
  range (Debug _ r) = r

data Bind t v = Bind
  { binder :: Id,
    boundType :: t,
    boundValue :: v
  }
  deriving (Show)

data Module
  = Module
      { bindings :: [Bind () Expr],
        fixities :: Map Id Fixity
      }
  | EntryModule
      { bindings :: [Bind () Expr],
        fixities :: Map Id Fixity,
        _entryExpr :: Expr
      }
  deriving (Show)
