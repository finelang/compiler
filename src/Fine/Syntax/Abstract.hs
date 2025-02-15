module Fine.Syntax.Abstract
  ( Pattern (..),
    Block (..),
    Expr (..),
    Module (..),
    boundVars,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty2 (NonEmpty2)
import Data.Map.Strict (Map)
import Fine.Syntax.Common
  ( Bind,
    Ext,
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
  deriving (Show)

instance HasRange Pattern where
  getRange :: Pattern -> Range
  getRange (LiteralP _ r) = r
  getRange (DataP _ _ r) = r
  getRange (RecordP _ r) = r
  getRange (TupleP _ r) = r
  getRange (Capture (Id _ r)) = r

boundVars :: Pattern -> [Id]
boundVars (LiteralP _ _) = []
boundVars (DataP _ patts _) = concatMap boundVars patts
boundVars (RecordP props _) = foldMap (boundVars . snd) props
boundVars (TupleP patts _) = foldMap boundVars patts
boundVars (Capture idn) = [idn]

data Block
  = Return Expr
  | Do Expr Block
  | Let Id () Expr Block
  deriving (Show)

data Expr
  = Literal Lit Range
  | Data Id [Expr] Range
  | Record (NonEmpty (Id, Expr)) Range
  | Tuple (NonEmpty2 Expr) Range
  | Var Id
  | App Expr (NonEmpty Expr) Range
  | Access Expr Id
  | Index Expr Int Range
  | PatternMatch Expr (NonEmpty (Pattern, Expr)) Range
  | Cond Expr Expr Expr Range
  | Fun (NonEmpty Id) Expr Range
  | Block Block Range
  | ExtExpr Ext
  | Debug Expr Range
  | Closure (Map Id Expr) Expr (Maybe Id)
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Literal _ r) = r
  getRange (Data _ _ r) = r
  getRange (Record _ r) = r
  getRange (Tuple _ r) = r
  getRange (Var var) = getRange var
  getRange (App _ _ r) = r
  getRange (Access expr prop) = getRange (expr, prop)
  getRange (Index _ _ r) = r
  getRange (Cond _ _ _ r) = r
  getRange (PatternMatch _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Block _ r) = r
  getRange (ExtExpr ext) = getRange ext
  getRange (Debug _ r) = r
  getRange (Closure _ expr _) = getRange expr

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
