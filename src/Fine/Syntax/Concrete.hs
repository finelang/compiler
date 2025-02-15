module Fine.Syntax.Concrete (module Fine.Syntax.Concrete) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty2 (NonEmpty2)
import Fine.Syntax.Common
  ( Ext,
    Fixity,
    HasRange (..),
    Id,
    Lit,
    OpChain,
    Range,
  )

data Stmt
  = Do Expr
  | Let Bool Id () Expr
  deriving (Show)

data Expr
  = Literal Lit Range
  | Record (NonEmpty (Id, Expr)) Range
  | Tuple (NonEmpty2 Expr) Range
  | Var Id
  | Mut Id Expr
  | App Expr (NonEmpty Expr) Range
  | Access Expr Id
  | Index Expr Int Range
  | Cond Expr Expr Expr Range
  | PatternMatch Expr (NonEmpty (Expr, Expr)) Range
  | Fun (NonEmpty Id) Expr Range
  | Block [Stmt] Expr Range
  | Chain (OpChain Expr)
  | ExtExpr Ext
  | Debug Expr Range
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Literal _ r) = r
  getRange (Record _ r) = r
  getRange (Tuple _ r) = r
  getRange (Var var) = getRange var
  getRange (Mut var expr) = getRange (var, expr)
  getRange (App _ _ r) = r
  getRange (Access expr prop) = getRange (expr, prop)
  getRange (Index _ _ r) = r
  getRange (Cond _ _ _ r) = r
  getRange (PatternMatch _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Block _ _ r) = r
  getRange (Chain chain) = getRange chain
  getRange (ExtExpr ext) = getRange ext
  getRange (Debug _ r) = r

data Defn
  = Defn Id Expr
  | CtorDefn Id [Id] Range
  | FixDefn Fixity Id
  deriving (Show)

data Module = Module
  { definitions :: [Defn],
    entryExpr :: Maybe Expr
  }
  deriving (Show)
