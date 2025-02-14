module Fine.Syntax.Concrete (module Fine.Syntax.Concrete) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty2 (NonEmpty2)
import Fine.Syntax.Common
  ( Bind,
    Ext,
    Fixity,
    HasRange (..),
    Lit,
    OpChain,
    Range,
    Var,
  )

data Stmt
  = Do Expr
  | Let Var () Expr
  deriving (Show)

data Expr
  = Literal Lit Range
  | Record (NonEmpty (Var, Expr)) Range
  | Tuple (NonEmpty2 Expr) Range
  | Id Var
  | App Expr (NonEmpty Expr) Range
  | Access Expr Var
  | Index Expr Int Range
  | Cond Expr Expr Expr Range
  | PatternMatch Expr (NonEmpty (Expr, Expr)) Range
  | Fun (NonEmpty Var) Expr Range
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
  getRange (Id var) = getRange var
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
  = Defn (Bind () Expr)
  | CtorDefn Var [Var] Range
  | FixDefn Fixity Var
  deriving (Show)

data Module = Module
  { definitions :: [Defn],
    entryExpr :: Maybe Expr
  }
  deriving (Show)
