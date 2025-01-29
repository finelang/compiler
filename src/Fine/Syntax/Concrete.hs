module Fine.Syntax.Concrete (module Fine.Syntax.Concrete) where

import Data.List.NonEmpty (NonEmpty)
import Fine.Syntax.Common
  ( Bind,
    Ext,
    Fixity,
    HasRange (..),
    Lit,
    OpChain,
    Prop (..),
    Range,
    Var (Var),
  )

data Stmt
  = Do Expr
  | Let Var () Expr
  deriving (Show)

data Expr
  = Literal Lit Range
  | Obj [Prop Expr] Range
  | Variant Var [Prop Expr] Range
  | Tuple (NonEmpty Expr) Range
  | Id Var
  | App Expr [Expr] Range
  | Access Expr Var
  | Cond Expr Expr Expr Range
  | PatternMatch Expr (NonEmpty (Expr, Expr)) Range
  | Fun [Var] Expr Range
  | Parens Expr
  | Block [Stmt] Expr Range
  | Chain (OpChain Expr)
  | ExtExpr Ext
  | Debug Expr Range
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Literal _ r) = r
  getRange (Obj _ r) = r
  getRange (Variant _ _ r) = r
  getRange (Tuple _ r) = r
  getRange (Id (Var _ r)) = r
  getRange (App _ _ r) = r
  getRange (Access expr prop) = getRange (expr, prop)
  getRange (Cond _ _ _ r) = r
  getRange (PatternMatch _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Parens expr) = getRange expr
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
