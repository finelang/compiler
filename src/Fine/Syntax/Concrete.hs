module Fine.Syntax.Concrete (module Fine.Syntax.Concrete) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty2 (NonEmpty2)
import Fine.Syntax.Common
  ( Bind,
    Ext,
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
  | Debug Expr Range

data Expr
  = Literal Lit Range
  | Record (NonEmpty (Id, Expr)) Range
  | Tuple (NonEmpty2 Expr) Range
  | Var Id
  | Discard Range
  | Mut Id Expr
  | App Expr [Expr] Range
  | Access Expr Id
  | Index Expr Int Range
  | Cond Expr Expr Expr Range
  | PatternMatch Expr (NonEmpty (Expr, Expr)) Range
  | Fun [Id] Expr Range
  | Block [Stmt] Expr Range
  | Chain (OpChain Expr)
  | ExtExpr Ext

instance HasRange Expr where
  range :: Expr -> Range
  range (Literal _ r) = r
  range (Record _ r) = r
  range (Tuple _ r) = r
  range (Var var) = range var
  range (Discard r) = r
  range (Mut var expr) = range var <> range expr
  range (App _ _ r) = r
  range (Access expr prop) = range expr <> range prop
  range (Index _ _ r) = r
  range (Cond _ _ _ r) = r
  range (PatternMatch _ _ r) = r
  range (Fun _ _ r) = r
  range (Block _ _ r) = r
  range (Chain chain) = range chain
  range (ExtExpr ext) = range ext

data CtorDefn = CtorDefn Id [Id] Range

data Defn
  = Defn (Bind () Expr)
  | DataDefn (NonEmpty CtorDefn)
  | MRDefns (NonEmpty2 (Bind () Expr)) -- mutually recursive function definitions
  | FixDefn Fixity Id

data Module = Module
  { definitions :: [Defn],
    entryExpr :: Maybe Expr
  }
