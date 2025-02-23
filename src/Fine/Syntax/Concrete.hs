module Fine.Syntax.Concrete (module Fine.Syntax.Concrete) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL
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

data Expr
  = Literal Lit Range
  | Record (NonEmpty (Id, Expr)) Range
  | Tuple (NonEmpty2 Expr) Range
  | Var Id
  | Discard Range
  | Mut Id Expr
  | App Expr Expr
  | Access Expr Id
  | Index Expr Int Range
  | Cond Expr Expr Expr Range
  | PatternMatch Expr (NonEmpty (Expr, Expr)) Range
  | Fun (NonEmpty Id) Expr Range
  | Block (NonEmpty Stmt) Expr Range
  | Debug Expr Range
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
  range (App f arg) = range f <> range arg
  range (Access expr prop) = range expr <> range prop
  range (Index _ _ r) = r
  range (Cond _ _ _ r) = r
  range (PatternMatch _ _ r) = r
  range (Fun _ _ r) = r
  range (Block _ _ r) = r
  range (Debug _ r) = r
  range (Chain chain) = range chain
  range (ExtExpr ext) = range ext

flattenApp :: Expr -> Maybe (Expr, NonEmpty Expr)
flattenApp expr = (fmap . fmap) NEL.reverse (go expr)
  where
    go (App app@(App _ _) arg) = (fmap . fmap) (NEL.cons arg) (go app)
    go (App f arg) = Just (f, NEL.singleton arg)
    go _ = Nothing

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
