module Fine.Syntax.ParsedExpr
  ( Defn (..),
    Expr (..),
    Module (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Fine.Syntax.Common (Bind, Ext, Fixity, HasRange (..), OpChain, Prop (..), Range, Var (Var), VariantSpec)

data Expr
  = Int Int Range
  | Float Float Range
  | Str Text Range
  | Unit Range
  | Obj [Prop Expr] Range
  | Variant Var [Prop Expr] Range
  | Tuple Expr Expr [Expr] Range
  | Id Var
  | App Expr [Expr] Range
  | Access Expr Var
  | Cond Expr Expr Expr Range
  | PatternMatch Expr (NonEmpty (Expr, Expr)) Range
  | Fun [Var] Expr Range
  | Parens Expr
  | Block (NonEmpty Expr) Range
  | Chain (OpChain Expr)
  | ExtExpr Ext
  | Debug Expr Range
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Str _ r) = r
  getRange (Unit r) = r
  getRange (Obj _ r) = r
  getRange (Variant _ _ r) = r
  getRange (Tuple _ _ _ r) = r
  getRange (Id (Var _ r)) = r
  getRange (App _ _ r) = r
  getRange (Access expr prop) = getRange (expr, prop)
  getRange (Cond _ _ _ r) = r
  getRange (PatternMatch _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Parens expr) = getRange expr
  getRange (Block _ r) = r
  getRange (Chain chain) = getRange chain
  getRange (ExtExpr ext) = getRange ext
  getRange (Debug _ r) = r

data Defn
  = Defn (Bind () Expr)
  | FixDefn Fixity Var
  | DtypeDefn [VariantSpec]
  | EntryDefn Expr
  deriving (Show)

data Module = Module
  { definitions :: [Defn]
  }
  deriving (Show)
