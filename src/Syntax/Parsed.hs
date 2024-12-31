module Syntax.Parsed
  ( Defn (..),
    Expr (..),
    Module (..),
    justFixDefn,
    justDataCtors,
    justBinds,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Syntax.Common (Bind, Ctor, Data, Ext, Fixity, HasRange (..), OpChain, Range, Var (Var))

data Expr
  = Int Int Range
  | Float Float Range
  | Str Text Range
  | Obj (Data Expr) Range
  | Variant Var (Data Expr) Range
  | Id Var
  | App Expr [Expr] Range
  | Cond Expr Expr Expr Range
  | Fun [Var] Expr Range
  | Parens Expr
  | Block (NonEmpty Expr) Range
  | Chain (OpChain Expr)
  | ExtExpr Ext
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Str _ r) = r
  getRange (Obj _ r) = r
  getRange (Variant _ _ r) = r
  getRange (Id (Var _ r)) = r
  getRange (App _ _ r) = r
  getRange (Cond _ _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Parens expr) = getRange expr
  getRange (Block _ r) = r
  getRange (Chain chain) = getRange chain
  getRange (ExtExpr ext) = getRange ext

data Defn
  = Defn (Bind () Expr)
  | FixDefn Fixity Var
  | DtypeDefn [Ctor]
  deriving (Show)

justBinds :: [Defn] -> [Bind () Expr]
justBinds [] = []
justBinds (Defn b : defns) = b : justBinds defns
justBinds (_ : defns) = justBinds defns

justFixDefn :: Defn -> Maybe (Fixity, Var)
justFixDefn (FixDefn fix op) = Just (fix, op)
justFixDefn _ = Nothing

justDataCtors :: Defn -> Maybe [Ctor]
justDataCtors (DtypeDefn ctors) = Just ctors
justDataCtors _ = Nothing

data Module = Module
  { definitions :: [Defn]
  }
  deriving (Show)
