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
import Syntax.Common (Bind, Data, Fixity, HasRange (..), OpChain, Range, Var (Var))

data Expr
  = Int Int Range
  | Float Float Range
  | Str Text Range
  | Obj (Data Expr) Range
  | Id Var
  | App Expr [Expr] Range
  | Cond Expr Expr Expr Range
  | Fun [Var] Expr Range
  | Ctor Var [Var]
  | Parens Expr
  | Block (NonEmpty Expr) Range
  | Chain (OpChain Expr)
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Str _ r) = r
  getRange (Obj _ r) = r
  getRange (Id (Var _ r)) = r
  getRange (App _ _ r) = r
  getRange (Cond _ _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Ctor v _) = getRange v
  getRange (Parens expr) = getRange expr
  getRange (Block _ r) = r
  getRange (Chain chain) = getRange chain

data Defn
  = Defn (Bind () Expr)
  | FixDefn Fixity Var
  | DtypeDefn [Bind () Expr]
  deriving (Show)

justBinds :: [Defn] -> [Bind () Expr]
justBinds [] = []
justBinds (Defn b : defns) = b : justBinds defns
justBinds (DtypeDefn bs : defns) = bs ++ justBinds defns
justBinds (_ : defns) = justBinds defns

justFixDefn :: Defn -> Maybe (Fixity, Var)
justFixDefn (FixDefn fix op) = Just (fix, op)
justFixDefn _ = Nothing

justDataCtors :: Defn -> Maybe [Bind () Expr]
justDataCtors (DtypeDefn ctors) = Just ctors
justDataCtors _ = Nothing

data Module = Module
  { definitions :: [Defn]
  }
  deriving (Show)
