module Fine.Syntax.Expr (Expr (..), Closure (..), Module (..), closureVars) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Fine.Syntax.Common (Bind, Data, Ext, Fixities, HasRange (..), Range, Var (Var), VariantSpecs)

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
  | ExtExpr Ext
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Int _ r) = r
  getRange (Float _ r) = r
  getRange (Str _ r) = r
  getRange (Obj _ r) = r
  getRange (Variant _ _ r) = getRange r
  getRange (Id (Var _ r)) = r
  getRange (App _ _ r) = r
  getRange (Cond _ _ _ r) = r
  getRange (Fun _ _ r) = r
  getRange (Block _ r) = r
  getRange (Parens expr) = getRange expr
  getRange (ExtExpr ext) = getRange ext

data Closure v = Closure
  { closureEnv :: Map Var (Closure v),
    closureValue :: v,
    recBinder :: Maybe Var
  }
  deriving (Show)

closureVars :: Closure v -> [Var]
closureVars (Closure env _ bder) = M.keys env ++ maybeToList bder

data Module = Module
  { bindings :: [Bind () (Closure Expr)],
    fixities :: Fixities,
    variantSpecs :: VariantSpecs
  }
  deriving (Show)
