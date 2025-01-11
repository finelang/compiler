module Fine.Syntax.Expr (Expr (..), Closure (..), Module (..), closureVars) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Fine.Syntax.Common (Bind, Ext, Fixities, HasRange (..), Prop (..), Range, Var (Var), VariantSpecs)
import Fine.Syntax.Pattern (Pattern)

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
  | PatternMatch Expr (NonEmpty (Pattern, Expr)) Range
  | Cond Expr Expr Expr Range
  | Fun [Var] Expr Range
  | Parens Expr
  | Block (NonEmpty Expr) Range
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
  getRange (Block _ r) = r
  getRange (Parens expr) = getRange expr
  getRange (ExtExpr ext) = getRange ext
  getRange (Debug _ r) = r

data Closure v = Closure
  { closureEnv :: Map Var (Closure v),
    closureValue :: v,
    recBinder :: Maybe Var
  }
  deriving (Show)

closureVars :: Closure v -> [Var]
closureVars (Closure env _ bder) = M.keys env ++ maybeToList bder

data Module
  = Module
      { bindings :: [Bind () (Closure Expr)],
        fixities :: Fixities,
        variantSpecs :: VariantSpecs
      }
  | EntryModule
      { bindings :: [Bind () (Closure Expr)],
        fixities :: Fixities,
        variantSpecs :: VariantSpecs,
        _entryExpr :: Closure Expr
      }
  deriving (Show)
