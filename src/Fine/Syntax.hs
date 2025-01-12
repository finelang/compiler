module Fine.Syntax
  ( Pattern (..),
    PropsPattern (..),
    Expr (..),
    Closure (..),
    Module (..),
    boundVars,
    closureVars,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Fine.Syntax.Common
  ( Bind,
    Ext,
    Fixities,
    HasRange (..),
    Prop (..),
    Range,
    Var (Var),
    VariantSpecs,
  )

data PropsPattern = PropsPattern [(Var, Pattern)] (Maybe Var)
  deriving (Show)

data Pattern
  = IntPatt Int Range
  | FloatPatt Float Range
  | StrPatt Text Range
  | UnitPatt Range
  | ObjPatt PropsPattern Range
  | VariantPatt Var PropsPattern Range
  | TuplePatt Pattern Pattern [Pattern] Range
  | Capture Var
  deriving (Show)

instance HasRange Pattern where
  getRange :: Pattern -> Range
  getRange (IntPatt _ r) = r
  getRange (FloatPatt _ r) = r
  getRange (StrPatt _ r) = r
  getRange (UnitPatt r) = r
  getRange (ObjPatt _ r) = r
  getRange (VariantPatt _ _ r) = getRange r
  getRange (TuplePatt _ _ _ r) = r
  getRange (Capture (Var _ r)) = r

propsBoundVars :: PropsPattern -> [Var]
propsBoundVars (PropsPattern named objCapture) =
  let fromNamed = concat (map (boundVars . snd) named)
      fromObjCapture = maybeToList objCapture
   in fromObjCapture ++ fromNamed

boundVars :: Pattern -> [Var]
boundVars (IntPatt _ _) = []
boundVars (FloatPatt _ _) = []
boundVars (StrPatt _ _) = []
boundVars (UnitPatt _) = []
boundVars (ObjPatt props _) = propsBoundVars props
boundVars (VariantPatt _ props _) = propsBoundVars props
boundVars (TuplePatt fst' snd' rest _) = concat $ map boundVars (fst' : snd' : rest)
boundVars (Capture var) = [var]

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
  | ExtId Ext
  | ExtOpApp Ext Expr Expr
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
  getRange (ExtId ext) = getRange ext
  getRange (ExtOpApp ext _ _) = getRange ext
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
