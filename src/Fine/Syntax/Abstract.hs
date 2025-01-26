module Fine.Syntax.Abstract
  ( Pattern (..),
    PropsPattern (..),
    Block (..),
    Expr (..),
    Closure (..),
    Module (..),
    boundVars,
    closureVars,
    justClosed,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (maybeToList)
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Syntax.Common
  ( Bind,
    Ext,
    Fixity,
    HasRange (..),
    Lit,
    Prop (..),
    Range,
    Var (Var),
  )

data PropsPattern = PropsPattern [(Var, Pattern)] (Maybe Var)
  deriving (Show)

data Pattern
  = LiteralPatt Lit Range
  | ObjPatt PropsPattern Range
  | VariantPatt Var PropsPattern Range
  | TuplePatt Pattern Pattern [Pattern] Range
  | Capture Var
  deriving (Show)

instance HasRange Pattern where
  getRange :: Pattern -> Range
  getRange (LiteralPatt _ r) = r
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
boundVars (LiteralPatt _ _) = []
boundVars (ObjPatt props _) = propsBoundVars props
boundVars (VariantPatt _ props _) = propsBoundVars props
boundVars (TuplePatt fst' snd' rest _) = concat $ map boundVars (fst' : snd' : rest)
boundVars (Capture var) = [var]

data Closure v = Closure
  { closureEnv :: Map Var v,
    closureValue :: v,
    _self :: Maybe Var
  }
  deriving (Show)

closureVars :: Closure v -> Set Var
closureVars (Closure env _ self) =
  case (M.keysSet env, self) of
    (vars, Nothing) -> vars
    (vars, Just another) -> S.insert another vars

instance (HasRange v) => HasRange (Closure v) where
  getRange :: Closure v -> Range
  getRange (Closure _ x _) = getRange x

data Block
  = Return Expr
  | Do Expr Block
  | Let Var () Expr Block
  deriving (Show)

data Expr
  = Literal Lit Range
  | Obj [Prop Expr] Range
  | Variant Var [Prop Expr] Range
  | Tuple Expr Expr [Expr] Range
  | Id Var
  | App Expr [Expr] Range
  | Access Expr Var
  | PatternMatch Expr (NonEmpty (Pattern, Expr)) Range
  | Cond Expr Expr Expr Range
  | Fun [Var] Expr Range
  | Block Block Range
  | ExtExpr Ext
  | Debug Expr Range
  | Closed (Closure Expr)
  deriving (Show)

instance HasRange Expr where
  getRange :: Expr -> Range
  getRange (Literal _ r) = r
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
  getRange (ExtExpr ext) = getRange ext
  getRange (Debug _ r) = r
  getRange (Closed cl) = getRange cl

justClosed :: Expr -> Maybe (Closure Expr)
justClosed (Closed cl) = Just cl
justClosed _ = Nothing

data Module
  = Module
      { bindings :: [Bind () Expr],
        fixities :: Map Var Fixity
      }
  | EntryModule
      { bindings :: [Bind () Expr],
        fixities :: Map Var Fixity,
        _entryExpr :: Expr
      }
  deriving (Show)
