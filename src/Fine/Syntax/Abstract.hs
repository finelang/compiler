module Fine.Syntax.Abstract
  ( Pattern (..),
    Block (..),
    Expr (..),
    Bind,
    Module (..),
    boundVars,
    apply,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty2 (NonEmpty2)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Fine.Syntax.Common
  ( Bind,
    Ext,
    Fixity,
    HasRange (..),
    Id (Id),
    Lit,
    Range,
  )

data Pattern
  = LiteralP Lit Range
  | DataP Id [Pattern] Range
  | RecordP (NonEmpty (Id, Pattern)) Range
  | TupleP (NonEmpty2 Pattern) Range
  | Capture Id
  | DiscardP Range
  deriving (Show)

instance HasRange Pattern where
  range :: Pattern -> Range
  range (LiteralP _ r) = r
  range (DataP _ _ r) = r
  range (RecordP _ r) = r
  range (TupleP _ r) = r
  range (Capture (Id _ r)) = r
  range (DiscardP r) = r

boundVars :: Pattern -> [Id]
boundVars (LiteralP _ _) = []
boundVars (DataP _ patts _) = concatMap boundVars patts
boundVars (RecordP props _) = foldMap (boundVars . snd) props
boundVars (TupleP patts _) = foldMap boundVars patts
boundVars (Capture idn) = [idn]
boundVars (DiscardP _) = []

data Block
  = Return Expr
  | Do Expr Block
  | Let Bool Id () Expr Block
  | Debug Expr Range Block
  | Void -- internal
  | Loop Expr Block Block -- internal
  deriving (Show)

data Expr
  = Literal Lit Range
  | Data Id [Expr] Range
  | Record (NonEmpty (Id, Expr)) Range
  | Tuple (NonEmpty2 Expr) Range
  | Var Id
  | Mut Id Expr
  | App Expr [Expr] Range
  | Access Expr Id
  | Index Expr Int Range
  | PatternMatch Expr (NonEmpty (Pattern, Expr)) Range
  | Cond Expr Expr Expr Range
  | Fun [Id] Expr Range
  | Block Block Range
  | ExtExpr Ext
  | Closure (Set Id) Expr (Maybe Id)
  deriving (Show)

apply :: (Expr -> Expr) -> Expr -> Expr
apply _ expr@(Literal _ _) = expr
apply f (Data tag exprs r) = Data tag (map f exprs) r
apply f (Record props r) = Record ((fmap . fmap) f props) r
apply f (Tuple exprs r) = Tuple (fmap f exprs) r
apply _ expr@(Var _) = expr
apply f (Mut var expr) = Mut var (f expr)
apply f (App f' args r) = App (f f') (map f args) r
apply f (Access expr prop) = Access (f expr) prop
apply f (Index expr ix r) = Index (f expr) ix r
apply f (PatternMatch expr matches r) = PatternMatch (f expr) ((fmap . fmap) f matches) r
apply f (Cond cond yes no r) = Cond (f cond) (f yes) (f no) r
apply f (Fun params body r) = Fun params (f body) r
apply f (Block block' r') = Block (apply2block block') r'
  where
    apply2block (Return expr) = Return (f expr)
    apply2block (Do expr block) = Do (f expr) (apply2block block)
    apply2block (Let isMut binder t expr block) =
      Let isMut binder t (f expr) (apply2block block)
    apply2block (Debug expr r block) = Debug (f expr) r (apply2block block)
    apply2block Void = Void
    apply2block (Loop cond actions block) =
      Loop (f cond) (apply2block actions) (apply2block block)
apply _ expr@(ExtExpr _) = expr
apply f (Closure env expr self) = Closure env (f expr) self

instance HasRange Expr where
  range :: Expr -> Range
  range (Literal _ r) = r
  range (Data _ _ r) = r
  range (Record _ r) = r
  range (Tuple _ r) = r
  range (Var var) = range var
  range (Mut var expr) = range var <> range expr
  range (App _ _ r) = r
  range (Access expr prop) = range expr <> range prop
  range (Index _ _ r) = r
  range (Cond _ _ _ r) = r
  range (PatternMatch _ _ r) = r
  range (Fun _ _ r) = r
  range (Block _ r) = r
  range (ExtExpr ext) = range ext
  range (Closure _ expr _) = range expr

data Module
  = Module
      { bindings :: [Bind () Expr],
        fixities :: Map Id Fixity
      }
  | EntryModule
      { bindings :: [Bind () Expr],
        fixities :: Map Id Fixity,
        _entryExpr :: Expr
      }
  deriving (Show)
