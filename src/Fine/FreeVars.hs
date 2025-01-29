module Fine.FreeVars (freeVars) where

import qualified Data.List.NonEmpty as L
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Syntax.Abstract (Block (..), Expr (..), boundVars)
import Fine.Syntax.Common (Prop (..), Var)

propFreeVars :: Prop Expr -> Set Var
propFreeVars (NamedProp _ expr) = freeVars expr
propFreeVars (SpreadProp expr) = freeVars expr

blockFreeVars :: Block -> Set Var
blockFreeVars (Return expr) = freeVars expr
blockFreeVars (Do expr block) = S.union (freeVars expr) (blockFreeVars block)
blockFreeVars (Let bound _ expr block) =
  S.union (freeVars expr) (S.delete bound $ blockFreeVars block)

freeVars :: Expr -> Set Var
freeVars (Literal _ _) = S.empty
freeVars (Obj props _) = S.unions (map propFreeVars props)
freeVars (Variant tag props _) = S.insert tag $ S.unions (map propFreeVars props)
freeVars (Tuple exprs _) = S.unions $ fmap freeVars exprs
freeVars (Id var) = S.singleton var
freeVars (App f args _) = S.unions $ map freeVars (f : args)
freeVars (Access expr _) = freeVars expr
freeVars (Cond cond yes no _) = S.unions (map freeVars [cond, yes, no])
freeVars (PatternMatch expr matches _) =
  let (patts, exprs) = unzip (L.toList matches)
      boundVarsList = map (S.fromList . boundVars) patts
      freeVarsList = map freeVars exprs
   in S.unions (freeVars expr : zipWith S.difference freeVarsList boundVarsList)
freeVars (Fun params body _) = S.difference (freeVars body) (S.fromList params)
freeVars (Block block _) = blockFreeVars block
freeVars (ExtExpr _) = S.empty
freeVars (Debug expr _) = freeVars expr
freeVars (Closed _) = S.empty
