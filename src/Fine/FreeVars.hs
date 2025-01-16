module Fine.FreeVars (freeVars) where

import qualified Data.List.NonEmpty as L
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Syntax (Expr (..), boundVars)
import Fine.Syntax.Common (Prop (..), Var)

propFreeVars :: Prop Expr -> Set Var
propFreeVars (NamedProp _ expr) = freeVars expr
propFreeVars (SpreadProp expr) = freeVars expr

freeVars :: Expr -> Set Var
freeVars (Int _ _) = S.empty
freeVars (Float _ _) = S.empty
freeVars (Str _ _) = S.empty
freeVars (Unit _) = S.empty
freeVars (Obj props _) = S.unions (map propFreeVars props)
freeVars (Variant _ props _) = S.unions (map propFreeVars props)
freeVars (Tuple fst' snd' rest _) = S.unions $ map freeVars (fst' : snd' : rest)
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
freeVars (Parens expr) = freeVars expr
freeVars (Block exprs _) = S.unions (fmap freeVars exprs)
freeVars (ExtId _) = S.empty
freeVars (ExtOpApp _ l r) = S.union (freeVars l) (freeVars r)
freeVars (Debug expr _) = freeVars expr
