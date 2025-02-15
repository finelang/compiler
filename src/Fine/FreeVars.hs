module Fine.FreeVars (freeVars) where

import Data.List.NonEmpty (toList)
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Syntax.Abstract (Block (..), Expr (..), Pattern (..), boundVars)
import Fine.Syntax.Common (Id)

blockFreeVars :: Block -> Set Id
blockFreeVars (Return expr) = freeVars expr
blockFreeVars (Do expr block) = S.union (freeVars expr) (blockFreeVars block)
blockFreeVars (Debug expr block) = S.union (freeVars expr) (blockFreeVars block)
blockFreeVars (Let bound _ expr block) =
  S.union (freeVars expr) (S.delete bound $ blockFreeVars block)

patternFreeVars :: Pattern -> Set Id
patternFreeVars (LiteralP _ _) = S.empty
patternFreeVars (DataP tag patts _) = S.insert tag (foldMap patternFreeVars patts)
patternFreeVars (RecordP props _) = foldMap (patternFreeVars . snd) props
patternFreeVars (TupleP patts _) = foldMap patternFreeVars patts
patternFreeVars (Capture _) = S.empty

freeVars :: Expr -> Set Id
freeVars (Literal _ _) = S.empty
freeVars (Data _ exprs _) = foldMap freeVars exprs
freeVars (Record props _) = foldMap (freeVars . snd) props
freeVars (Tuple exprs _) = foldMap freeVars exprs
freeVars (Var var) = S.singleton var
freeVars (App f args _) = S.union (freeVars f) (foldMap freeVars args)
freeVars (Access expr _) = freeVars expr
freeVars (Index expr _ _) = freeVars expr
freeVars (Cond cond yes no _) = S.unions (map freeVars [cond, yes, no])
freeVars (PatternMatch expr matches _) =
  let contsFreeVars =
        foldMap
          (\(patt, cont) -> S.difference (freeVars cont) (S.fromList $ boundVars patt))
          matches
      pattsFreeVars = foldMap (patternFreeVars . fst) matches
   in S.unions [freeVars expr, pattsFreeVars, contsFreeVars]
freeVars (Fun params body _) = S.difference (freeVars body) (S.fromList $ toList params)
freeVars (Block block _) = blockFreeVars block
freeVars (ExtExpr _) = S.empty
freeVars (Closure _ _ _) = S.empty
