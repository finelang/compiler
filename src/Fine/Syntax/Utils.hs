module Fine.Syntax.Utils (module Fine.Syntax.Utils) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Fine.Syntax (Expr (App, Fun), Id, Pattern (..))

flattenApp :: Expr p -> Maybe (Expr p, NonEmpty (Expr p))
flattenApp expr = (fmap . fmap) NonEmpty.reverse (go expr)
  where
    go (App _ app@(App _ _ _) arg) = (fmap . fmap) (NonEmpty.cons arg) (go app)
    go (App _ f arg) = Just (f, NonEmpty.singleton arg)
    go _ = Nothing

flattenFun :: Expr p -> Maybe (Expr p, NonEmpty Id)
flattenFun expr = go expr
  where
    go (Fun _ param fun@(Fun _ _ _)) = (fmap . fmap) (NonEmpty.cons param) (go fun)
    go (Fun _ param body) = Just (body, NonEmpty.singleton param)
    go _ = Nothing

boundVars :: Pattern -> [Id]
boundVars (LiteralP _ _) = []
boundVars (DataP _ _ patts) = concatMap boundVars patts
boundVars (RecordP _ props) = foldMap (boundVars . snd) props
boundVars (TupleP _ patts) = foldMap boundVars patts
boundVars (Capture _ idn) = [idn]
boundVars (Discard _) = []
