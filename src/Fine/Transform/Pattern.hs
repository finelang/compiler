module Fine.Transform.Pattern (runTransform) where

import Control.Monad.Trans.RW (RW, ask, asks, runRW, tell)
import Data.List.NonEmpty (toList)
import qualified Data.Set as S
import Fine.Error (Error (InvalidPattern), Errors, collectError)
import Fine.Syntax.Abstract (Pattern (..))
import Fine.Syntax.Common (Lit (Unit), range)
import Fine.Syntax.Concrete (Expr (..), flattenApp)
import Fine.Transform.Common (CtBinders)

invalidPattern :: Expr -> RW r Errors Pattern
invalidPattern expr = do
  let r = range expr
  tell (collectError $ InvalidPattern r) >> return (LiteralP Unit r)

transform :: Expr -> RW CtBinders Errors Pattern
transform (Literal lit r) = return (LiteralP lit r)
transform (Record props r) = do
  props' <- (mapM . mapM) transform props
  return (RecordP props' r)
transform (Tuple exprs r) = do
  patts <- mapM transform exprs
  return (TupleP patts r)
transform (Var var) = do
  isCt <- asks (S.member var)
  if isCt
    then return (DataP var [] (range var))
    else return (Capture var)
transform app@(App _ _) = do
  cts <- ask
  case flattenApp app of
    Just (Var tag, exprs) | S.member tag cts -> do
      patts <- mapM transform exprs
      return (DataP tag (toList patts) (range app))
    _ -> invalidPattern app
transform (Discard r) = return (DiscardP r)
transform other = invalidPattern other

runTransform :: CtBinders -> Expr -> (Pattern, Errors)
runTransform cts expr = runRW (transform expr) cts
