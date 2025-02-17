module Fine.Transform.Pattern (runTransform) where

import Control.Monad.Trans.RW (RW, asks, runRW, tell)
import Data.List.NonEmpty (toList)
import qualified Data.Set as S
import Fine.Error (Error (InvalidPattern), Errors, collectError)
import Fine.Syntax.Abstract (Pattern (..))
import Fine.Syntax.Common (Lit (Unit), getRange)
import Fine.Syntax.Concrete (Expr (..))
import Fine.Transform.Common (CtBinders)

invalidPattern :: Expr -> RW r Errors Pattern
invalidPattern expr = do
  let r = getRange expr
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
    then return (DataP var [] (getRange var))
    else return (Capture var)
transform app@(App (Var var) args r) = do
  isCt <- asks (S.member var)
  if isCt
    then do
      patts <- mapM transform args
      return (DataP var (toList patts) r)
    else invalidPattern app
transform (Discard r) = return (DiscardP r)
transform other = invalidPattern other

runTransform :: CtBinders -> Expr -> (Pattern, Errors)
runTransform cts expr = runRW (transform expr) cts
