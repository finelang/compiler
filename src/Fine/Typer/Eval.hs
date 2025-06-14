module Fine.Typer.Eval (runEval) where

import Control.Monad.Trans.Reader (Reader, asks, local, runReader)
import Data.Map.Strict qualified as Map
import Fine.Syntax (Phase (Typed), Type (..))
import Fine.Typer.Common (Env)

eval :: Type Typed -> Reader (Env (Type Typed)) (Type Typed)
eval type'@(LiteralT _ _ _) = pure type'
eval type'@(VoidT _ _) = pure type'
eval (TupleT k r fst' snd' rest) =
  TupleT k r <$> eval fst' <*> eval snd' <*> mapM eval rest
eval (RecordT k r propTypes) = RecordT k r <$> (mapM . mapM) eval propTypes
eval (FunT k at bt) = FunT k <$> eval at <*> eval bt
eval (Forall k r univars type') = Forall k r univars <$> eval type'
eval (DataT k tag types) = DataT k tag <$> mapM eval types
eval type'@(TVar _ var) = asks (Map.findWithDefault type' var)
eval (TApp k tf ta) = do
  tf' <- eval tf
  ta' <- eval ta
  case tf' of
    TFun _ tp tb -> local (Map.insert tp ta') (eval tb)
    _ -> pure $ TApp k tf' ta'
eval (TFun k tp tb) = TFun k tp <$> eval tb

runEval :: Env (Type Typed) -> Type Typed -> Type Typed
runEval env type' = runReader (eval type') env
