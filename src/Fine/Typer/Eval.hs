module Fine.Typer.Eval (runEval) where

import Control.Monad.Trans.Reader (Reader, asks, local, runReader)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Fine.Syntax (Name, Phase (Typed), Type (..))
import Fine.Syntax.Utils (unqualified)

eval :: Type Typed -> Reader (Map Name (Type Typed)) (Type Typed)
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
    TFun _ tp tb -> local (Map.insert (unqualified tp) ta') (eval tb)
    _ -> pure $ TApp k tf' ta'
eval (TFun k tp tb) = TFun k tp <$> eval tb

runEval :: Map Name (Type Typed) -> Type Typed -> Type Typed
runEval env type' = runReader (eval type') env
