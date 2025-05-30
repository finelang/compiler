module Fine.Typer.Eval (runEval) where

import Control.Monad.Trans.Reader (Reader, asks, local, runReader)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Fine.Syntax (Phase (Typed), Type (..))
import Fine.Typer.Common (Env)

eval :: Type Typed -> Reader (Env (Type Typed)) (Type Typed)
eval type'@(LiteralT _ _) = return type'
eval type'@(VoidT _) = return type'
eval (TupleT ext fst' snd' rest) =
  TupleT ext <$> eval fst' <*> eval snd' <*> mapM eval rest
eval (ListT ext type') = ListT ext <$> eval type'
eval (RecordT ext propTypes) = RecordT ext <$> (mapM . mapM) eval propTypes
eval (FunT ext argTypes retType) = FunT ext <$> mapM eval argTypes <*> eval retType
eval (Forall ext univars type') = Forall ext univars <$> eval type'
eval (DataT ext tag types) = DataT ext tag <$> mapM eval types
eval type'@(TVar _ var) = asks (Map.findWithDefault type' var)
eval (TApp ext tfun targs) = do
  tfun' <- eval tfun
  targs' <- mapM eval targs
  case tfun' of
    TFun _ tparams tbody -> do
      let localEnv = Map.fromList $ NonEmpty.toList $ NonEmpty.zip tparams targs'
      local (Map.union localEnv) (eval tbody)
    _ -> return $ TApp ext tfun' targs'
eval (TFun ext tparams tbody) = TFun ext tparams <$> eval tbody

runEval :: Env (Type Typed) -> Type Typed -> Type Typed
runEval env type' = runReader (eval type') env
