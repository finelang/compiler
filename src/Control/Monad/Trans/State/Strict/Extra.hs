module Control.Monad.Trans.State.Strict.Extra (module Control.Monad.Trans.State.Strict.Extra) where

import Control.Monad.Trans.State.Strict (StateT, get, put)

withTempState :: (Monad m) => (s -> s) -> StateT s m a -> StateT s m a
withTempState f comp = do
  s' <- get
  put (f s')
  res <- comp
  put s'
  return res
