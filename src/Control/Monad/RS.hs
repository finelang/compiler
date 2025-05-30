module Control.Monad.RS where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, withReaderT)
import Control.Monad.Trans.State.Strict (State, evalState)

type RS r s a = ReaderT r (State s) a

withReader :: (r' -> r) -> RS r s a -> RS r' s a
withReader = withReaderT

runRS :: RS r s a -> r -> s -> a
runRS rs r s = evalState (runReaderT rs r) s
