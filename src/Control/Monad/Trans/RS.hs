module Control.Monad.Trans.RS (module Control.Monad.Trans.RS) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State

type RS r s a = ReaderT r (State s) a

ask :: RS r s r
ask = Reader.ask

asks :: (r -> a) -> RS r s a
asks = Reader.asks

local :: (r -> r) -> RS r s a -> RS r s a
local = Reader.local

withReader :: (r' -> r) -> RS r s a -> RS r' s a
withReader = Reader.withReaderT

get :: RS r s s
get = lift State.get

put :: s -> RS r s ()
put = lift . State.put

runRS :: RS r s a -> r -> s -> a
runRS rs r s = State.evalState (Reader.runReaderT rs r) s
