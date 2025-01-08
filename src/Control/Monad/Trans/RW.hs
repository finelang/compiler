module Control.Monad.Trans.RW (module Control.Monad.Trans.RW) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as W

type RW r w a = ReaderT r (Writer w) a

ask :: (Monoid w) => RW r w r
ask = R.ask

asks :: (Monoid w) => (r -> a) -> RW r w a
asks = R.asks

tell :: (Monoid w) => w -> RW r w ()
tell = lift . W.tell

withReader :: (r' -> r) -> RW r w a -> RW r' w a
withReader = R.withReaderT

runRW :: RW r w a -> r -> (a, w)
runRW rw r = W.runWriter (R.runReaderT rw r)
