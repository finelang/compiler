module Control.Monad.Trans.RW (module Control.Monad.Trans.RW) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.Writer (Writer)
import qualified Control.Monad.Trans.Writer as W

type RW r w a = ReaderT r (Writer w) a

ask :: (Monoid w) => RW r w r
ask = R.ask

tell :: (Monoid w) => w -> RW r w ()
tell = lift . W.tell

runRW :: RW r w a -> r -> (a, w)
runRW rw r = W.runWriter (R.runReaderT rw r)
