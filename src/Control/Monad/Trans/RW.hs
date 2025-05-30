module Control.Monad.Trans.RW where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Writer.Strict (Writer)
import Control.Monad.Trans.Writer.Strict qualified as Writer

type RW r w a = ReaderT r (Writer w) a

tell :: (Monoid w) => w -> RW r w ()
tell = lift . Writer.tell

runRW :: RW r w a -> r -> (a, w)
runRW rw r = Writer.runWriter (runReaderT rw r)
