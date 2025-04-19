module Control.Monad.Trans.RW where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.Writer.Strict (Writer)
import Control.Monad.Trans.Writer.Strict qualified as Writer

type RW r w a = ReaderT r (Writer w) a

ask :: (Monoid w) => RW r w r
ask = Reader.ask

asks :: (Monoid w) => (r -> a) -> RW r w a
asks = Reader.asks

tell :: (Monoid w) => w -> RW r w ()
tell = lift . Writer.tell

withReader :: (r' -> r) -> RW r w a -> RW r' w a
withReader = Reader.withReaderT

runRW :: RW r w a -> r -> (a, w)
runRW rw r = Writer.runWriter (Reader.runReaderT rw r)
