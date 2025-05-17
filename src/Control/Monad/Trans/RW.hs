module Control.Monad.Trans.RW where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, withReaderT)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter)

type RW r w a = ReaderT r (Writer w) a

withReader :: (r' -> r) -> RW r w a -> RW r' w a
withReader = withReaderT

runRW :: RW r w a -> r -> (a, w)
runRW rw r = runWriter (runReaderT rw r)
