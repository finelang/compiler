module Control.Monad.REC where

import Control.Monad.ErrorCollector (ErrorCollector)
import Control.Monad.ErrorCollector qualified as ErrorCollector
import Control.Monad.Reader (ReaderT, runReaderT, withReaderT)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty)

type REC r e w a = ReaderT r (ErrorCollector e w) a

withReader :: (r' -> r) -> REC r e w a -> REC r' e w a
withReader = withReaderT

fail' :: e -> REC r e w ()
fail' = lift . ErrorCollector.fail'

warn :: w -> REC r e w ()
warn = lift . ErrorCollector.warn

runREC :: REC r e w a -> r -> (Either (NonEmpty e) a, [w])
runREC rec' r = ErrorCollector.runErrorCollector (runReaderT rec' r)
