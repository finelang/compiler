module Control.Monad.Trans.REC where

import Control.Monad.ErrorCollector (ErrorCollector)
import Control.Monad.ErrorCollector qualified as ErrorCollector
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Reader qualified as Reader
import Data.List.NonEmpty (NonEmpty)

type REC r e w a = ReaderT r (ErrorCollector e w) a

asks :: (r -> a) -> REC r e w a
asks = Reader.asks

withReader :: (r' -> r) -> REC r e w a -> REC r' e w a
withReader = Reader.withReaderT

fail' :: e -> REC r e w ()
fail' = lift . ErrorCollector.fail'

warn :: w -> REC r e w ()
warn = lift . ErrorCollector.warn

runREC :: REC r e w a -> r -> (Either (NonEmpty e) a, [w])
runREC rec' r = ErrorCollector.runErrorCollector (Reader.runReaderT rec' r)
