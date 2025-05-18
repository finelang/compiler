module Control.Monad.SEC where

import Control.Monad.ErrorCollector (ErrorCollector)
import Control.Monad.ErrorCollector qualified as ErrorCollector
import Control.Monad.State.Strict (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty)

type SEC s e w a = StateT s (ErrorCollector e w) a

fail' :: e -> SEC s e w ()
fail' = lift . ErrorCollector.fail'

warn :: w -> SEC s e w ()
warn = lift . ErrorCollector.warn

runSEC :: SEC s e w a -> s -> (Either (NonEmpty e) a, [w])
runSEC sec' s = ErrorCollector.runErrorCollector (evalStateT sec' s)
