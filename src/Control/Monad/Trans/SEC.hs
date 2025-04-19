module Control.Monad.Trans.SEC where

import Control.Monad.ErrorCollector (ErrorCollector)
import Control.Monad.ErrorCollector qualified as ErrorCollector
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.State.Strict qualified as State
import Data.List.NonEmpty (NonEmpty)

type SEC s e w a = StateT s (ErrorCollector e w) a

get :: SEC s e w s
get = State.get

gets :: (s -> s') -> SEC s e w s'
gets = State.gets

modify :: (s -> s) -> SEC s e w ()
modify = State.modify'

fail' :: e -> SEC s e w ()
fail' = lift . ErrorCollector.fail'

warn :: w -> SEC s e w ()
warn = lift . ErrorCollector.warn

runSEC :: SEC s e w a -> s -> (Either (NonEmpty e) a, [w])
runSEC sec' s = ErrorCollector.runErrorCollector (State.evalStateT sec' s)
