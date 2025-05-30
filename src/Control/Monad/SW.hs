module Control.Monad.SW (module Control.Monad.SW) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Control.Monad.Trans.Writer.Strict (Writer)
import Control.Monad.Trans.Writer.Strict qualified as Writer

type SW s w a = StateT s (Writer w) a

tell :: (Monoid w) => w -> SW r w ()
tell = lift . Writer.tell

runSW :: (Monoid w) => (SW s w a) -> s -> (a, w)
runSW sw s = Writer.runWriter (evalStateT sw s)
