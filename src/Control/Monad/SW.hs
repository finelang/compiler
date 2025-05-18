module Control.Monad.SW (module Control.Monad.SW) where

import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.Writer.Strict (Writer, runWriter)

type SW s w a = StateT s (Writer w) a

runSW :: (SW s w a) -> s -> (a, s, w)
runSW sw s =
  let ((x, s'), w) = runWriter (runStateT sw s)
   in (x, s', w)
