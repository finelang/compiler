module Control.Monad.Trans.SW (module Control.Monad.Trans.SW) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer

type SW s w a = StateT s (Writer w) a

get :: (Monoid w) => SW s w s
get = State.get

gets :: (Monoid w) => (s -> a) -> SW s w a
gets = State.gets

modify :: (Monoid w) => (s -> s) -> SW s w ()
modify = State.modify'

tell :: (Monoid w) => w -> SW s w ()
tell = lift . Writer.tell

runSW :: (SW s w a) -> s -> (a, s, w)
runSW sw s =
  let ((x, s'), w) = Writer.runWriter (State.runStateT sw s)
   in (x, s', w)
