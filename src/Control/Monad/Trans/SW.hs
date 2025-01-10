module Control.Monad.Trans.SW (module Control.Monad.Trans.SW) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as W

type SW s w a = StateT s (Writer w) a

get :: (Monoid w) => SW s w s
get = S.get

gets :: (Monoid w) => (s -> a) -> SW s w a
gets = S.gets

modify :: (Monoid w) => (s -> s) -> SW s w ()
modify = S.modify'

tell :: (Monoid w) => w -> SW s w ()
tell = lift . W.tell

runSW :: (SW s w a) -> s -> (a, s, w)
runSW sw s =
  let ((x, s'), w) = W.runWriter (S.runStateT sw s)
   in (x, s', w)
