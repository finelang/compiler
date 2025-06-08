module Control.Monad.Trans.Collector where

import Control.Monad.Trans.Writer.Strict (WriterT (runWriterT), tell)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (singleton)

type CollectorT c = WriterT [c]

collect :: (Monad m) => c -> CollectorT c m ()
collect = tell . singleton

runCollectorT :: (Monad m) => CollectorT c m a -> m (a, [c])
runCollectorT = runWriterT

type Collector c = CollectorT c Identity

runCollector :: Collector c a -> (a, [c])
runCollector = runIdentity . runCollectorT
