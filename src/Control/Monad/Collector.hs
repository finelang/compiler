module Control.Monad.Collector (
  Collector,
  runCollector,
  collect,
) where

import Data.Tuple (swap)

newtype Collector c a = Collector {runCollector' :: ([c], a)}
  deriving (Functor, Applicative, Monad)

runCollector :: Collector c b -> (b, [c])
runCollector = swap . runCollector'

collect :: c -> Collector c ()
collect x = Collector ([x], ())
