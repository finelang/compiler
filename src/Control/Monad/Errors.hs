module Control.Monad.Errors (
  Errors,
  runErrors,
  failure,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

data Errors e a = Errors (NonEmpty e) | Success a

runErrors :: Errors e a -> Either (NonEmpty e) a
runErrors (Success x) = Right x
runErrors (Errors es) = Left es

failure :: e -> Errors e a
failure = Errors . NonEmpty.singleton

instance Functor (Errors e) where
  fmap :: (a -> b) -> Errors e a -> Errors e b
  fmap f (Success x) = Success (f x)
  fmap _ (Errors es) = Errors es

instance Applicative (Errors e) where
  pure :: a -> Errors e a
  pure = Success

  (<*>) :: Errors e (a -> b) -> Errors e a -> Errors e b
  Success f <*> res = fmap f res
  Errors es <*> Errors es' = Errors (NonEmpty.append es es')
  Errors es <*> _ = Errors es

-- UNLAWFUL MONAD HERE
instance Monad (Errors e) where
  (>>) :: Errors e a -> Errors e b -> Errors e b
  (>>) = (*>)

  (>>=) :: Errors e a -> (a -> Errors e b) -> Errors e b
  Success x >>= f = f x
  Errors es >>= _ = Errors es
