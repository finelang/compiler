module Data.List.NonEmpty2 (NonEmpty2 (..), toList) where

import Control.Applicative (liftA3)

data NonEmpty2 t = NonEmpty2 t t [t]
  deriving (Show)

toList :: NonEmpty2 a -> [a]
toList (NonEmpty2 x y zs) = x : y : zs

instance Semigroup (NonEmpty2 t) where
  (<>) :: NonEmpty2 t -> NonEmpty2 t -> NonEmpty2 t
  NonEmpty2 a b cs <> ds = NonEmpty2 a b (cs ++ toList ds)

instance Functor NonEmpty2 where
  fmap :: (a -> b) -> NonEmpty2 a -> NonEmpty2 b
  fmap f (NonEmpty2 x y zs) = NonEmpty2 (f x) (f y) (map f zs)

instance Foldable NonEmpty2 where
  foldMap :: (Monoid m) => (a -> m) -> NonEmpty2 a -> m
  foldMap f (NonEmpty2 x y zs) = f x <> f y <> foldMap f zs

instance Traversable NonEmpty2 where
  traverse :: (Applicative f) => (a -> f b) -> NonEmpty2 a -> f (NonEmpty2 b)
  traverse f (NonEmpty2 x y zs) = liftA3 NonEmpty2 (f x) (f y) (traverse f zs)
