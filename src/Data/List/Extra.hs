{-# OPTIONS_GHC -Wno-x-partial #-}

module Data.List.Extra (module Data.List.Extra) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set (empty, insert, member)
import GHC.Stack (HasCallStack)

repeated :: (Ord a) => [a] -> [a]
repeated xs = reverse (go xs empty)
  where
    go [] _ = []
    go (y : ys) s | member y s = y : go ys s
    go (y : ys) s = go ys (insert y s)

toNonEmptyPARTIAL :: (HasCallStack) => [a] -> NonEmpty a
toNonEmptyPARTIAL xs = head xs :| tail xs
