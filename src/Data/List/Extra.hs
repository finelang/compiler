module Data.List.Extra (module Data.List.Extra) where

import Data.Set (empty, insert, member)

repeated :: (Ord a) => [a] -> [a]
repeated xs = reverse (go xs empty)
  where
    go [] _ = []
    go (y : ys) s | member y s = y : go ys s
    go (y : ys) s = go ys (insert y s)
