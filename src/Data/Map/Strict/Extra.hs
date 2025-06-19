module Data.Map.Strict.Extra where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import GHC.Err.Extra (errorUNREACHABLE)
import GHC.Stack (HasCallStack)

find :: (Ord k, Show k, HasCallStack) => k -> Map k v -> v
find key map' = case Map.lookup key map' of
  Just value -> value
  Nothing -> errorUNREACHABLE [i|'#{show key}' should be in the map.|]
