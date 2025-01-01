module Data.List.NonEmpty.Extra (module Data.List.NonEmpty.Extra) where

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty ((:|)))

unsnoc :: NonEmpty a -> ([a], a)
unsnoc (x :| xs) = case L.unsnoc xs of
  Just (xs', x') -> (x : xs', x')
  Nothing -> ([], x)
