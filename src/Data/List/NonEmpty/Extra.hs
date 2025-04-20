module Data.List.NonEmpty.Extra where

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))

unsnoc :: NonEmpty a -> ([a], a)
unsnoc (first :| rest) = case List.unsnoc rest of
  Just (middle, last') -> (first : middle, last')
  Nothing -> ([], first)
