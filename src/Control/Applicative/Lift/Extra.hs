module Control.Applicative.Lift.Extra where

import Control.Applicative.Lift (Errors)

success :: (Monoid e) => a -> Errors e a
success = pure
