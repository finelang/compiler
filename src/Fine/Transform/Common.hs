module Fine.Transform.Common (module Fine.Transform.Common) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Fine.Syntax (Fixity, Id)

type Fixities = Map Id Fixity

type Constructors = Set Id
