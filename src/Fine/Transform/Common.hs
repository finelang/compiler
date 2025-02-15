module Fine.Transform.Common (module Fine.Transform.Common) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Fine.Syntax.Common (Fixity, Id)

type Fixities = Map Id Fixity

type CtBinders = Set Id
