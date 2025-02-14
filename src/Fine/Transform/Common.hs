module Fine.Transform.Common (module Fine.Transform.Common) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Fine.Syntax.Common (Fixity, Var)

type Fixities = Map Var Fixity

type CtBinders = Set Var
