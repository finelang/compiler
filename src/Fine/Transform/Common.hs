module Fine.Transform.Common (module Fine.Transform.Common) where

import Data.Map.Strict (Map)
import Fine.Syntax.Common (Fixity, Var)

type Fixities = Map Var Fixity

data VariantSpec = VariantSpec Var [Var]

type VariantSpecs = Map Var VariantSpec
