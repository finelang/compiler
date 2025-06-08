module Fine.Typer.Common where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Fine.Error (errorUNREACHABLE)
import Fine.Syntax (Id)

type Env a = Map Id a

fromEnv :: Id -> Env a -> a
fromEnv var env = case Map.lookup var env of
  Just value -> value
  Nothing -> errorUNREACHABLE [i|'#{var}' should be in environment.|]
