module Fine.Syntax.Name where

import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Fine.Syntax (Id (Id), Range (NoRange))
import GHC.Err.Extra (errorUNREACHABLE)

matchedVar :: Range -> Id
matchedVar r = Id r "$"

matchedParam :: Range -> Id
matchedParam r = Id r "$param"

param :: Range -> Int -> Id
param r x = Id r [i|x#{x}|]

tagProp :: Id
tagProp = Id NoRange "$tag"

irrelevant :: Id
irrelevant = Id NoRange "_"

isRelevant :: Id -> Bool
isRelevant (Id _ name) = case Text.uncons name of
  Just (ch', _) -> ch' /= '_'
  _ -> errorUNREACHABLE "Found a variable with empty name."
