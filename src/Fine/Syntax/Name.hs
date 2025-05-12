module Fine.Syntax.Name where

import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Fine.Error (errorUNREACHABLE)
import Fine.Syntax (Id (Id), Range (NoRange))

matchedVar :: Maybe Range -> Id
matchedVar Nothing = Id NoRange "$"
matchedVar (Just r) = Id r "$"

param :: Maybe Range -> Int -> Id
param Nothing x = Id NoRange [i|x#{x}|]
param (Just r) x = Id r [i|x#{x}|]

tagProp :: Id
tagProp = Id NoRange "$tag"

lengthProp :: Id
lengthProp = Id NoRange "length"

irrelevant :: Id
irrelevant = Id NoRange "_"

isRelevant :: Id -> Bool
isRelevant (Id _ name) = case Text.uncons name of
  Just (ch', _) -> ch' /= '_'
  _ -> errorUNREACHABLE "Found a variable with empty name."
