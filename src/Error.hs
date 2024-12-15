{-# LANGUAGE QuasiQuotes #-}

module Error (ErrorCollection (..), SemanticWarning (..), SemanticError (..), showText) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Interpolate (i)
import Data.Text (Text, intercalate)
import Syntax.Common (Range)

data ErrorCollection e w = ErrorCollection
  { errors :: [e],
    warnings :: [w]
  }

data SemanticWarning
  = MissingFixity Text Range

data SemanticError
  = UndefinedVar Text Range
  | RepeatedParams (NonEmpty Text) Range

hl :: Text -> Text
hl text = [i|'#{text}'|]

class ShowText t where
  showText :: t -> Text

instance ShowText SemanticWarning where
  showText :: SemanticWarning -> Text
  showText (MissingFixity name _) = [i|The operator #{hl name} is missing a fixity definition.|]

instance ShowText SemanticError where
  showText :: SemanticError -> Text
  showText (UndefinedVar name _) = [i|Value bound to #{hl name} is not in scope.|]
  showText (RepeatedParams params _) = [i|#{go params} are repeated.|]
    where
      go :: NonEmpty Text -> String
      go (p :| []) = [i|Parameter #{hl p}|]
      go (p :| ps) =
        let ps' = intercalate ", " $ map hl (p : init ps)
            p' = hl (last ps)
         in [i|Parameters #{ps'} and #{p'}|]
