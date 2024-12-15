{-# LANGUAGE QuasiQuotes #-}

module Error
  ( ErrorCollection (..),
    SemanticWarning (..),
    SemanticError (..),
    showText,
    collectError,
    collectWarning,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Interpolate (i)
import Data.Text (Text, intercalate)
import Syntax.Common (Fixity, Operator (Operator), Range)

data ErrorCollection e w = ErrorCollection
  { collectedErrors :: [e],
    collectedWarnings :: [w]
  }

collectError :: e -> ErrorCollection e w
collectError err = ErrorCollection [err] []

collectWarning :: w -> ErrorCollection e w
collectWarning wrn = ErrorCollection [] [wrn]

instance Semigroup (ErrorCollection e w) where
  (<>) :: ErrorCollection e w -> ErrorCollection e w -> ErrorCollection e w
  (ErrorCollection es ws) <> (ErrorCollection es' ws') = ErrorCollection (es <> es') (ws <> ws')

instance Monoid (ErrorCollection e w) where
  mempty :: ErrorCollection e w
  mempty = ErrorCollection [] []

data SemanticWarning
  = MissingFixity Text Range

data SemanticError
  = UndefinedVar Text Range
  | RepeatedParams (NonEmpty Text) Range
  | SameInfixPrecedence (Operator, Fixity) (Operator, Fixity)

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
  showText (RepeatedParams params _) = [i|#{go params} repeated.|]
    where
      go :: NonEmpty Text -> String
      go (p :| []) = [i|Parameter #{hl p} is|]
      go (p :| ps) =
        let ps' = intercalate ", " $ map hl (p : init ps)
            p' = hl (last ps)
         in [i|Parameters #{ps'} and #{p'} are|]
  -- TODO
  showText (SameInfixPrecedence (Operator _ _, _) (Operator _ _, _)) =
    [i|TODO|]
