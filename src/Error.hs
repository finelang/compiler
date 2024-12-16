{-# LANGUAGE QuasiQuotes #-}

module Error
  ( ErrorCollection (..),
    SemanticWarning (..),
    SemanticError (..),
    collectErrors,
    collectWarnings,
    errorTODO,
    errorUNREACHABLE,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.String.Interpolate (i)
import Data.Text (Text, intercalate)
import GHC.Stack (HasCallStack)
import Syntax.Common (Binder (binderName), Fixity, Operator (Operator), Range)

errorTODO :: (HasCallStack) => a
errorTODO = error "Not Implemented"

errorUNREACHABLE :: (HasCallStack) => a
errorUNREACHABLE = error "This section of code should be unreachable"

data ErrorCollection e w = ErrorCollection
  { collectedErrors :: [e],
    collectedWarnings :: [w]
  }

collectErrors :: [e] -> ErrorCollection e w
collectErrors errs = ErrorCollection errs []

collectWarnings :: [w] -> ErrorCollection e w
collectWarnings = ErrorCollection []

instance Semigroup (ErrorCollection e w) where
  (<>) :: ErrorCollection e w -> ErrorCollection e w -> ErrorCollection e w
  (ErrorCollection es ws) <> (ErrorCollection es' ws') = ErrorCollection (es <> es') (ws <> ws')

instance Monoid (ErrorCollection e w) where
  mempty :: ErrorCollection e w
  mempty = ErrorCollection [] []

data SemanticWarning
  = MissingFixity Text Range
  | UnusedVar Binder

data SemanticError
  = UndefinedVar Text Range
  | RepeatedParams (NonEmpty Binder)
  | SameInfixPrecedence (Operator, Fixity) (Operator, Fixity)

hl :: Text -> Text
hl text = [i|'#{text}'|]

instance Show SemanticWarning where
  show :: SemanticWarning -> String
  show (MissingFixity name _) = [i|The operator #{hl name} is missing a fixity definition.|]
  show (UnusedVar b) = [i|Variable #{hl $ binderName b} is not used.|]

instance Show SemanticError where
  show :: SemanticError -> String
  show (UndefinedVar name _) = [i|Variable #{hl name} is not in scope.|]
  show (RepeatedParams params) = [i|#{go params} repeated.|]
    where
      go :: NonEmpty Binder -> String
      go (p :| []) = [i|Parameter #{hl $ binderName p} is|]
      go (p :| ps) =
        let ps' = intercalate ", " $ map (hl . binderName) (p : init ps)
            p' = hl $ binderName (last ps)
         in [i|Parameters #{ps'} and #{p'} are|]
  show (SameInfixPrecedence (Operator _ _, _) (Operator _ _, _)) = errorTODO
