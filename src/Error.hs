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

import Data.String.Interpolate (i)
import Data.Text (Text)
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
  | BindingShadowing Binder

data SemanticError
  = UndefinedVar Text Range
  | RepeatedParam Binder
  | SameInfixPrecedence (Operator, Fixity) (Operator, Fixity)

hl :: Text -> Text
hl text = [i|'#{text}'|]

instance Show SemanticWarning where
  show :: SemanticWarning -> String
  show (MissingFixity name _) = [i|The operator #{hl name} is missing a fixity definition.|]
  show (UnusedVar b) = [i|Variable #{hl $ binderName b} is not used.|]
  show (BindingShadowing b) = [i|The binding for #{hl $ binderName b} shadows the existing binding.|]

instance Show SemanticError where
  show :: SemanticError -> String
  show (UndefinedVar name _) = [i|Variable #{hl name} is not in scope.|]
  show (RepeatedParam b) = [i|Parameter #{hl $ binderName b} is repeated.|]
  show (SameInfixPrecedence (Operator _ _, _) (Operator _ _, _)) = errorTODO
