{-# LANGUAGE QuasiQuotes #-}

module Error
  ( ErrorCollection,
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
import Syntax.Common (Binder (Binder), Fixity, Operator (Operator), Range)

errorTODO :: (HasCallStack) => a
errorTODO = error "Not Implemented"

errorUNREACHABLE :: (HasCallStack) => a
errorUNREACHABLE = error "This section of code should be unreachable"

type ErrorCollection e w = ([e], [w])

collectErrors :: [e] -> ErrorCollection e w
collectErrors errs = (errs, [])

collectWarnings :: [w] -> ErrorCollection e w
collectWarnings wrns = ([], wrns)

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
  show (UnusedVar (Binder name _)) = [i|Variable #{hl name} is not used.|]
  show (BindingShadowing (Binder name _)) = [i|The binding for #{hl name} shadows the existing binding.|]

instance Show SemanticError where
  show :: SemanticError -> String
  show (UndefinedVar name _) = [i|Variable #{hl name} is not in scope.|]
  show (RepeatedParam (Binder name _)) = [i|Parameter #{hl name} is repeated.|]
  show (SameInfixPrecedence (Operator _ _, _) (Operator _ _, _)) = errorTODO
