{-# LANGUAGE QuasiQuotes #-}

module Error
  ( Errors,
    Warning (..),
    Error (..),
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

type Errors = ([Error], [Warning])

collectErrors :: [Error] -> Errors
collectErrors errs = (errs, [])

collectWarnings :: [Warning] -> Errors
collectWarnings wrns = ([], wrns)

data Warning
  = UnusedVar Binder
  | BindShadowing Binder

data Error
  = UndefinedVar Text Range
  | RepeatedParam Binder
  | InvalidPrecedence Text Range
  | SameInfixPrecedence (Operator, Fixity) (Operator, Fixity)

hl :: Text -> Text
hl text = [i|'#{text}'|]

instance Show Warning where
  show :: Warning -> String
  show (UnusedVar (Binder name _)) = [i|Variable #{hl name} is not used.|]
  show (BindShadowing (Binder name _)) = [i|The binding for #{hl name} shadows the existing binding.|]

instance Show Error where
  show :: Error -> String
  show (UndefinedVar name _) = [i|Variable #{hl name} is not in scope.|]
  show (RepeatedParam (Binder name _)) = [i|Parameter #{hl name} is repeated.|]
  show (InvalidPrecedence _ _) = errorTODO
  show (SameInfixPrecedence (Operator _ _, _) (Operator _ _, _)) = errorTODO
