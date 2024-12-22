{-# LANGUAGE QuasiQuotes #-}

module Error
  ( Errors,
    Warning (..),
    Error (..),
    collectErrors,
    collectWarnings,
    errorTODO,
    errorUNREACHABLE,
    wrapWarning,
    wrapError,
  )
where

import Data.String.Interpolate (i)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import String.ANSI (red, yellow)
import Syntax.Common (Fixity, Var (Var))

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
  = UnusedVar Var

data Error
  = UndefinedVar Var
  | RepeatedVar Var
  | InvalidPrecedence Int Int Var
  | SameInfixPrecedence (Var, Fixity) (Var, Fixity)

hl :: Text -> Text
hl text = [i|'#{text}'|]

instance Show Warning where
  show :: Warning -> String
  show (UnusedVar (Var name _)) = [i|Variable #{hl name} is not used.|]

instance Show Error where
  show :: Error -> String
  show (UndefinedVar (Var name _)) = [i|Variable #{hl name} is not defined.|]
  show (RepeatedVar (Var name _)) = [i|Variable #{hl name} is repeated.|]
  show (InvalidPrecedence lb ub (Var name _)) =
    [i|Precedence of operator #{hl name} must be greater or equal than #{lb} and lesser than #{ub}.|]
  show (SameInfixPrecedence (Var _ _, _) (Var _ _, _)) = errorTODO

warningPrefix :: String
warningPrefix = yellow "Warning: "

wrapWarning :: Warning -> String
wrapWarning wrn = [i|#{warningPrefix}#{wrn}|]

errorPrefix :: String
errorPrefix = red "Error: "

wrapError :: Error -> String
wrapError err = [i|#{errorPrefix}#{err}|]
