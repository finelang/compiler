module Fine.Error
  ( Errors,
    Warning (..),
    Error (..),
    collectErrors,
    collectError,
    collectWarnings,
    collectWarning,
    errorTODO,
    errorUNREACHABLE,
    wrapWarning,
    wrapError,
  )
where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Fine.Syntax.Common (Fixity, Id, Range)
import GHC.Stack (HasCallStack)
import String.ANSI (red, yellow)

errorTODO :: (HasCallStack) => a
errorTODO = error "Not Implemented"

errorUNREACHABLE :: (HasCallStack) => a
errorUNREACHABLE = error "This section of code should be unreachable"

hl :: (Show a) => a -> Text
hl x = [i|'#{show x}'|]

data Error
  = UndefinedVar Id
  | RepeatedVar Id
  | RepeatedParam Id
  | AlreadyInScope Id
  | UsageBeforeInit Id
  | InvalidPrecedence Int Int Id
  | RepeatedFixity Id
  | SameInfixPrecedence (Id, Fixity) (Id, Fixity)
  | InvalidPattern Range
  | DiscardUsage Range

instance Show Error where
  show :: Error -> String
  show (UndefinedVar var) =
    [i|Variable #{hl var} is not defined.|]
  show (RepeatedVar var) =
    [i|Variable #{hl var} is repeated.|]
  show (RepeatedParam var) =
    [i|Parameter #{hl var} is repeated.|]
  show (AlreadyInScope var) =
    [i|Variable #{hl var} is already defined in the current scope.|]
  show (UsageBeforeInit var) =
    [i|Variable #{hl var} cannot be read during its own initialization.|]
  show (InvalidPrecedence lb ub var) =
    [i|Precedence of operator #{hl var} must be greater or equal than #{lb} and lesser than #{ub}.|]
  show (RepeatedFixity var) =
    [i|Fixity definition for #{hl var} is repeated.|]
  show (SameInfixPrecedence _ _) =
    errorTODO
  show (InvalidPattern _) =
    [i|This expression is not a valid pattern.|]
  show (DiscardUsage _) =
    [i|The 'discard' identifier can only be used inside patterns.|]

errorPrefix :: String
errorPrefix = red "Error: "

wrapError :: Error -> String
wrapError err = [i|#{errorPrefix}#{err}|]

data Warning
  = UnusedVar Id
  | MissingFixity Id Fixity
  | DebugKeywordUsage Range

instance Show Warning where
  show :: Warning -> String
  show (UnusedVar var) =
    [i|Variable #{hl var} is not used.|]
  show (MissingFixity var fix) =
    [i|Missing fixity definition for #{hl var}. Defaulting to #{hl fix}.|]
  show (DebugKeywordUsage _) =
    [i|Consider removing the debug keyword because it produces an IO action.|]

warningPrefix :: String
warningPrefix = yellow "Warning: "

wrapWarning :: Warning -> String
wrapWarning wrn = [i|#{warningPrefix}#{wrn}|]

type Errors = ([Error], [Warning])

collectError :: Error -> Errors
collectError err = ([err], [])

collectErrors :: [Error] -> Errors
collectErrors errs = (errs, [])

collectWarning :: Warning -> Errors
collectWarning wrn = ([], [wrn])

collectWarnings :: [Warning] -> Errors
collectWarnings wrns = ([], wrns)
