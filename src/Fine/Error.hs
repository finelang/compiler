module Fine.Error (
  Warning (..),
  Error (..),
  errorTODO,
  errorUNREACHABLE,
  wrapWarning,
  wrapError,
)
where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Fine.Syntax (Id, Op, Range)
import GHC.Stack (HasCallStack)
import String.ANSI (red, yellow)

errorTODO :: (HasCallStack) => a
errorTODO = error "Not Implemented"

errorUNREACHABLE :: (HasCallStack) => String -> a
errorUNREACHABLE message = error $ "This section of code should be unreachable. " ++ message

hl :: (Show a) => a -> Text
hl x = [i|'#{show x}'|]

data Error
  = UndefinedVar Id
  | UnusedUniVar Id
  | AlreadyDefined Id Id
  | UsageBeforeInit Id
  | MutRecBindNotFun Id
  | SameInfixPrecedence Op Op
  | CannotUnify Range Range

instance Show Error where
  show :: Error -> String
  show (UndefinedVar var) =
    [i|Variable #{hl var} is not defined.|]
  show (UnusedUniVar var) =
    [i|Universally quantified variable #{hl var} is not used.|]
  show (AlreadyDefined _ repeated) =
    [i|Variable #{hl repeated} is already defined.|]
  show (UsageBeforeInit var) =
    [i|Variable #{hl var} cannot be read during its own initialization.|]
  show (MutRecBindNotFun var) =
    [i|The expression bound to #{hl var} must be a function expression.|]
  show (SameInfixPrecedence _ _) = errorTODO
  show (CannotUnify r r') = [i|Cannot unify kinds at #{r} and #{r'}.|]

errorPrefix :: String
errorPrefix = red "Error: "

wrapError :: Error -> String
wrapError err = [i|#{errorPrefix}#{err}|]

data Warning
  = UnusedVar Id
  | DebugKeywordUsage Range

instance Show Warning where
  show :: Warning -> String
  show (UnusedVar var) =
    [i|Variable #{hl var} is not used.|]
  show (DebugKeywordUsage _) =
    [i|Consider removing the debug keyword because it produces an IO action.|]

warningPrefix :: String
warningPrefix = yellow "Warning: "

wrapWarning :: Warning -> String
wrapWarning wrn = [i|#{warningPrefix}#{wrn}|]
