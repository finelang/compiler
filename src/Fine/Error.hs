module Fine.Error (
  Warning (..),
  Error (..),
  wrapWarning,
  wrapError,
)
where

import Data.String.Interpolate (i)
import Data.Text (Text)
import Fine.Syntax (Id, Kind, Op, Phase (PartiallyKinded), Range)
import GHC.Err.Extra (errorTODO)
import String.ANSI (red, yellow)

hl :: (Show a) => a -> Text
hl x = [i|'#{show x}'|]

data Error
  = UndefinedVar Id
  | UnusedUniVar Id
  | AlreadyDefined Id Id
  | UsageBeforeInit Id
  | MutRecBindNotFun Id
  | SameInfixPrecedence Op Op
  | CannotUnifyKinds (Kind PartiallyKinded) (Kind PartiallyKinded)
  | BadKindSub Id (Kind PartiallyKinded)

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
  show (CannotUnifyKinds kind kind') =
    [i|Cannot unify kinds #{hl kind} and #{hl kind'}.|]
  show (BadKindSub _ _) = errorTODO

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
