{-# LANGUAGE QuasiQuotes #-}

module Fine.Error
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
import Fine.Syntax.Common (Fixity, Range, Var)
import GHC.Stack (HasCallStack)
import String.ANSI (red, yellow)

errorTODO :: (HasCallStack) => a
errorTODO = error "Not Implemented"

errorUNREACHABLE :: (HasCallStack) => a
errorUNREACHABLE = error "This section of code should be unreachable"

hl :: (Show a) => a -> Text
hl x = [i|'#{show x}'|]

data Error
  = UndefinedVar Var
  | RepeatedVar Var
  | RepeatedParam Var
  | RepeatedVariant Var
  | UndefinedVariant Var
  | RepeatedProp Var
  | RequiredProp Var Var
  | InvalidProp Var Var
  | InvalidPrecedence Int Int Var
  | RepeatedFixity Var
  | SameInfixPrecedence (Var, Fixity) (Var, Fixity)
  | InvalidPattern Range

instance Show Error where
  show :: Error -> String
  show (UndefinedVar var) =
    [i|Variable #{hl var} is not defined.|]
  show (RepeatedVar var) =
    [i|Variable #{hl var} is repeated.|]
  show (RepeatedParam var) =
    [i|Parameter #{hl var} is repeated.|]
  show (RepeatedVariant var) =
    [i|Variant #{hl var} is repeated.|]
  show (UndefinedVariant var) =
    [i|Variant #{hl var} is not defined.|]
  show (RepeatedProp var) =
    [i|Property #{hl var} is repeated.|]
  show (RequiredProp tag prop) =
    [i|Property #{hl prop} of variant #{hl tag} is missing.|]
  show (InvalidProp tag prop) =
    [i|Variant #{hl tag} does not have a #{hl prop} property.|]
  show (InvalidPrecedence lb ub var) =
    [i|Precedence of operator #{hl var} must be greater or equal than #{lb} and lesser than #{ub}.|]
  show (RepeatedFixity var) =
    [i|Fixity definition for #{hl var} is repeated.|]
  show (SameInfixPrecedence _ _) =
    errorTODO
  show (InvalidPattern _) =
    errorTODO

errorPrefix :: String
errorPrefix = red "Error: "

wrapError :: Error -> String
wrapError err = [i|#{errorPrefix}#{err}|]

data Warning
  = UnusedVar Var
  | MissingFixity Var Fixity
  | UnusedFixity Var

instance Show Warning where
  show :: Warning -> String
  show (UnusedVar var) =
    [i|Variable #{hl var} is not used.|]
  show (MissingFixity var fix) =
    [i|Missing fixity definition for #{hl var}. Defaulting to #{hl fix}.|]
  show (UnusedFixity var) =
    [i|Fixity definition for #{hl var} lacks an accompanying binding.|]

warningPrefix :: String
warningPrefix = yellow "Warning: "

wrapWarning :: Warning -> String
wrapWarning wrn = [i|#{warningPrefix}#{wrn}|]

type Errors = ([Error], [Warning])

collectErrors :: [Error] -> Errors
collectErrors errs = (errs, [])

collectWarnings :: [Warning] -> Errors
collectWarnings wrns = ([], wrns)
