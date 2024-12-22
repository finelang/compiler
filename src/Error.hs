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
  | BindShadowing Var

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
  show (BindShadowing (Var name _)) = [i|The binding for #{hl name} shadows the existing binding.|]

instance Show Error where
  show :: Error -> String
  show (UndefinedVar (Var name _)) = [i|Variable #{hl name} is not defined.|]
  show (RepeatedVar (Var name _)) = [i|Variable #{hl name} is repeated.|]
  show (InvalidPrecedence _ _ _) = errorTODO
  show (SameInfixPrecedence (Var _ _, _) (Var _ _, _)) = errorTODO
