module Control.Monad.ErrorCollector (
  ErrorCollector,
  fail',
  warn,
  runErrorCollector,
) where

import Data.Errors (Errors (Errors), error', warning)
import Data.List.NonEmpty (NonEmpty ((:|)))

data ErrorCollector e w a = ErrorCollector (Errors e w) a

instance Functor (ErrorCollector e w) where
  fmap :: (a -> b) -> ErrorCollector e w a -> ErrorCollector e w b
  fmap f (ErrorCollector errs x) = ErrorCollector errs (f x)

instance Applicative (ErrorCollector e w) where
  pure :: a -> ErrorCollector e w a
  pure = ErrorCollector mempty
  (<*>) :: ErrorCollector e w (a -> b) -> ErrorCollector e w a -> ErrorCollector e w b
  (ErrorCollector errs f) <*> (ErrorCollector errs' x) =
    ErrorCollector (errs <> errs') (f x)

instance Monad (ErrorCollector e w) where
  (>>=) :: ErrorCollector e w a -> (a -> ErrorCollector e w b) -> ErrorCollector e w b
  (ErrorCollector errs x) >>= f =
    let (ErrorCollector errs' y) = f x
     in ErrorCollector (errs <> errs') y

fail' :: e -> ErrorCollector e w ()
fail' err = ErrorCollector (error' err) ()

warn :: w -> ErrorCollector e w ()
warn wrn = ErrorCollector (warning wrn) ()

runErrorCollector :: ErrorCollector e w a -> (Either (NonEmpty e) a, [w])
runErrorCollector (ErrorCollector (Errors [] wrns) x) = (Right x, wrns)
runErrorCollector (ErrorCollector (Errors (err : errs) wrns) _) = (Left (err :| errs), wrns)
