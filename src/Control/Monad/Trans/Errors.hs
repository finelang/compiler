module Control.Monad.Trans.Errors (
  ErrorsT (runErrorsT),
  failure,
  fromEither,
  Errors,
  runErrors,
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.Functor.Identity (Identity (runIdentity))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty

newtype ErrorsT e m a = ErrorsT {runErrorsT :: m (Either (NonEmpty e) a)}

failure :: (Applicative m) => e -> ErrorsT e m a
failure = ErrorsT . pure . Left . NonEmpty.singleton

fromEither :: (Applicative m) => Either (NonEmpty e) a -> ErrorsT e m a
fromEither = ErrorsT . pure

instance (Functor m) => Functor (ErrorsT e m) where
  fmap :: (Functor m) => (a -> b) -> ErrorsT e m a -> ErrorsT e m b
  fmap f = ErrorsT . fmap (fmap f) . runErrorsT

instance (Applicative m) => Applicative (ErrorsT e m) where
  pure :: (Applicative m) => a -> ErrorsT e m a
  pure = ErrorsT . pure . Right
  (<*>) :: (Applicative m) => ErrorsT e m (a -> b) -> ErrorsT e m a -> ErrorsT e m b
  ErrorsT mef <*> ErrorsT mex = ErrorsT $ go <$> mef <*> mex
   where
    go (Right f) (Right x) = Right (f x)
    go (Left es) (Left es') = Left (NonEmpty.append es es')
    go (Left es) _ = Left es
    go _ (Left es') = Left es'

-- UNLAWFUL MONAD!
instance (Monad m) => Monad (ErrorsT e m) where
  (>>) :: (Monad m) => ErrorsT e m a -> ErrorsT e m b -> ErrorsT e m b
  (>>) = (*>)

  (>>=) :: (Monad m) => ErrorsT e m a -> (a -> ErrorsT e m b) -> ErrorsT e m b
  ErrorsT mex >>= f = ErrorsT $ do
    ex <- mex
    case ex of
      Left es -> pure (Left es)
      Right x -> runErrorsT (f x)

instance MonadTrans (ErrorsT e) where
  lift :: (Monad m) => m a -> ErrorsT e m a
  lift = ErrorsT . fmap Right

type Errors e = ErrorsT e Identity

runErrors :: Errors e a -> Either (NonEmpty e) a
runErrors = runIdentity . runErrorsT
