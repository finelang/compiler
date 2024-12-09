{-# LANGUAGE OverloadedStrings #-}

module Check (check) where

import AST (Expr (..), OpChain (..))
import Control.Monad (forM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, local)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

type Ctx = Set Text

checkChain :: OpChain -> ReaderT Ctx (Either Text) ()
checkChain (Operand expr) = check' expr
checkChain (Operation chain op l) = do
  checkChain chain
  check' op
  check' l

check' :: Expr -> ReaderT Ctx (Either Text) ()
check' (Id name _) = do
  names <- ask
  if S.member name names
    then return ()
    else lift $ Left (name <> " is not defined")
check' (App f args _) = do
  check' f
  forM_ args check'
check' (Fun params body _) = do
  let params' = S.fromList params
  if length params == S.size params'
    then return ()
    else lift $ Left "duplicate params"
  local (S.union params') (check' body)
check' (Parens expr _) = check' expr
check' (Chain chain) = checkChain chain
check' _ = return ()

check :: Expr -> Either Text ()
check expr = runReaderT (check' expr) S.empty
