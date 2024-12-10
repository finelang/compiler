{-# LANGUAGE OverloadedStrings #-}

module Check (check) where

import AST (Binder (binderName), Expr (..), OpChain (..))
import Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

type Ctx = Set Text

checkChain :: OpChain -> Reader Ctx [Text]
checkChain (Operand expr) = check' expr
checkChain (Operation chain op l) =
  concat <$> sequence [checkChain chain, check' op, check' l]

check' :: Expr -> Reader Ctx [Text]
check' (Int _ _) = return []
check' (Float _ _) = return []
check' (Id name _) = do
  names <- ask
  if S.member name names
    then return []
    else return [name <> " is not defined"]
check' (App f args _) =
  concat <$> mapM check' (f : args)
check' (Fun params body _) = do
  let params' = S.fromList (map binderName params)
  let paramErrors = ["duplicate params" | length params /= S.size params']
  bodyErrors <- local (S.union params') (check' body)
  return (paramErrors ++ bodyErrors)
check' (Parens expr _) = check' expr
check' (Chain chain) = checkChain chain

check :: Expr -> Either [Text] ()
check expr =
  let errors = runReader (check' expr) S.empty
   in if null errors then Right () else Left errors
