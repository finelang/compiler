{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Check (Checkable (..), checkExpr) where

import AST (Binder (binderName), Expr (..), OpChain (..))
import Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)

type Ctx = Set Text

class Checkable t where
  check :: t -> Reader Ctx [Text]

instance Checkable OpChain where
  check :: OpChain -> Reader Ctx [Text]
  check (Operand expr) = check expr
  check (Operation chain op l) =
    concat <$> sequence [check chain, check op, check l]

instance Checkable Expr where
  check :: Expr -> Reader Ctx [Text]
  check (Int _ _) = return []
  check (Float _ _) = return []
  check (Id name _) = do
    names <- ask
    if S.member name names
      then return []
      else return [name <> " is not defined"]
  check (App f args _) =
    concat <$> mapM check (f : args)
  check (Fun params body _) = do
    let params' = S.fromList (map binderName params)
    let paramErrors = ["duplicate params" | length params /= S.size params']
    bodyErrors <- local (S.union params') (check body)
    return (paramErrors ++ bodyErrors)
  check (Parens expr _) = check expr
  check (Chain chain) = check chain

checkExpr :: Expr -> Either [Text] ()
checkExpr expr =
  let errors = runReader (check expr) S.empty
   in if null errors then Right () else Left errors
