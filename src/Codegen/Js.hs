{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Codegen.Js (genCode) where

import Data.String.Interpolate (i)
import Data.Text (Text, intercalate, pack)
import Error (errorTODO)
import Syntax.Common (Bind (..), Var (Var), varName)
import Syntax.Expr (Closure (Closure), Expr (..), Module (Module))

class CodeGens t where
  genCode :: t -> Text

instance CodeGens Expr where
  genCode :: Expr -> Text
  genCode (Int v _) = pack (show v)
  genCode (Float v _) = pack (show v)
  genCode (Id (Var name _)) = name
  genCode (App f args _) =
    let f' = genCode f
        args' = intercalate ", " (map genCode args)
     in [i|#{f'}(#{args'})|]
  genCode (Fun params body _) =
    let params' = intercalate ", " (map varName params)
        body' = genCode body
     in [i|(#{params'}) => #{body'}|]
  genCode (Parens expr) = [i|(#{genCode expr})|]

instance CodeGens (Bind () (Closure any Expr)) where
  genCode :: Bind () (Closure any Expr) -> Text
  genCode (Bind (Var name _) _ (Closure _ expr)) =
    let expr' = genCode expr
     in [i|const #{name} = #{expr'};|]
  genCode _ = errorTODO

instance CodeGens Module where
  genCode :: Module -> Text
  genCode (Module bindings _) = intercalate "\n" (map genCode bindings) <> "\n"