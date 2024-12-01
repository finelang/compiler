module AST (Term (..)) where

import Lexer (Token)

data Term
  = BinOp Term Term Term
  | IdTerm Token
  | IntTerm Token
  deriving (Show)
