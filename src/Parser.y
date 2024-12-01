{
module Parser (parseTokens) where

import Lexer (Token(Token), TokenType(..))
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  infix   { Token _ Infix _ }
  infixl  { Token _ Infixl _ }
  infixr  { Token _ Infixr _ }
  id      { Token _ Identifier _ }
  int     { Token _ Integer _ }
  '='     { Token _ Equals _ }
  ':'     { Token _ Of _ }
  '('     { Token _ Opar _ }
  ')'     { Token _ Cpar _ }
  op      { Token _ Operator _ }
  ','     { Token _ Comma _ }

%%

Term : Term op Term { BinOp $1 (IdTerm $2) $3 }
     | id           { IdTerm $1 }
     | int          { IntTerm $1 }

{
parseError tokens = error . show . head $ tokens

data Term
  = BinOp Term Term Term
  | IdTerm Token
  | IntTerm Token
  deriving Show
}
