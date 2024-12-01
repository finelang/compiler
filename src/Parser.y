{
module Parser (parseTokens) where

import Lexer (Token(..))
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  infix   { InfixTok }
  infixl  { InfixlTok }
  infixr  { InfixrTok }
  id      { IdTok _ _ }
  int     { IntTok _ _ }
  '='     { EqTok }
  ':'     { OfTok }
  '('     { OparTok _ }
  ')'     { CparTok _ }
  op      { OpTok _ _ }
  ','     { CommaTok }

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
