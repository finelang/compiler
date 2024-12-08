{
module Parser (parseTokens) where

import AST (Expr (..))
import Error (HasMetadata (metadata), Metadata (..))
import Lexer (Token (..), TokenType (..))
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  infix   { Token Infix _ _ }
  infixl  { Token Infixl _ _ }
  infixr  { Token Infixr _ _ }
  id      { Token IdTok _ _ }
  int     { Token IntTok _ _ }
  '='     { Token Eq _ _ }
  ':'     { Token Of _ _ }
  '('     { Token Opar _ _ }
  ')'     { Token Cpar _ _ }
  op      { Token Op _ _ }
  ','     { Token Comma _ _ }

%%

Term : Term op Term { binop $1 $2 $3 }
     | id           { Id (tokenLexeme $1) (metadata $1) }
     | int          { Int (tokenLexeme $1) (metadata $1) }

{
binop l op r = App (Id (tokenLexeme op) (metadata op))
                   [l, r]
                   (Metadata (startIndex $ metadata l) (endIndex $ metadata r))

parseError tokens = error . show . head $ tokens
}
