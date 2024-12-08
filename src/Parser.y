{
module Parser (parseTokens) where

import AST (Expr (..))
import Error (HasRange (getRange), Range (..))
import Lexer (Token (..), TokenType (..))
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  infix   { Token Infix _ _ }
  infixl  { Token Infixl _ _ }
  infixr  { Token Infixr _ _ }
  let     { Token Let _ _ }
  fn      { Token Fn _ _ }
  id      { Token IdTok _ _ }
  int     { Token IntTok _ _ }
  '='     { Token Eq _ _ }
  ':'     { Token Of _ _ }
  '('     { Token Opar _ _ }
  ')'     { Token Cpar _ _ }
  op      { Token Op _ _ }
  ','     { Token Comma _ _ }

%%

Expr : fn '(' Params ')' Expr { Fun (reverse $3) $5 (getRange ($1, $5)) }
     | Atom                   { $1 }

Params : Params ',' Param { $3 : $1 }
       | Param            { [$1] }
       | {- empty -}      { [] }

Param : id  { tokenLexeme $1 }

Atom : '(' Expr ')' { Parens $2 (getRange ($1, $3)) }
     | id           { Id (tokenLexeme $1) (getRange $1) }
     | int          { Int (tokenLexeme $1) (getRange $1) }

{
parseError tokens = error . show . head $ tokens
}
