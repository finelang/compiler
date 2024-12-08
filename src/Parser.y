{
module Parser (parseTokens) where

import AST (Expr(..), Metadata(Metadata))
import qualified Data.Text as Text (length)
import Lexer (Token (..), TokenType (..), TokenPosn (..))
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

Term : Term op Term { Bin $1 (tokenLexeme $2) $3 }
     | id           { Id (tokenLexeme $1) (metadata $1) }
     | int          { Int (tokenLexeme $1) (metadata $1) }

{
metadata tok =
  let si = posnIndex . tokenPosn $ tok
      ei = si + (Text.length . tokenLexeme $ tok)
   in Metadata si ei

parseError tokens = error . show . head $ tokens
}
