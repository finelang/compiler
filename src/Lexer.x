{
module Lexer (Token(..), scanTokens, posnIndex, posnLine, posnColumn) where
}

%wrapper "posn-strict-text"

tokens :-

  $white+             ;
  [a-z_][a-zA-Z_0-9]* { mkt Identifier }
  [0-9]+              { mkt Integer }

{
data TokenClass
  = Identifier
  | Integer
  deriving (Eq, Show)

data Token = Token{
    tokenLexeme :: Data.Text.Text,
    tokenClass :: TokenClass,
    tokenPosn :: AlexPosn
} deriving (Eq, Show)

mkt c p l = Token l c p

scanTokens = alexScanTokens

posnIndex (AlexPn i _ _) = i
posnLine (AlexPn _ l _) = l
posnColumn (AlexPn _ _ c) = c
}
