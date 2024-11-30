{
module Lexer (Token(..), scanTokens, posnIndex, posnLine, posnColumn) where
}

%wrapper "posn-strict-text"

$digit  = [0-9]
$alpha  = [a-zA-Z_]
$sym    = [\+ \- \* \/ \% \^ \| \& \< \> \= \: \\ \? \! \$ \@ \~]

tokens :-

  $white+                 ;
  "infix"                 { mkt Infix }
  "infixl"                { mkt Infixl }
  "infixr"                { mkt Infixr }
  [a-z_][$alpha $digit]*  { mkt Identifier }
  $digit+                 { mkt Integer }
  "="                     { mkt Equals }
  ":"                     { mkt Of }
  "("                     { mkt Opar } 
  ")"                     { mkt Cpar }
  $sym{1, 3}              { mkt Operator }
  ","                     { mkt Comma }

{
data TokenType
  = Infix
  | Infixl
  | Infixr
  | Identifier
  | Integer
  | Equals
  | Of
  | Opar
  | Cpar
  | Operator
  | Comma
  deriving (Eq, Show)

data Token = Token{
    tokenLexeme :: Data.Text.Text,
    tokenType :: TokenType,
    tokenPosn :: AlexPosn
} deriving (Eq, Show)

mkt t p l = Token l t p

scanTokens = alexScanTokens

posnIndex (AlexPn i _ _) = i
posnLine (AlexPn _ l _) = l
posnColumn (AlexPn _ _ c) = c
}
