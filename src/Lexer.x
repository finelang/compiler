{
module Lexer (Token(..), TokenType(..), scanTokens) where

import Data.Text (Text)
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

data TokenPosn = TokenPosn{
  posnIndex :: Int,
  posnLine :: Int,
  posnColumn :: Int
} deriving (Eq, Show)

data Token = Token{
    tokenLexeme :: Text,
    tokenType :: TokenType,
    tokenPosn :: TokenPosn
} deriving (Eq, Show)

tokenIndex (Token _ _ p) = posnIndex p
tokenLine (Token _ _ p) = posnLine p
tokenColumn (Token _ _ p) = posnColumn p

mkt t (AlexPn i ln cl) l = Token l t (TokenPosn i ln cl)

scanTokens = alexScanTokens
}
