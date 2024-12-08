{
{-# LANGUAGE StrictData #-}

module Lexer (Token (..), TokenType (..), TokenPosn (..), lexText) where

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
  [a-z_][$alpha $digit]*  { mkt IdTok }
  $digit+                 { mkt IntTok }
  "="                     { mkt Eq }
  ":"                     { mkt Of }
  "("                     { mkt Opar } 
  ")"                     { mkt Cpar }
  $sym{1, 3}              { mkt Op }
  ","                     { mkt Comma }

{
data TokenType
  = Infix
  | Infixl
  | Infixr
  | IdTok
  | IntTok
  | Eq
  | Of
  | Opar
  | Cpar
  | Op
  | Comma
  deriving (Show)

data TokenPosn = TokenPosn
  { posnIndex :: Int,
    posnLine :: Int,
    posnColumn :: Int
  }
  deriving (Show)

data Token = Token
  { tokenType :: TokenType,
    tokenLexeme :: Text,
    tokenPosn :: TokenPosn
  }
  deriving (Show)

mkt ttype (AlexPn i line col) lexm = Token ttype lexm (TokenPosn i line col)

lexText = alexScanTokens
}
