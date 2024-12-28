{
module Lexer (Token (..), TokenType (..), lexText) where

import Data.Text (Text)
import qualified Data.Text as Text (length)
import Syntax.Common (HasRange (getRange), Range (Range))
}

%wrapper "posn-strict-text"

$digit  = [0-9]
$alpha  = [a-zA-Z_]
$sym    = [\+ \- \* \/ \% \^ \| \& \< \> \= \: \\ \? \! \$ \@ \~]

tokens :-

  $white+                   ;
  "infix"                   { mkt Infix }
  "infixl"                  { mkt Infixl }
  "infixr"                  { mkt Infixr }
  "fn"                      { mkt Fn }
  "let"                     { mkt Let }
  [a-z_][$alpha $digit]*    { mkt IdTok }
  "-"? $digit+              { mkt IntTok }
  "-"? $digit+ "." $digit+  { mkt FloatTok }
  "->"                      { mkt Arrow }
  "="                       { mkt Eq }
  "("                       { mkt Opar } 
  ")"                       { mkt Cpar }
  "{"                       { mkt Obrace }
  "}"                       { mkt Cbrace }
  $sym{1, 3}                { mkt Op }
  ";"                       { mkt Semi }
  "`"                       { mkt Btick }

{
data TokenType
  = Infix
  | Infixl
  | Infixr
  | Fn
  | Let
  | IdTok
  | IntTok
  | FloatTok
  | Arrow
  | Eq
  | Of
  | Opar
  | Cpar
  | Obrace
  | Cbrace
  | Op
  | Semi
  | Btick
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

instance HasRange Token where
  getRange (Token _ lexeme (TokenPosn i line col)) =
    let len = Text.length lexeme
        ei = i + len
        ec = col + len
     in Range i col line ei ec line

mkt ttype (AlexPn i line col) lexm = Token ttype lexm (TokenPosn i line col)

lexText = alexScanTokens
}
