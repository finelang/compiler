{
module Lexer (Token (..), TokenType (..), lexText) where

import Data.Text (Text)
import qualified Data.Text as Text (length)
import Syntax.Common (HasRange (getRange), Range (Range))
}

%wrapper "posn-strict-text"

$whitechar    = [ \t\n\r\f\v]
$special      = [\(\)\,\;\[\]\`\{\}]
$digit        = 0-9
$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$opsymbol     = $ascsymbol # [\.\#]
$symbol       = $ascsymbol # [$special \_\:\"\']
$large        = [A-Z \xc0-\xd6 \xd8-\xde]
$small        = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha        = [$small $large]
$graphic      = [$small $large $symbol $digit $special \:\"\']
$cntrl        = [$large \@\[\\\]\^\_]
$charesc      = [abfnrtv\\\"\'\&]

@decimal      = $digit+
@ascii        = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	            | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	            | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	            | SUB | ESC | FS | GS | RS | US | SP | DEL
@escape       = \\ ($charesc | @ascii)
@gap          = \\ $whitechar+ \\
@string       = $graphic # [\"\\] | " " | @escape | @gap

tokens :-

  $white+                     ;
  "--"\-*[^$symbol].*         ;
  "else"                      { mkt Else }
  "data"                      { mkt DataTok }
  "if"                        { mkt If }
  "infix"                     { mkt Infix }
  "infixl"                    { mkt Infixl }
  "infixr"                    { mkt Infixr }
  "fn"                        { mkt Fn }
  "let"                       { mkt Let }
  "then"                      { mkt Then }
  [$alpha][$alpha $digit]*    { mkt IdTok }
  \" @string* \"              { mkt StrTok }
  "-"? @decimal               { mkt IntTok }
  "-"? @decimal "." @decimal  { mkt FloatTok }
  "->"                        { mkt Arrow }
  "="                         { mkt Eq }
  "("                         { mkt Opar } 
  ")"                         { mkt Cpar }
  "{"                         { mkt Obrace }
  "}"                         { mkt Cbrace }
  $opsymbol{1, 3}             { mkt Op }
  ";"                         { mkt Semi }
  ","                         { mkt Comma }
  "`"                         { mkt Btick }

{
data TokenType
  = Else
  | DataTok
  | If
  | Infix
  | Infixl
  | Infixr
  | Fn
  | Let
  | Then
  | IdTok
  | StrTok
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
  | Comma
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
