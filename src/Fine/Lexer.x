{
module Fine.Lexer (Token (..), TokenType (..), lexText) where

import Data.Text (Text)
import qualified Data.Text as Text
import Fine.Syntax (HasRange (range), Range (Range))
}

%wrapper "posn-strict-text"

$whitechar    = [ \t\n\r\f\v]
$special      = [\(\)\,\;\[\]\`\{\}]
$digit        = 0-9
$ascsymbol    = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$opsymbol     = [$ascsymbol \:] # [\# \.]
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
  "#external"                 { mkt ExtTok }
  "#debug"                    { mkt DebugTok }
  "#run"                      { mkt Run }
  "bool"                      { mkt BoolTTok }
  "do"                        { mkt DoTok }
  "else"                      { mkt Else }
  "false"                     { mkt FalseTok }
  "forall"                    { mkt ForallTok }
  "if"                        { mkt If }
  "infix"                     { mkt Infix }
  "infixl"                    { mkt Infixl }
  "infixr"                    { mkt Infixr }
  "int"                       { mkt IntTTok }
  "fn"                        { mkt Fn }
  "float"                     { mkt FloatTTok }
  "match"                     { mkt Match }
  "mut"                       { mkt MutTok }
  "string"                    { mkt StrTTok }
  "then"                      { mkt Then }
  "true"                      { mkt TrueTok }
  "type"                      { mkt TypeTok }
  "while"                     { mkt While }
  "with"                      { mkt With }
  "_"+                        { mkt DiscardTok }
  [$alpha][$alpha $digit]*    { mkt IdTok }
  \" @string* \"              { mkt StrTok }
  @decimal                    { mkt Nat }
  "-" @decimal                { mkt NonNat }
  "-"? @decimal "." @decimal  { mkt FloatTok }
  "->"                        { mkt Arrow }
  "<-"                        { mkt RArrow }
  "="                         { mkt Eq }
  "."                         { mkt Dot }
  "|"                         { mkt Bar }
  "("                         { mkt Opar } 
  ")"                         { mkt Cpar }
  "{"                         { mkt Obrace }
  "}"                         { mkt Cbrace }
  ";"                         { mkt Semi }
  ":"                         { mkt Colon }
  $opsymbol{1, 3}             { mkt Op }
  ","                         { mkt Comma }

{
data TokenType
  = ExtTok
  | DebugTok
  | Run
  | BoolTTok
  | DoTok
  | Else
  | FalseTok
  | ForallTok
  | If
  | Infix
  | Infixl
  | Infixr
  | IntTTok
  | Fn
  | FloatTTok
  | Match
  | MutTok
  | StrTTok
  | Then
  | TrueTok
  | TypeTok
  | While
  | With
  | DiscardTok
  | IdTok
  | StrTok
  | Nat
  | NonNat
  | FloatTok
  | Arrow
  | RArrow
  | Eq
  | Dot
  | Bar
  | Opar
  | Cpar
  | Obrace
  | Cbrace
  | Semi
  | Colon
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

instance HasRange Token where
  range (Token _ lexeme (TokenPosn i line col)) =
    let len = Text.length lexeme
        ei = i + len
        ec = col + len
     in Range i col line ei ec line

mkt ttype (AlexPn i line col) lexm = Token ttype lexm (TokenPosn i line col)

lexText = alexScanTokens
}
