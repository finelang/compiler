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
  "bool"                      { mkt Bool }
  "debug"                     { mkt Debug }
  "else"                      { mkt Else }
  "false"                     { mkt FalseTok }
  "float"                     { mkt Float }
  "fn"                        { mkt Fn }
  "foreign"                   { mkt Foreign }
  "if"                        { mkt If }
  "infix"                     { mkt Infix }
  "infixl"                    { mkt Infixl }
  "infixr"                    { mkt Infixr }
  "int"                       { mkt Int }
  "let"                       { mkt Let }
  "match"                     { mkt Match }
  "mut"                       { mkt Mut }
  "run"                       { mkt Run }
  "string"                    { mkt Str }
  "then"                      { mkt Then }
  "true"                      { mkt TrueTok }
  "type"                      { mkt Type }
  "void"                      { mkt Void }
  "while"                     { mkt While }
  "_"+                        { mkt Discard }
  [$small][$alpha $digit]*    { mkt Id }
  [$large][$alpha $digit]*    { mkt CapId }
  \" @string* \"              { mkt StrLit }
  @decimal                    { mkt Nat }
  "-" @decimal                { mkt NonNat }
  "-"? @decimal "." @decimal  { mkt FloatLit }
  "->"                        { mkt Arrow }
  "="                         { mkt Eq }
  "."                         { mkt Dot }
  ":"                         { mkt Colon }
  $opsymbol{1, 3}             { mkt Op }
  "("                         { mkt Opar } 
  ")"                         { mkt Cpar }
  "{"                         { mkt Obrace }
  "}"                         { mkt Cbrace }
  "["                         { mkt Osquare }
  "]"                         { mkt Csquare }
  ","                         { mkt Comma }
  ";"                         { mkt Semi }

{
  data TokenType
  -- keywords
  = Debug
  | Else
  | Fn
  | Foreign
  | If
  | Infix
  | Infixl
  | Infixr
  | Let
  | Match
  | Mut
  | Run
  | Then
  | Type
  | While
  -- identifiers
  | FalseTok
  | Float
  | Int
  | Str
  | TrueTok
  | Bool
  | Void
  | Discard
  | Id
  | CapId
  -- literals
  | StrLit
  | Nat
  | NonNat
  | FloatLit
  -- symbols
  | Arrow
  | Eq
  | Dot
  | Colon
  | Op
  | Opar
  | Cpar
  | Obrace
  | Cbrace
  | Osquare
  | Csquare
  | Comma
  | Semi
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
