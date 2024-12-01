{
module Lexer (Token(..), lexText) where

import Data.Text (Text)
}

%wrapper "posn-strict-text"

$digit  = [0-9]
$alpha  = [a-zA-Z_]
$sym    = [\+ \- \* \/ \% \^ \| \& \< \> \= \: \\ \? \! \$ \@ \~]

tokens :-

  $white+                 ;
  "infix"                 { \_ _ -> InfixTok }
  "infixl"                { \_ _ -> InfixlTok }
  "infixr"                { \_ _ -> InfixrTok }
  [a-z_][$alpha $digit]*  { \p t -> IdTok t (mkp p) }
  $digit+                 { \p t -> IntTok t (mkp p) }
  "="                     { \_ _ -> EqTok }
  ":"                     { \_ _ -> OfTok }
  "("                     { \p _ -> OparTok (mkp p) } 
  ")"                     { \p _ -> CparTok (mkp p) }
  $sym{1, 3}              { \p t -> OpTok t (mkp p) }
  ","                     { \_ _ -> CommaTok }

{
data Token
  = InfixTok
  | InfixlTok
  | InfixrTok
  | IdTok Text TokenPosn
  | IntTok Text TokenPosn
  | EqTok
  | OfTok
  | OparTok TokenPosn
  | CparTok TokenPosn
  | OpTok Text TokenPosn
  | CommaTok
  deriving Show

data TokenPosn = TokenPosn{
  posnIndex :: Int,
  posnLine :: Int,
  posnColumn :: Int
} deriving Show

mkp (AlexPn i ln cl) = TokenPosn i ln cl

lexText = alexScanTokens
}
