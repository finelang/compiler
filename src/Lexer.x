{
module Lexer (Token(..), scanTokens) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  let                            { \s -> Let }
  in                             { \s -> In }
  $digit+                        { \s -> Int (read s) }
  [\=\+\-\*\/\(\)]               { \s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*  { \s -> Var s }

{
data Token
  = Let
  | In
  | Sym Char
  | Var String
  | Int Int
  deriving (Eq, Show)

scanTokens = alexScanTokens
}
