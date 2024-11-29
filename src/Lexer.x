{
module Lexer (Token(..), scanTokens) where
import Data.Text (Text)
import qualified Data.Text as Text
}

%wrapper "posn-strict-text"

tokens :-

  $white+         ;
  [a-z_][a-zA-Z]* { mkt Identifier }
  [0-9]+          { mkt Integer }

{
data TokenClass
  = Identifier
  | Integer
  deriving (Eq, Show)

data Token = Token{
    tokenLexeme :: Text,
    tokenClass :: TokenClass,
    tokenPosn :: AlexPosn
} deriving (Eq, Show)

mkt c p l = Token l c p

scanTokens = alexScanTokens
}
