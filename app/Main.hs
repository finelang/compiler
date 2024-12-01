module Main (main) where

import Lexer (lexText)
import Parser (parseTokens)
import Data.Text.IO as TextIO (getLine)

main :: IO ()
main = do
  text <- TextIO.getLine
  print . parseTokens . lexText $ text
