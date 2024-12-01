module Main (main) where

import Lexer (scanTokens)
import Parser (parseTokens)
import Data.Text.IO as TextIO (getLine)

main :: IO ()
main = do
  text <- TextIO.getLine
  print $ parseTokens $ scanTokens text
