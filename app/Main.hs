module Main (main) where

import Lexer (scanTokens)
import Data.Text.IO as TextIO (getLine)

main :: IO ()
main = do
  text <- TextIO.getLine
  print $ scanTokens text
