module Main (main) where

import qualified Lexer
import qualified Data.Text.IO as TextIO

main :: IO ()
main = do
  text <- TextIO.getLine
  print $ Lexer.scanTokens text
