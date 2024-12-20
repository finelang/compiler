module Main (main) where

import Control.Monad (forM_, guard)
import Data.Text.IO as TIO (readFile)
import Lexer (lexText)
import Parser (parseTokens)
import System.Environment (getArgs)
import Transform (transformModule, try)

main :: IO ()
main = do
  args <- getArgs
  guard (not $ null args)
  let filePath = head args
  code <- TIO.readFile filePath
  let parsed = parseTokens $ lexText code
  let (result, warnings) = try transformModule parsed
  forM_ warnings print
  case result of
    Left errors -> forM_ errors print
    Right mdule -> print mdule
