module Main (main) where

import Check (check)
import Control.Monad (guard)
import Data.Text.IO as TIO (readFile)
import Lexer (lexText)
import Parser (parseTokens)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  guard (not $ null args)
  let filePath = head args
  code <- TIO.readFile filePath
  let expr = parseTokens $ lexText code
  case check expr of
    (Left errorMessage) -> print errorMessage
    _ -> print expr
