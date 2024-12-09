module Main (main) where

import Check (check)
import Control.Monad (guard, forM_, unless)
import Data.Text.IO as TIO (readFile, putStrLn)
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
  let errors = check expr
  unless (null errors) (forM_ errors TIO.putStrLn)
  print expr
