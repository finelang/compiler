module Main (main) where

import Check (checkExpr)
import Control.Monad (forM_, guard)
import Data.Either (fromLeft)
import Data.Text.IO as TIO (putStrLn, readFile)
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
  let errors = fromLeft [] (checkExpr expr)
  forM_ errors TIO.putStrLn
  print expr
