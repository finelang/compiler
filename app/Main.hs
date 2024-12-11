module Main (main) where

import Control.Monad (forM_, guard)
import Data.Text.IO as TIO (readFile)
import Lexer (lexText)
import Parser (parseTokens)
import System.Environment (getArgs)
import Transform (transformParsedExpr)

main :: IO ()
main = do
  args <- getArgs
  guard (not $ null args)
  let filePath = head args
  code <- TIO.readFile filePath
  let pexpr = parseTokens $ lexText code
  case transformParsedExpr pexpr of
    Left errors -> forM_ errors print
    Right expr -> print expr
