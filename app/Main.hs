module Main (main) where

import Codegen.Js (runGenCode)
import Control.Monad (forM_)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import Error (wrapError, wrapWarning)
import Lexer (lexText)
import Parser (parseTokens)
import System.Environment (getArgs)
import Transform (transformModule)

getPaths :: IO (String, String)
getPaths = do
  args <- getArgs
  case args of
    (x : y : _) -> return (x, y)
    _ -> error "Not enough arguments."

main :: IO ()
main = do
  (inFilePath, outFilePath) <- getPaths
  code <- TIO.readFile inFilePath
  let parsed = parseTokens $ lexText code
  let (result, warnings) = transformModule parsed
  forM_ warnings (putStrLn . wrapWarning)
  case result of
    Left errors -> forM_ errors (putStrLn . wrapError)
    Right mdule -> print mdule >> TIO.writeFile outFilePath (runGenCode mdule)
