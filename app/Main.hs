module Main (main) where

import Control.Monad (forM_)
import Data.Text.IO qualified as TIO (readFile, writeFile)
import Fine.Codegen.Js (runCodegen)
import Fine.Error (wrapError, wrapWarning)
import Fine.Lexer (lexText)
import Fine.Parser (parseTokens)
import Fine.Transform (runTransformer)
import System.Environment (getArgs)

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
  let (result, warnings) = runTransformer parsed
  let warn = forM_ warnings (putStrLn . wrapWarning)
  case result of
    Left errors -> do
      forM_ errors (putStrLn . wrapError)
      warn
    Right mdule -> do
      warn
      print mdule >> TIO.writeFile outFilePath (runCodegen mdule)
