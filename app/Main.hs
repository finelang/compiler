module Main (main) where

import Control.Monad (forM_)
import qualified Data.Text.IO as TIO (readFile, writeFile)
import Fine.Codegen (runGenCode)
import Fine.Error (wrapError, wrapWarning)
import Fine.Lexer (lexText)
import Fine.Parser (parseTokens)
import Fine.Transform (runTransform)
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
  let (result, warnings) = runTransform parsed
  forM_ warnings (putStrLn . wrapWarning)
  case result of
    Left errors -> forM_ errors (putStrLn . wrapError)
    Right mdule -> print mdule >> TIO.writeFile outFilePath (runGenCode mdule)
