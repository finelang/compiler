module Main (main) where

import Control.Monad (forM_)
import Data.Set qualified as Set
import Data.Text.IO qualified as TIO (readFile, writeFile)
import Fine.Codegen (runCodegen)
import Fine.Error (wrapError, wrapWarning)
import Fine.Lexer (lexText)
import Fine.Parser (parseTokens)
import Fine.Rename (runRenamer)
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
  forM_ warnings (putStrLn . wrapWarning)
  case result of
    Left errors -> forM_ errors (putStrLn . wrapError)
    Right mdule -> TIO.writeFile outFilePath (runCodegen $ runRenamer Set.empty mdule)
