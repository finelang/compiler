module Main (main) where

import Control.Monad.Errors (Errors, runErrors)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.IO qualified as TIO (readFile, writeFile)
import Fine.Codegen.Js (runCodegen)
import Fine.Error (Error, Warning, wrapError, wrapWarning)
import Fine.Lexer (lexText)
import Fine.Parser (parseTokens)
import Fine.Syntax (ParsedModule)
import Fine.Transform (runTransformer)
import Fine.Typer (runTyper)
import Fine.Typer.Kinder (runKinder)
import System.Environment (getArgs)

type EW e w a = ExceptT e (Writer w) a

try :: (Monoid w) => (a -> Errors Error b) -> a -> EW (NonEmpty Error) w b
try op x = do
  let result = runErrors $ op x
  case result of
    Left errs -> throwE errs
    Right y -> return y

pipeline :: ParsedModule -> EW (NonEmpty Error) [Warning] Text
pipeline parsed = do
  (transformed, wrns) <- try runTransformer parsed
  lift $ tell wrns
  kinded <- try runKinder transformed
  (typed, wrns') <- try runTyper kinded
  lift $ tell wrns'
  let code = runCodegen typed
  return code

getPaths :: IO (String, String)
getPaths = do
  args <- getArgs
  case args of
    (x : y : _) -> return (x, y)
    _ -> error "Not enough arguments."

warn :: (Foldable t) => t Warning -> IO ()
warn = mapM_ (putStrLn . wrapWarning)

fail' :: (Foldable t) => t Error -> IO ()
fail' = mapM_ (putStrLn . wrapError)

runPipeline :: IO ()
runPipeline = do
  (inFilePath, outFilePath) <- getPaths
  code <- TIO.readFile inFilePath
  let parsedModule = parseTokens (lexText code)
  let (result, wrns) = runWriter (runExceptT (pipeline parsedModule))
  case result of
    Left errs -> fail' errs >> warn wrns
    Right targetCode -> do
      warn wrns
      TIO.writeFile outFilePath targetCode

main :: IO ()
main = runPipeline
