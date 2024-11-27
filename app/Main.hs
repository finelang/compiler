module Main (main) where

import qualified Lexer as L

main :: IO ()
main = getLine >>= print . L.scanTokens
