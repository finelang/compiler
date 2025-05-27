module Main (main) where

import Test.Hspec
import qualified Fine.LexerSpec
import qualified Fine.ParserSpec

main :: IO ()
main = hspec $ do
  describe "Fine Compiler Tests" $ do
    describe "Lexer" Fine.LexerSpec.spec
    describe "Parser" Fine.ParserSpec.spec
