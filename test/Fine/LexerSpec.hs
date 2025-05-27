module Fine.LexerSpec (spec) where

import Test.Hspec
import Fine.Lexer -- Assuming Token(..), TokenType(..), lexText, TokenPosn(..) are exported
import Data.Text (Text)
import qualified Data.Text as Text
-- Fine.Syntax likely exports Range and Position if needed,
-- but TokenPosn from Fine.Lexer might be more direct for positions.

-- Helper function to simplify token checking, ignoring position.
shouldMatchTokens :: [Token] -> [(TokenType, Text)] -> Expectation
shouldMatchTokens actual expected =
  map (\t -> (tokenType t, tokenLexeme t)) actual `shouldBe` expected

-- Helper function that checks token type, lexeme, line, and column.
shouldMatchTokensWithPos :: [Token] -> [(TokenType, Text, Int, Int)] -> Expectation
shouldMatchTokensWithPos actual expected =
  map (\t -> (tokenType t, tokenLexeme t, posnLine (tokenPosn t), posnColumn (tokenPosn t))) actual `shouldBe` expected

spec :: Spec
spec = describe "Fine.Lexer.lexText" $ do
  -- Test cases for Keywords
  describe "Keywords" $ do
    it "lexes 'let' keyword" $
      lexText "let" `shouldMatchTokens` [(Let, "let")]
    it "lexes 'if' keyword" $
      lexText "if" `shouldMatchTokens` [(If, "if")]
    it "lexes 'else' keyword" $
      lexText "else" `shouldMatchTokens` [(Else, "else")]
    it "lexes 'fn' keyword" $
      lexText "fn" `shouldMatchTokens` [(Fn, "fn")]
    it "lexes 'forall' keyword" $
      lexText "forall" `shouldMatchTokens` [(Forall, "forall")]
    it "lexes 'foreign' keyword" $
      lexText "foreign" `shouldMatchTokens` [(Foreign, "foreign")]
    it "lexes 'match' keyword" $
      lexText "match" `shouldMatchTokens` [(Match, "match")]
    it "lexes 'mut' keyword" $
      lexText "mut" `shouldMatchTokens` [(Mut, "mut")]
    it "lexes 'run' keyword" $
      lexText "run" `shouldMatchTokens` [(Run, "run")]
    it "lexes 'then' keyword" $
      lexText "then" `shouldMatchTokens` [(Then, "then")]
    it "lexes 'type' keyword" $
      lexText "type" `shouldMatchTokens` [(Type, "type")]
    it "lexes 'while' keyword" $
      lexText "while" `shouldMatchTokens` [(While, "while")]

  -- Test cases for Identifiers
  describe "Identifiers" $ do
    it "lexes a lowercase identifier" $
      lexText "x" `shouldMatchTokens` [(Id, "x")]
    it "lexes a capitalized identifier" $
      lexText "MyVar" `shouldMatchTokens` [(CapId, "MyVar")]
    it "lexes an identifier with numbers" $
      lexText "var123" `shouldMatchTokens` [(Id, "var123")]
    it "lexes an identifier with underscore" $
      lexText "_myVar" `shouldMatchTokens` [(Id, "_myVar")]

  -- Test cases for Literals
  describe "Literals" $ do
    it "lexes an integer" $
      lexText "123" `shouldMatchTokens` [(Nat, "123")]
    it "lexes a negative integer" $
      lexText "-456" `shouldMatchTokens` [(NonNat, "-456")]
    it "lexes a float" $
      lexText "123.45" `shouldMatchTokens` [(FloatLit, "123.45")]
    it "lexes a negative float" $
      lexText "-0.5" `shouldMatchTokens` [(FloatLit, "-0.5")]
    it "lexes a simple string literal" $
      lexText "\"hello\"" `shouldMatchTokens` [(StrLit, "\"hello\"")]
    it "lexes a string literal with escapes" $
      lexText "\"hello\\nworld\"" `shouldMatchTokens` [(StrLit, "\"hello\\nworld\"")]
    it "lexes 'true' boolean literal" $
      lexText "true" `shouldMatchTokens` [(TrueTok, "true")]
    it "lexes 'false' boolean literal" $
      lexText "false" `shouldMatchTokens` [(FalseTok, "false")]

  -- Test cases for Operators
  describe "Operators" $ do
    it "lexes '->' arrow operator" $
      lexText "->" `shouldMatchTokens` [(Arrow, "->")]
    it "lexes '+' operator" $
      lexText "+" `shouldMatchTokens` [(Add, "+")]
    it "lexes '=' assign operator" $
      lexText "=" `shouldMatchTokens` [(Assign, "=")]
    it "lexes '.' dot operator" $
      lexText "." `shouldMatchTokens` [(Dot, ".")]
    it "lexes ':' colon operator" $
      lexText ":" `shouldMatchTokens` [(Colon, ":")]
    it "lexes '|>' pipe operator" $
      lexText "|>" `shouldMatchTokens` [(Pipe, "|>")]
    it "lexes '<|' reverse pipe operator" $
      lexText "<|" `shouldMatchTokens` [(RevPipe, "<|")]
    it "lexes '&&' logical and operator" $
      lexText "&&" `shouldMatchTokens` [(And, "&&")]
    it "lexes '||' logical or operator" $
      lexText "||" `shouldMatchTokens` [(Or, "||")]
    it "lexes '<=' less than or equal operator" $
      lexText "<=" `shouldMatchTokens` [(Lte, "<=")]
    it "lexes '>=' greater than or equal operator" $
      lexText ">=" `shouldMatchTokens` [(Gte, ">=")]
    it "lexes '==' equality operator" $
      lexText "==" `shouldMatchTokens` [(Eq, "==")]
    it "lexes '!=' inequality operator" $
      lexText "!=" `shouldMatchTokens` [(Neq, "!=")]
    it "lexes '<' less than operator" $
      lexText "<" `shouldMatchTokens` [(Lt, "<")]
    it "lexes '>' greater than operator" $
      lexText ">" `shouldMatchTokens` [(Gt, ">")]
    it "lexes '-' subtract operator" $
      lexText "-" `shouldMatchTokens` [(Sub, "-")]
    it "lexes '*' multiply operator" $
      lexText "*" `shouldMatchTokens` [(Mul, "*")]
    it "lexes '/' divide operator" $
      lexText "/" `shouldMatchTokens` [(Div, "/")]
    it "lexes '%' modulo operator" $
      lexText "%" `shouldMatchTokens` [(Mod, "%")]

  -- Test cases for Punctuation
  describe "Punctuation" $ do
    it "lexes '(' open parenthesis" $
      lexText "(" `shouldMatchTokens` [(Opar, "(")]
    it "lexes ')' close parenthesis" $
      lexText ")" `shouldMatchTokens` [(Cpar, ")")]
    it "lexes '{' open brace" $
      lexText "{" `shouldMatchTokens` [(OBrace, "{")]
    it "lexes '}' close brace" $
      lexText "}" `shouldMatchTokens` [(CBrace, "}")]
    it "lexes '[' open bracket" $
      lexText "[" `shouldMatchTokens` [(OBrack, "[")]
    it "lexes ']' close bracket" $
      lexText "]" `shouldMatchTokens` [(CBrack, "]")]
    it "lexes ',' comma" $
      lexText "," `shouldMatchTokens` [(Comma, ",")]
    it "lexes ';' semicolon" $
      lexText ";" `shouldMatchTokens` [(Semi, ";")]

  -- Test cases for Whitespace and Comments
  describe "Whitespace and Comments" $ do
    it "ignores leading and trailing whitespace" $
      lexText "  let  " `shouldMatchTokens` [(Let, "let")]
    it "handles multiple tokens with whitespace" $
      lexText "let x = 10" `shouldMatchTokens` [(Let, "let"), (Id, "x"), (Assign, "="), (Nat, "10")]
    it "ignores single-line comments" $
      lexText "// this is a comment\nlet" `shouldMatchTokens` [(Let, "let")]
    it "handles tokens separated by comments" $
      lexText "if //condition\ntrue" `shouldMatchTokens` [(If, "if"), (TrueTok, "true")]

  -- Test cases for Token Positions
  describe "Token Positions" $ do
    it "reports correct position for a single token" $
      lexText "abc" `shouldMatchTokensWithPos` [(Id, "abc", 1, 1)]
    it "reports correct positions for multiple tokens on one line" $
      lexText "a b c" `shouldMatchTokensWithPos` [(Id, "a", 1, 1), (Id, "b", 1, 3), (Id, "c", 1, 5)]
    it "reports correct positions for tokens on multiple lines" $
      lexText "a\nb" `shouldMatchTokensWithPos` [(Id, "a", 1, 1), (Id, "b", 2, 1)]
    it "reports correct positions for tokens with leading whitespace" $
      lexText "  def" `shouldMatchTokensWithPos` [(Id, "def", 1, 3)]
    it "reports correct positions after a comment" $
      lexText "// comment\n  ghi" `shouldMatchTokensWithPos` [(Id, "ghi", 2, 3)]
    it "reports correct positions for string literals with newlines" $
       lexText "\"a\\nb\"" `shouldMatchTokensWithPos` [(StrLit, "\"a\\nb\"", 1, 1)]
    it "reports correct positions for various tokens" $
      lexText "let x = \"hi\"\nif true" `shouldMatchTokensWithPos`
        [ (Let, "let", 1, 1)
        , (Id, "x", 1, 5)
        , (Assign, "=", 1, 7)
        , (StrLit, "\"hi\"", 1, 9)
        , (If, "if", 2, 1)
        , (TrueTok, "true", 2, 4)
        ]
-- Lexical errors are not tested for now as per instructions.
-- describe "Lexical Errors" $ do
--   it "handles unterminated string literal" $
--     lexText "\"unterminated" `shouldThrow` anyException -- Example if errors throw exceptions
--   it "handles invalid character" $
--     lexText "$" `shouldThrow` anyException -- Example if errors throw exceptions
