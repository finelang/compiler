{
{-# LANGUAGE NoStrictData #-}
module Parser (parseTokens) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (unpack)
import Lexer (Token (..), TokenType (..))
import Syntax.Common
  ( Bind (..),
    Var (Var),
    HasRange (getRange),
    Range (..),
    OpChain' (..),
    fromLRChain,
    Fixity(Fixity),
    Assoc(..)
  )
import Syntax.Parsed (Defn (..), Expr (..), Module (Module))
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  infix   { Token Infix _ _ }
  infixl  { Token Infixl _ _ }
  infixr  { Token Infixr _ _ }
  fn      { Token Fn _ _ }
  let     { Token Let _ _ }
  id      { Token IdTok _ _ }
  int     { Token IntTok _ _ }
  float   { Token FloatTok _ _ }
  '->'    { Token Arrow _ _ }
  '='     { Token Eq _ _ }
  '('     { Token Opar _ _ }
  ')'     { Token Cpar _ _ }
  '{'     { Token Obrace _ _ }
  '}'     { Token Cbrace _ _ }
  op      { Token Op _ _ }
  ';'     { Token Semi _ _ }

%%

Module : Defns    { Module (reverse $1) }

Defns : Defns Defn  { $2 : $1 }
      | {- empty -} { [] }

Defn : let id '=' Expr   { Defn (Bind (mkVar $2) () $4) }
     | Fix Op '=' Expr   { OpDefn (Bind $2 () $4) $1 }

Fix : Assoc int   { Fixity $1 (read $ unpack $ tokenLexeme $2) }

Assoc : infix   { NonAssoc }
      | infixl  { LeftAssoc }
      | infixr  { RightAssoc }

Op : '(' op ')'     { Var (tokenLexeme $2) (getRange ($1, $3)) }

Expr : fn Params '->' Expr    { Fun (reverse $2) $4 (getRange ($1, $4)) }
     | Chain                  { chainToExpr $1 }

Params : Params Param   { $2 : $1 }
       | {- empty -}    { [] }

Param : id  { mkVar $1 }

Chain : App              { Operand' (mkApp $1) }
      | Chain op App     { Operation' $1 (mkVar $2) (mkApp $3) }

App : App Atom    { $2 : $1 }
    | Atom        { [$1] }

Atom : '(' Expr ')'      { Parens $2 }
     | '{' Block '}'     { mkBlock (reverse $2) (getRange ($1, $3)) }
     | id                { Id $ mkVar $1 }
     | int               { Int (read $ unpack $ tokenLexeme $1) (getRange $1) }
     | float             { Float (read $ unpack $ tokenLexeme $1) (getRange $1) }

Block : Block ';' Expr   { $3 : $1 }
      | Block ';'        { $1 }
      | Expr             { [$1] }

{
mkVar tok = Var (tokenLexeme tok) (getRange tok)

mkFix assoc precTok = Fixity assoc (read $ unpack $ tokenLexeme precTok)

mkApp [expr] = expr
mkApp exprs = let last = head exprs
                  (f : args) = reverse exprs
               in App f args (getRange (f, last))

chainToExpr (Operand' expr) = expr
chainToExpr chain = Chain (fromLRChain chain)

mkBlock [e] _ = e
mkBlock (e : es) r = Block (e :| es) r

parseError tokens = error . show . head $ tokens
}
