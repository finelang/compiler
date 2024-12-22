{
{-# LANGUAGE NoStrictData #-}
module Parser (parseTokens) where

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
  let     { Token Let _ _ }
  fn      { Token Fn _ _ }
  id      { Token IdTok _ _ }
  int     { Token IntTok _ _ }
  float   { Token FloatTok _ _ }
  '='     { Token Eq _ _ }
  '('     { Token Opar _ _ }
  ')'     { Token Cpar _ _ }
  op      { Token Op _ _ }
  ','     { Token Comma _ _ }

%%

Module : Defns    { Module (reverse $1) }

Defns : Defns Defn  { $2 : $1 }
      | {- empty -} { [] }

Defn : let id '=' Expr   { BindDefn (Bind (mkVar $2) () $4) }
     | Fix Op '=' Expr   { BindDefn (OpBind $2 () $4 $1) }

Fix : Assoc int   { Fixity $1 (read $ unpack $ tokenLexeme $2) }

Assoc : infix   { NonAssoc }
      | infixl  { LeftAssoc }
      | infixr  { RightAssoc }

Op : '(' op ')'     { Var (tokenLexeme $2) (getRange ($1, $3)) }

Expr : fn '(' Params ')' Expr { Fun (reverse $3) $5 (getRange ($1, $5)) }
     | Chain                  { chainToExpr $1 }

Params : Params ',' Param { $3 : $1 }
       | Param            { [$1] }
       | {- empty -}      { [] }

Param : id  { mkVar $1 }

Chain : App              { Operand' $1 }
      | Chain op App     { Operation' $1 (mkVar $2) $3 }

App : App '(' Args ')'   { App $1 (reverse $3) (getRange ($1, $4)) }
    | Atom               { $1 }

Args : Args ',' Expr     { $3 : $1 }
     | Expr              { [$1] }
     | {- empty -}       { [] }

Atom : '(' Expr ')' { Parens $2 }
     | id           { Id $ mkVar $1 }
     | int          { Int (read $ unpack $ tokenLexeme $1) (getRange $1) }
     | float        { Float (read $ unpack $ tokenLexeme $1) (getRange $1) }

{
mkVar tok = Var (tokenLexeme tok) (getRange tok)

mkFix assoc precTok = Fixity assoc (read $ unpack $ tokenLexeme precTok)

chainToExpr (Operand' expr) = expr
chainToExpr chain = Chain (fromLRChain chain)

parseError tokens = error . show . head $ tokens
}
