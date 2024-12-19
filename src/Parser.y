{
{-# LANGUAGE NoStrictData #-}
module Parser (parseTokens) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (unpack)
import Lexer (Token (..), TokenType (..))
import Syntax.Common
  ( Binding (Binding),
    Binder (Binder),
    HasRange (getRange),
    Range (..),
    OpChain' (..),
    Operator (..),
    fromLRChain
  )
import Syntax.Parsed (Expr (..), Module (Module))
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
  rec     { Token Rec _ _ }
  id      { Token IdTok _ _ }
  int     { Token IntTok _ _ }
  float   { Token FloatTok _ _ }
  '='     { Token Eq _ _ }
  ':'     { Token Of _ _ }
  '('     { Token Opar _ _ }
  ')'     { Token Cpar _ _ }
  op      { Token Op _ _ }
  ','     { Token Comma _ _ }

%%

Module : Binding Bindings	{ Module ($1 :| reverse $2) }

Bindings : Bindings Binding	{ $2 : $1 }
         | {- empty -}			{ [] }

Binding : let IsRec Binder '=' Expr     { Binding $3 () $5 $2 }

IsRec : rec         { True }
      | {- empty -} { False }

Binder : id         { mkBinder $1 }
       | '(' op ')' { Binder (tokenLexeme $2) (getRange ($1, $3)) }

Expr : fn '(' Params ')' Expr { Fun (reverse $3) $5 (getRange ($1, $5)) }
     | Chain                  { Chain (fromLRChain $1) }

Params : Params ',' Param { $3 : $1 }
       | Param            { [$1] }
       | {- empty -}      { [] }

Param : id  { mkBinder $1 }

Chain : App              { Operand' $1 }
      | Chain op App     { Operation' $1 (mkOp $2) $3 }

App : App '(' Args ')'   { App $1 (reverse $3) (getRange ($1, $4)) }
    | Atom               { $1 }

Args : Args ',' Expr     { $3 : $1 }
     | Expr              { [$1] }
     | {- empty -}       { [] }

Atom : '(' Expr ')' { Parens $2 (getRange ($1, $3)) }
     | id           { mkVar $1 }
     | int          { Int (read $ unpack $ tokenLexeme $1) (getRange $1) }
     | float        { Float (read $ unpack $ tokenLexeme $1) (getRange $1) }

{
mkBinder tok = Binder (tokenLexeme tok) (getRange tok)

mkVar tok = Var (tokenLexeme tok) (getRange tok)

mkOp tok = Operator (tokenLexeme tok) (getRange tok)

parseError tokens = error . show . head $ tokens
}
