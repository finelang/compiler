{
{-# LANGUAGE NoStrictData #-}
module Parser (parseTokens) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Lexer (Token (..), TokenType (..))
import Syntax.Common
  ( Bind (..),
    Var (Var),
    HasRange (getRange),
    Range (..),
    OpChain' (..),
    fromLRChain,
    Fixity(Fixity),
    Assoc(..),
    Data (Data)
  )
import Syntax.Parsed (Defn (..), Expr (..), Module (Module))
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  ext     { Token ExtTok _ _ }
  else    { Token Else _ _ }
  data    { Token DataTok _ _ }
  if      { Token If _ _ }
  infix   { Token Infix _ _ }
  infixl  { Token Infixl _ _ }
  infixr  { Token Infixr _ _ }
  fn      { Token Fn _ _ }
  let     { Token Let _ _ }
  then    { Token Then _ _ }
  id      { Token IdTok _ _ }
  str     { Token StrTok _ _ }
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
  ','     { Token Comma _ _ }
  '`'     { Token Btick _ _ }

%%

Module : Defns  { Module (reverse $1) }

Defns : Defns Defn  { $2 : $1 }
      | {- empty -} { [] }

Defn : let Prefix '=' Expr  { Defn (Bind $2 () $4) }
     | Ext let Prefix       { Defn (Bind $3 () $1) }
     | Fix Infix            { FixDefn $1 $2 }
     | data '{' Ctors '}'   { DtypeDefn (reverse $3) }

Ctors : Ctors Ctor  { $2 : $1 }
      | Ctor        { [$1] }

Ctor : let Prefix Params  { Bind $2 () (Ctor $2 (reverse $3)) }
     | Ext let Prefix     { Bind $3 () $1 }

Fix : Assoc int { Fixity $1 (read $ T.unpack $ tokenLexeme $2) }

Assoc : infix   { NonAssoc }
      | infixl  { LeftAssoc }
      | infixr  { RightAssoc }

Prefix : id         { mkVar $1 }
       | '(' op ')' { Var (tokenLexeme $2) (getRange ($1, $3)) }

Infix : op          { mkVar $1 }
      | '`' id '`'  { Var (tokenLexeme $2) (getRange ($1, $3)) }

Expr : fn Params '->' Expr          { Fun (reverse $2) $4 (getRange ($1, $4)) }
     | if Expr then Expr else Expr  { Cond $2 $4 $6 (getRange ($1, $6)) }
     | Chain                        { chainToExpr $1 }

Params : Params Param { $2 : $1 }
       | {- empty -}  { [] }

Param : id  { mkVar $1 }

Chain : App             { Operand' (mkApp $1) }
      | Chain Infix App { Operation' $1 $2 (mkApp $3) }

App : App Atom  { $2 : $1 }
    | Atom      { [$1] }

Atom : '(' Expr ')'   { Parens $2 }
     | '{' Obj '}'    { Obj (Data $ reverse $2) (getRange ($1, $3)) }
     | '{' Block '}'  { mkBlock (reverse $2) (getRange ($1, $3)) }
     | Prefix         { Id $1 }
     | int            { Int (read $ T.unpack $ tokenLexeme $1) (getRange $1) }
     | float          { Float (read $ T.unpack $ tokenLexeme $1) (getRange $1) }
     | str            { mkStr $1 }

Block : Block ';' Expr  { $3 : $1 }
      | Block ';'       { $1 }
      | Expr            { [$1] }

Obj : Obj ',' ObjMember { $3 : $1 }
    | Obj ','           { $1 }
    | ObjMember         { [$1] }

ObjMember : Param '=' Expr  { ($1, $3) }

Ext : ext str { Ext (transformStr $ tokenLexeme $2) (getRange ($1, $2)) }

{
mkVar tok = Var (tokenLexeme tok) (getRange tok)

transformStr = T.tail . T.init

mkStr tok = Str (transformStr $ tokenLexeme tok) (getRange tok)

mkFix assoc precTok = Fixity assoc (read $ T.unpack $ tokenLexeme precTok)

mkApp [expr] = expr
mkApp exprs =
  let last = head exprs
      (f : args) = reverse exprs
   in App f args (getRange (f, last))

chainToExpr (Operand' expr) = expr
chainToExpr chain = Chain (fromLRChain chain)

mkBlock [e] _ = e
mkBlock (e : es) r = Block (e :| es) r

parseError tokens = error . show . head $ tokens
}
