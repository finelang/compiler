{
{-# LANGUAGE NoStrictData #-}
module Fine.Parser (parseTokens) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Fine.Lexer (Token (..), TokenType (..))
import Fine.Syntax.Common
  ( Bind (..),
    Var (Var),
    HasRange (getRange),
    Range (..),
    OpChain' (..),
    fromLRChain,
    Fixity(Fixity),
    Assoc(..),
    Ext (Ext),
    Prop (..),
    varName,
    Lit (..)
  )
import Fine.Syntax.Concrete (Defn (..), Expr (..), Module (Module))
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  ext     { Token ExtTok _ _ }
  run     { Token Run _ _ }
  data    { Token Data _ _ }
  debug   { Token DebugTok _ _ }
  else    { Token Else _ _ }
  false   { Token FalseTok _ _ }
  if      { Token If _ _ }
  infix   { Token Infix _ _ }
  infixl  { Token Infixl _ _ }
  infixr  { Token Infixr _ _ }
  fn      { Token Fn _ _ }
  let     { Token Let _ _ }
  match   { Token Match _ _ }
  then    { Token Then _ _ }
  true    { Token TrueTok _ _ }
  id      { Token IdTok _ _ }
  str     { Token StrTok _ _ }
  int     { Token IntTok _ _ }
  float   { Token FloatTok _ _ }
  '->'    { Token Arrow _ _ }
  '='     { Token Eq _ _ }
  '.'     { Token Dot _ _ }
  '('     { Token Opar _ _ }
  ')'     { Token Cpar _ _ }
  '{'     { Token Obrace _ _ }
  '}'     { Token Cbrace _ _ }
  op      { Token Op _ _ }
  ';'     { Token Semi _ _ }
  ','     { Token Comma _ _ }

%%

Module : Defns Entry  { Module (reverse $1) $2 }

Entry : run Expr    { Just $2 }
      | {- empty -} { Nothing }

Prefix : id { mkVar $1 }

Infix : op  { mkVar $1 }

Defns : Defns Defn      { $2 : $1 }
      | Defns DataDefn  { $2 ++ $1 }
      | {- empty -} { [] }

Defn : let Prefix '=' Expr              { Defn (Bind $2 () $4) }
     | ExtExpr let Prefix               { Defn (Bind $3 () $1) }
     | let Prefix Infix Prefix '=' Expr { Defn (Bind $3 () (Fun [$2, $4] $6 (getRange ($2, $6)))) }
     | ExtExpr let Prefix Infix Prefix  { Defn (Bind $4 () (Fun [$3, $5] $1 (getRange ($3, $5)))) }
     | Fix Infix                        { FixDefn $1 $2 }

DataDefn: data '{' Ctors '}'  { reverse $3 }

Ctors : Ctors Ctor  { $2 : $1 }
      | Ctor        { [$1] }

Ctor : let Prefix '{' Params '}'  { CtorDefn $2 (reverse $4) (getRange ($2, $5)) }

Fix : Assoc int { Fixity $1 (read $ T.unpack $ tokenLexeme $2) }

Assoc : infix   { NonAssoc }
      | infixl  { LeftAssoc }
      | infixr  { RightAssoc }

Expr : fn '(' Params ')' '->' Expr      { Fun (reverse $3) $6 (getRange ($1, $6)) }
     | fn '(' Params ')' '{' Block '}'  { Fun (reverse $3) (mkBlock (reverse $6) (getRange ($5, $7))) (getRange ($1, $7)) }
     | if Expr then Expr else Expr      { Cond $2 $4 $6 (getRange ($1, $6)) }
     | match Group '{' Matches '}'      { PatternMatch $2 (asNonEmpty $ reverse $4) (getRange ($1, $5)) }
     | debug Expr                       { Debug $2 (getRange ($1, $2)) }
     | Chain                            { chainToExpr $1 }

Matches : Matches ';' Match { $3 : $1 }
        | Matches ';'       { $1 }
        | Match             { [$1] }

Match : Atom '->' Expr  { ($1, $3) }

Params : Params ',' Prefix  { $3 : $1 }
       | Prefix             { [$1] }
       | {- empty -}        { [] }

Chain : App             { Operand' $1 }
      | Chain Infix App { Operation' $1 $2 $3 }

App : App '(' Args ')'  { App $1 (reverse $3) (getRange ($1, $4)) }
    | App '.' Prefix    { Access $1 $3 }
    | Atom              { $1 }

Args : Args ',' Expr  { $3 : $1 }
     | Expr           { [$1] }
     | {- empty -}    { [] }

Atom : Group              { $1 }
     | '{' Obj '}'        { Obj (reverse $2) (getRange ($1, $3)) }
     | Prefix '{' Obj '}' { Variant $1 (reverse $3) (getRange ($1, $4)) }
     | '{' Block '}'      { mkBlock (reverse $2) (getRange ($1, $3)) }
     | Prefix             { Id $1 }
     | '(' op ')'         { Id $ Var (tokenLexeme $2) (getRange ($1, $3)) }
     | int                { Literal (Int $ read $ T.unpack $ tokenLexeme $1) (getRange $1) }
     | float              { Literal (Float $ read $ T.unpack $ tokenLexeme $1) (getRange $1) }
     | false              { Literal (Bool False) (getRange $1) }
     | true               { Literal (Bool True) (getRange $1) }
     | str                { mkStr $1 }

Group: '(' Args ')' { mkGroupExpr (reverse $2) (getRange ($1, $3)) }

Block : Block ';' Expr  { $3 : $1 }
      | Block ';'       { $1 }
      | Expr            { [$1] }

Obj : Obj ',' Prop  { $3 : $1 }
    | Prop          { [$1] }
    | {- empty -}   { [] }

Prop : Prefix '=' Expr  { NamedProp $1 $3 }
     | '.' '.' '.' Expr { SpreadProp $4 }
     | '=' Prefix       { NamedProp $2 (Id $2) }

Ext : ext str { Ext (transformStr $ tokenLexeme $2) (getRange ($1, $2)) }

ExtExpr : Ext { ExtExpr $1 }

{
mkVar tok = Var (tokenLexeme tok) (getRange tok)

transformStr = T.tail . T.init

mkStr tok = Literal (Str $ transformStr $ tokenLexeme tok) (getRange tok)

asNonEmpty (x : xs) = x :| xs

chainToExpr (Operand' expr) = expr
chainToExpr chain = Chain (fromLRChain chain)

mkBlock [e] _ = e
mkBlock (e : es) r = Block (e :| es) r

mkGroupExpr [] r = Literal Unit r
mkGroupExpr [expr] _ = Parens expr
mkGroupExpr (fst : snd : rest) r = Tuple fst snd rest r

parseError tokens = error . show . head $ tokens
}
