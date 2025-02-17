{
{-# LANGUAGE NoStrictData #-}
module Fine.Parser (parseTokens) where

import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.List.NonEmpty2 (NonEmpty2 (NonEmpty2))
import qualified Data.Text as T
import Fine.Lexer (Token (..), TokenType (..))
import Fine.Syntax.Common
  ( Id (Id),
    HasRange (getRange),
    Range (..),
    OpChain' (..),
    fromLRChain,
    Fixity(Fixity),
    Assoc(..),
    Ext (Ext),
    Lit (..)
  )
import Fine.Syntax.Concrete (Defn (..), Stmt (..), Expr (..), Module (Module))
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  debug   { Token DebugTok _ _ }
  ext     { Token ExtTok _ _ }
  run     { Token Run _ _ }
  data    { Token Data _ _ }
  else    { Token Else _ _ }
  false   { Token FalseTok _ _ }
  if      { Token If _ _ }
  infix   { Token Infix _ _ }
  infixl  { Token Infixl _ _ }
  infixr  { Token Infixr _ _ }
  fn      { Token Fn _ _ }
  let     { Token LetTok _ _ }
  match   { Token Match _ _ }
  mut     { Token MutTok _ _ }
  then    { Token Then _ _ }
  true    { Token TrueTok _ _ }
  discard { Token DiscardTok _ _ }
  id      { Token IdTok _ _ }
  str     { Token StrTok _ _ }
  nat     { Token Nat _ _ }
  nonnat  { Token NonNat _ _ }
  float   { Token FloatTok _ _ }
  '->'    { Token Arrow _ _ }
  '<-'    { Token RArrow _ _ }
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

Prefix : id { mkIdn $1 }

Infix : op  { mkIdn $1 }

Params_ : Params_ ',' Prefix  { $3 : $1 }
        | Prefix              { [$1] }

Params : Params_  { asNonEmpty (reverse $1) }

FParams : '(' Params ')'  { $2 }
        | '(' ')'         { Id "_" (getRange ($1, $2)) :| [] }

Defns : Defns Defn      { $2 : $1 }
      | Defns DataDefn  { $2 ++ $1 }
      | {- empty -}     { [] }

Defn : let Prefix '=' TopExpr               { Defn $2 $4 }
     | let Prefix FParams '=' TopExpr       { Defn $2 (Fun $3 $5 (getRange ($2, $5))) }
     | let Prefix Infix Prefix '=' TopExpr  { Defn $3 (Fun ($2 :| [$4]) $6 (getRange ($2, $6))) }
     | Fix Infix                            { FixDefn $1 $2 }

DataDefn: data '{' Ctors '}'  { reverse $3 }

Ctors : Ctors Ctor  { $2 : $1 }
      | Ctor        { [$1] }

Ctor : let Prefix                 { CtorDefn $2 [] (getRange $2) }
     | let Prefix '(' Params ')'  { CtorDefn $2 (toList $4) (getRange ($2, $5)) }

Fix : Assoc nat { Fixity $1 (read $ T.unpack $ tokenLexeme $2) }

Assoc : infix   { NonAssoc }
      | infixl  { LeftAssoc }
      | infixr  { RightAssoc }

TopExpr : ext str { ExtExpr $ Ext (transformStr $ tokenLexeme $2) (getRange ($1, $2)) }
        | Expr    { $1 }

Expr : fn FParams Expr              { Fun $2 $3 (getRange ($1, $3)) }
     | if Expr then Expr else Expr  { Cond $2 $4 $6 (getRange ($1, $6)) }
     | Prefix '<-' Expr             { Mut $1 $3 }
     | debug Expr                   { Debug $2 (getRange ($1, $2)) }
     | match Expr '{' Matches '}'   { PatternMatch $2 (asNonEmpty $ reverse $4) (getRange ($1, $5)) }
     | Chain                        { chainToExpr $1 }

Matches : Matches ';' Match { $3 : $1 }
        | Matches ';'       { $1 }
        | Match             { [$1] }

Match : App '->' Expr { ($1, $3) }

Chain : App             { Operand' $1 }
      | Chain Infix App { Operation' $1 $2 $3 }

App : App '(' Args ')'  { App $1 $3 (getRange ($1, $4)) }
    | App '(' ')'       { App $1 (Literal Unit (getRange ($2, $3)) :| []) (getRange ($1, $3)) }
    | App '.' Prefix    { Access $1 $3 }
    | App '.' nat       { Index $1 (read $ T.unpack $ tokenLexeme $3) (getRange ($1, $3)) }
    | Atom              { $1 }

Exprs_ : Exprs_ ',' Expr  { $3 : $1 }
       | Expr             { [$1] }

Args : Exprs_ { asNonEmpty (reverse $1) }

Atom : '(' Args ')'   { mkGroup $2 (getRange ($1, $3)) }
     | '(' ')'        { Literal Unit (getRange ($1, $2)) }
     | '{' Obj '}'    { Record $2 (getRange ($1, $3)) }
     | '{' Block '}'  { mkBlock $2 (getRange ($1, $3)) }
     | Prefix         { Var $1 }
     | '(' op ')'     { Var $ Id (tokenLexeme $2) (getRange ($1, $3)) }
     | Int            { $1 }
     | float          { Literal (Float $ read $ T.unpack $ tokenLexeme $1) (getRange $1) }
     | false          { Literal (Bool False) (getRange $1) }
     | true           { Literal (Bool True) (getRange $1) }
     | str            { mkStr $1 }
     | discard        { Discard (getRange $1) }

Int : nat     { Literal (Int $ read $ T.unpack $ tokenLexeme $1) (getRange $1) }
    | nonnat  { Literal (Int $ read $ T.unpack $ tokenLexeme $1) (getRange $1) }

Block : Stmts ';' Expr  { (reverse $1, $3) }
      | Expr            { ([], $1) }

Stmts : Stmts ';' Stmt  { $3 : $1 }
      | Stmt            { [$1] }

Stmt : Expr                     { Do $1 }
     | let Mut Prefix '=' Expr  { Let $2 $3 () $5 }

Mut : mut         { True }
    | {- empty -} { False }

Obj : Props { asNonEmpty (reverse $1) }

Props : Props ',' Prop  { $3 : $1 }
      | Prop            { [$1] }

Prop : Prefix '=' Expr  { ($1, $3) }
     | '=' Prefix       { ($2, Var $2) }

{
mkIdn tok = Id (tokenLexeme tok) (getRange tok)

transformStr = T.tail . T.init

mkStr tok = Literal (Str $ transformStr $ tokenLexeme tok) (getRange tok)

asNonEmpty (x : xs) = x :| xs

mkGroup (x :| []) _ = x
mkGroup (x :| (y : zs)) r = Tuple (NonEmpty2 x y zs) r 

chainToExpr (Operand' expr) = expr
chainToExpr chain = Chain (fromLRChain chain)

mkBlock ([], expr) _ = expr
mkBlock (stmts, expr) r = Block stmts expr r

parseError tokens = error . show . head $ tokens
}
