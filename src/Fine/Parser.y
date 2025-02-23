{
{-# LANGUAGE NoStrictData #-}
module Fine.Parser (parseTokens) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty2 (NonEmpty2 (NonEmpty2))
import qualified Data.Text as T
import Fine.Lexer (Token (..), TokenType (..))
import Fine.Syntax.Common
  ( Id (Id),
    HasRange (range),
    Range (..),
    OpChain' (..),
    fromLRChain,
    Fixity(Fixity),
    Assoc(..),
    Ext (Ext),
    Lit (..),
    Bind (Bind)
  )
import Fine.Syntax.Concrete (CtorDefn (..), Defn (..), Stmt (..), Expr (..), Module (Module))
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  ext     { Token ExtTok _ _ }
  debug   { Token DebugTok _ _ }
  and     { Token And _ _ }
  do      { Token DoTok _ _ }
  else    { Token Else _ _ }
  false   { Token FalseTok _ _ }
  if      { Token If _ _ }
  infix   { Token Infix _ _ }
  infixl  { Token Infixl _ _ }
  infixr  { Token Infixr _ _ }
  fn      { Token Fn _ _ }
  match   { Token Match _ _ }
  mut     { Token MutTok _ _ }
  then    { Token Then _ _ }
  true    { Token TrueTok _ _ }
  type    { Token TypeTok _ _ }
  with    { Token With _ _ }
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
  '|'     { Token Bar _ _ }
  '('     { Token Opar _ _ }
  ')'     { Token Cpar _ _ }
  '{'     { Token Obrace _ _ }
  '}'     { Token Cbrace _ _ }
  ';'     { Token Semi _ _ }
  op      { Token Op _ _ }
  ','     { Token Comma _ _ }

%%

Module : Defns Entry  { Module (reverse $1) $2 }

Prefix : id { mkIdn $1 }

Infix : op  { mkIdn $1 }

Params_ : Params_ Prefix  { $2 : $1 }
        | Prefix          { [$1] }

Params : Params_  { asNonEmpty (reverse $1) }

OptBar : '|'          {}
       | {- empty -}  {}

TopExpr : ext str { ExtExpr $ Ext (transformStr $ tokenLexeme $2) (range $1 <> range $2) }
        | Expr    { $1 }

Expr : NonMatchExpr { $1 }
     | MatchExpr    { $1 }

NonMatchExpr : fn Params '->' Expr          { Fun $2 $4 (range $1 <> range $4) }
             | if Expr then Expr else Expr  { Cond $2 $4 $6 (range $1 <> range $6) }
             | Prefix '<-' Expr             { Mut $1 $3 }
             | debug Expr                   { Debug $2 (range $1 <> range $2) }
             | Chain                        { chainToExpr $1 }

MatchExpr : match Expr with OptBar Matches  { PatternMatch $2 $5 (range $1 <> (range . snd . NEL.last) $5) }

Matches_ : Matches_ '|' Match { $3 : $1 }
         | Match              { [$1] }

Matches : Matches_  { asNonEmpty (reverse $1) }

Match : App '->' NonMatchExpr { ($1, $3) }

Chain : App             { Operand' $1 }
      | Chain Infix App { Operation' $1 $2 $3 }

App : App Access  { App $1 $2 }
    | Access      { $1 }

Access : Access '.' Prefix  { Access $1 $3 }
       | Access '.' nat     { Index $1 (read $ T.unpack $ tokenLexeme $3) (range $1 <> range $3) }
       | Atom               { $1 }

Atom : '(' Exprs ')'  { if length $2 == 1 then NEL.head $2 else mkTuple $2 (range $1 <> range $3) }
     | '(' ')'        { Literal Unit (range $1 <> range $2) }
     | '{' Obj '}'    { Record $2 (range $1 <> range $3) }
     | '{' Expr '}'   { $2 }
     | do '{' Stmts ';' Expr '}'  { Block (asNonEmpty (reverse $3)) $5 (range $1 <> range $6) }
     | Prefix         { Var $1 }
     | '(' op ')'     { Var $ Id (tokenLexeme $2) (range $1 <> range $3) }
     | Int            { $1 }
     | float          { Literal (Float $ read $ T.unpack $ tokenLexeme $1) (range $1) }
     | false          { Literal (Bool False) (range $1) }
     | true           { Literal (Bool True) (range $1) }
     | str            { mkStr $1 }
     | discard        { Discard (range $1) }

Exprs_ : Exprs_ ',' Expr  { $3 : $1 }
       | Expr             { [$1] }

Exprs : Exprs_  { asNonEmpty (reverse $1) }

Int : nat     { Literal (Int $ read $ T.unpack $ tokenLexeme $1) (range $1) }
    | nonnat  { Literal (Int $ read $ T.unpack $ tokenLexeme $1) (range $1) }

Obj : Props { asNonEmpty (reverse $1) }

Props : Props ',' Prop  { $3 : $1 }
      | Prop            { [$1] }

Prop : Prefix '=' Expr  { ($1, $3) }
     | '=' Prefix       { ($2, Var $2) }

Stmts : Stmts ';' Stmt  { $3 : $1 }
      | Stmt            { [$1] }

Stmt : Expr                 { Do $1 }
     | Prefix '=' Expr      { Let False $1 () $3 }
     | mut Prefix '=' Expr  { Let True $2 () $4 }

Entry : Expr        { Just $1 }
      | {- empty -} { Nothing }

Defns : Defns Defn ';'  { $2 : $1 }
      | {- empty -}     { [] }

Defn : Fix '(' op ')'         { FixDefn $1 (Id (tokenLexeme $3) (range $2 <> range $4)) }
     | type with OptBar Ctors { DataDefn (asNonEmpty (reverse $4)) }
     | Binds                  { handleBinds (reverse $1) }

Fix : Assoc nat { Fixity $1 (read $ T.unpack $ tokenLexeme $2) }

Assoc : infix   { NonAssoc }
      | infixl  { LeftAssoc }
      | infixr  { RightAssoc }

Ctors : Ctors '|' Ctor  { $3 : $1 }
      | Ctor            { [$1] }

Ctor : Prefix         { CtorDefn $1 [] (range $1) }
     | Prefix Params  { CtorDefn $1 (NEL.toList $2) (range $1 <> range (NEL.last $2)) }

Binds : Binds and Bind  { $3 : $1 }
      | Bind            { [$1] }

Bind : Prefix '=' TopExpr               { Bind $1 () $3 }
     | Prefix Params '=' TopExpr        { Bind $1 () (Fun $2 $4 (range $1 <> range $4)) }
     | Prefix Infix Prefix '=' TopExpr  { Bind $2 () (Fun ($1 :| [$3]) $5 (range $1 <> range $5)) }

{
mkIdn tok = Id (tokenLexeme tok) (range tok)

transformStr = T.tail . T.init

mkStr tok = Literal (Str $ transformStr $ tokenLexeme tok) (range tok)

mkTuple (x :| (y : zs)) r = Tuple (NonEmpty2 x y zs) r

asNonEmpty (x : xs) = x :| xs

chainToExpr (Operand' expr) = expr
chainToExpr chain = Chain (fromLRChain chain)

handleBinds [bind] = Defn bind
handleBinds (x : y : zs) = MRDefns (NonEmpty2 x y zs)

parseError tokens = error . show . head $ tokens
}
