{
{-# LANGUAGE NoStrictData #-}
module Fine.Parser (parseTokens) where

import Data.Function ((&))
import Data.List.Extra (toNonEmptyPARTIAL)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Fine.Lexer (Token (..))
import qualified Fine.Lexer as Lex
import Fine.Syntax
import Fine.Syntax.Utils (mkDataDefn, mkExprDefn)
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  case      { Token Lex.Case _ _ }
  debug     { Token Lex.Debug _ _ }
  do        { Token Lex.Do _ _ }
  else      { Token Lex.Else _ _ }
  foreign   { Token Lex.Foreign _ _ }
  if        { Token Lex.If _ _ }
  infix     { Token Lex.Infix _ _ }
  infixl    { Token Lex.Infixl _ _ }
  infixr    { Token Lex.Infixr _ _ }
  let       { Token Lex.Let _ _ }
  mut       { Token Lex.Mut _ _ }
  of        { Token Lex.Of _ _ }
  run       { Token Lex.Run _ _ }
  then      { Token Lex.Then _ _ }
  type      { Token Lex.Type _ _ }
  while     { Token Lex.While _ _ }
  false     { Token Lex.FalseTok _ _ }
  float     { Token Lex.Float _ _ }
  int       { Token Lex.Int _ _ }
  str       { Token Lex.Str _ _ }
  true      { Token Lex.TrueTok _ _ }
  bool      { Token Lex.Bool _ _ }
  void      { Token Lex.Void _ _ }
  discard   { Token Lex.Discard _ _ }
  id        { Token Lex.Id _ _ }
  capid     { Token Lex.CapId _ _ }
  strlit    { Token Lex.StrLit _ _ }
  nat       { Token Lex.Nat _ _ }
  nonnat    { Token Lex.NonNat _ _ }
  floatlit  { Token Lex.FloatLit _ _ }
  '->'      { Token Lex.Arrow _ _ }
  '<-'      { Token Lex.RArrow _ _ }
  '='       { Token Lex.Eq _ _ }
  '.'       { Token Lex.Dot _ _ }
  ':'       { Token Lex.Colon _ _ }
  op        { Token Lex.Op _ _ }
  '('       { Token Lex.Opar _ _ }
  ')'       { Token Lex.Cpar _ _ }
  '{'       { Token Lex.Obrace _ _ }
  '}'       { Token Lex.Cbrace _ _ }
  '['       { Token Lex.Osquare _ _ }
  ']'       { Token Lex.Csquare _ _ }
  ','       { Token Lex.Comma _ _ }

%%

Module : Defns Entry  { ParsedModule (reverse $1) $2 }

-- COMMON

Id : id { Id (range $1) (tokenLexeme $1) }

Ct : capid  { Id (range $1) (tokenLexeme $1) }

InfixOp : op  { Op (range $1) (tokenLexeme $1) }

PrefixOp : '(' op ')' { Op (range $1 <> range $3) (tokenLexeme $2) }

TopId : Id        { $1 }
      | PrefixOp  { $1 }

Params_ : Params_ ',' Id  { $3 : $1 }
        | Id              { [$1] }

Params : Params_  { toNonEmptyPARTIAL (reverse $1) }

OptParams : Params      { $1 }
          | {- empty -} { Id NoRange "_" :| [] }

-- PATTERN

Patterns_ : Patterns_ ',' Pattern { $3 : $1 }
          | Pattern               { [$1] }

Patterns : Patterns_  { toNonEmptyPARTIAL (reverse $1) }

IntPatt : nat     { LiteralP (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }
        | nonnat  { LiteralP (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }

PropPatterns : PropPatterns ',' PropPattern { $3 : $1 }
             | PropPattern                  { [$1] }

PropPattern : Id '=' Pattern  { ($1, $3) }
            | '=' Id          { ($2, Capture $2) }

Pattern : Ct                    { DataP (range $1) $1 [] }
        | Ct '(' Patterns ')'   { DataP (range $1 <> range $4) $1 (NonEmpty.toList $3) }
        | '(' Patterns ')'      { if NonEmpty.length $2 > 1 then TupleP (range $1 <> range $3) $2 else NonEmpty.head $2 }
        | '(' ')'               { LiteralP (range $1 <> range $2) Unit }
        | '{' PropPatterns '}'  { RecordP (range $1 <> range $3) (toNonEmptyPARTIAL (reverse $2)) }
        | IntPatt               { $1 }
        | floatlit              { LiteralP (range $1) (Float $ read $ Text.unpack $ tokenLexeme $1) }
        | true                  { LiteralP (range $1) (Bool True) }
        | false                 { LiteralP (range $1) (Bool False) }
        | strlit                { LiteralP (range $1) (Str $ extractStr $1) }
        | Id                    { Capture $1 }
        | discard               { Discard (range $1) }

-- BLOCK

Stmts : Stmts Stmt  { $2 : $1 }
      | Stmt        { [$1] }

Stmt : let Id '=' Expr              { Let False $2 $4 }
     | let mut Id '=' Expr          { Let True $3 $5 }
     | do Expr                      { Do $2 }
     | do Id '<-' Expr              { Mut $2 $4 }
     | debug Expr                   { Debug $2 }
     | while Expr '{' VoidBlock '}' { Loop $2 $4 }

Block : Stmts then Expr { foldl' (&) (Return $3) $1 }

VoidBlock : Stmts { foldl' (&) Void $1 }

-- EXPR

Int : nat     { Literal (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }
    | nonnat  { Literal (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }

Props : Props ',' Prop  { $3 : $1 }
      | Prop            { [$1] }

Prop : Id '=' Expr  { ($1, $3) }
     | '=' Id       { ($2, Var (range $2) $2) }

Exprs_ : Exprs_ ',' Expr  { $3 : $1 }
       | Expr             { [$1] }

Exprs : Exprs_  { toNonEmptyPARTIAL (reverse $1) }

Matches : Matches Match { $2 : $1 }
        | Match         { [$1] }

Match : of Pattern '->' Expr  { ($2, $4) }

Expr : Chain                            { tryUnchain $1 }
     | Block                            { Block NoRange $1 }
     | if Expr then Expr else Expr      { Cond (range $1 <> range $6) $2 $4 $6 }
     | '.' '(' OptParams ')' '->' Expr  { Fun (range $1 <> range $6) $3 $6 }
     | '.' '[' Params ']' '->' Expr     { GenFun (range $1 <> range $6) $3 $6 }

Chain : App               { Operand $1 }
      | App InfixOp Chain { Operation $1 $2 $3 }

App : App '(' Exprs ')' { App (range $1 <> range $4) $1 $3 }
    | App '(' ')'       { App (range $1 <> range $3) $1 (Literal (range $2 <> range $3) Unit :| []) }
    | App '[' Types ']' { GenApp (range $1 <> range $4) $1 $3 }
    | App '.' Id        { Access (range $1 <> range $3) $1 $3 }
    | App '.' nat       { Index (range $1 <> range $3) $1 (read $ Text.unpack $ tokenLexeme $3) }
    | Atom              { $1 }

Atom ::                           { Expr Parsed }
Atom : '(' Exprs ')'              { if NonEmpty.length $2 > 1 then Tuple (range $1 <> range $3) $2 else NonEmpty.head $2 }
     | '(' ')'                    { Literal (range $1 <> range $2) Unit }
     | '{' Props '}'              { Record (range $1 <> range $3) (toNonEmptyPARTIAL (reverse $2)) }
     | '{' Expr '}'               { $2 }
     | Int                        { $1 }
     | floatlit                   { Literal (range $1) (Float $ read $ Text.unpack $ tokenLexeme $1) }
     | true                       { Literal (range $1) (Bool True) }
     | false                      { Literal (range $1) (Bool False) }
     | strlit                     { Literal (range $1) (Str $ extractStr $1) }
     | TopId                      { Var (range $1) $1 }
     | Ct                         { Var (range $1) $1 }
     | case Expr '{' Matches '}'  { PatternMatching (range $1 <> range $5) $2 (toNonEmptyPARTIAL (reverse $4)) }

-- TYPE

PropTypes : PropTypes ',' PropType  { $3 : $1 }
          | PropType                { [$1] }

PropType : Id ':' Type  { ($1, $3) }

Types_ : Types_ ',' Type  { $3 : $1 }
       | Type             { [$1] }

Types : Types_  { toNonEmptyPARTIAL (reverse $1) }

Type : '.' '(' Types ')' '->' Type  { FunT (range $1 <> range $6) $3 $6 }
     | '.' '(' ')' '->' Type        { FunT (range $1 <> range $5) (LiteralT (range $2 <> range $3) UnitT :| []) $5 }
     | '.' '[' Params ']' '->' Type { Forall (range $1 <> range $6) $3 $6 }
     | TApp                         { $1 }

TApp : TApp '[' Types ']' { TApp (range $1 <> range $4) $1 $3 }
     | TAtom              { $1 }

TAtom ::                  { Type Parsed }
TAtom : '(' Types ')'     { if NonEmpty.length $2 > 1 then TupleT (range $1 <> range $3) $2 else NonEmpty.head $2 }
      | '{' PropTypes '}' { RecordT (range $1 <> range $3) (toNonEmptyPARTIAL (reverse $2)) }
      | '(' ')'           { LiteralT (range $1 <> range $2) UnitT }
      | bool              { LiteralT (range $1) BoolT }
      | int               { LiteralT (range $1) IntT }
      | str               { LiteralT (range $1) StrT }
      | float             { LiteralT (range $1) FloatT }
      | void              { VoidT (range $1) }
      | Id                { TVar (range $1) $1 }
      | Ct                { TVar (range $1) $1 }

-- MODULE

Entry : run Expr    { Just $2 }
      | {- empty -} { Nothing }

Defns : Defns Defn  { $2 : $1 }
      | {- empty -} { [] }

Defn : Fix PrefixOp                                                   { FixDefn $1 $2 }
     | type Id '[' Params ']' '=' Type                                { TypeDefn (TypeBind $2 (TFun (range $2 <> range $7) $4 $7)) }
     | type Id '=' Type                                               { TypeDefn (TypeBind $2 $4) }
     | type Ct '[' Params ']' '{' Ctors '}'                           { mkDataDefn $2 (Just $4) $7 }
     | type Ct '{' Ctors '}'                                          { mkDataDefn $2 Nothing $4 }
     | let TopId ':' Type '=' Expr                                    { Defn (ExprBind $2 $4 $6) }
     | let foreign TopId ':' Type '=' strlit                          { Defn (ForeignBind $3 $5 (extractStr $7)) }
     | let TopId '[' Params ']' ':' Type '=' Expr                     { Defn (ExprBind $2 (Forall NoRange $4 $7) (GenFun NoRange $4 $9)) }
     | let TopId '(' TypedParams ')' ':' Type '=' Expr                { mkExprDefn $2 Nothing $4 $7 $9 }
     | let TopId '[' Params ']' '(' TypedParams ')' ':' Type '=' Expr { mkExprDefn $2 (Just $4) $7 $10 $12 }

TypedParams_ : TypedParams_ ',' TypedParam  { $3 : $1 }
             | TypedParam                   { [$1] }

TypedParam: Id ':' Type { ($1, $3) }

TypedParams : TypedParams_  { toNonEmptyPARTIAL (reverse $1) }
            | {- empty -}   { (Id NoRange "_", LiteralT NoRange UnitT) :| [] }

Ctors_ : Ctors_ Ctor  { $2 : $1 }
       | Ctor         { [$1] }

Ctors : Ctors_  { toNonEmptyPARTIAL (reverse $1) }

Ctor : Ct                     { ($1, Nothing) }
     | Ct '(' TypedParams ')' { ($1, Just $3) }

Fix : Assoc nat { Fixity $1 (read $ Text.unpack $ tokenLexeme $2) }

Assoc : infix   { NonAssoc }
      | infixl  { LeftAssoc }
      | infixr  { RightAssoc }

{
extractStr = Text.tail . Text.init . tokenLexeme

tryUnchain (Operand expr) = expr
tryUnchain (Operation left op (Operand right)) =
  App (range left <> range right) (Var (range op) op) (left :| [right])
tryUnchain chain = Chain (range chain) chain

tryUnblock _ (Return expr) = expr
tryUnblock r block = Block r block

parseError tokens = error . show . head $ tokens
}
