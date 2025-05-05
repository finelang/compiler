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
import Fine.Syntax.Utils (mkDataDefn, mkAppOrFun)
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  and       { Token Lex.AndKw _ _ }
  debug     { Token Lex.Debug _ _ }
  else      { Token Lex.Else _ _ }
  fn        { Token Lex.Fn _ _ }
  forall    { Token Lex.Forall _ _ }
  foreign   { Token Lex.Foreign _ _ }
  if        { Token Lex.If _ _ }
  let       { Token Lex.Let _ _ }
  match     { Token Lex.Match _ _ }
  mut       { Token Lex.Mut _ _ }
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
  '='       { Token Lex.Assign _ _ }
  '.'       { Token Lex.Dot _ _ }
  ':'       { Token Lex.Colon _ _ }
  '|>'      { Token Lex.Pipe _ _ }
  '&&'      { Token Lex.And _ _ }
  '||'      { Token Lex.Or _ _ }
  '<='      { Token Lex.Le _ _ }
  '>='      { Token Lex.Ge _ _ }
  '@'       { Token Lex.Ccat _ _ }
  '=='      { Token Lex.Eq _ _ }
  '!='      { Token Lex.Neq _ _ }
  '>'       { Token Lex.Gt _ _ }
  '<'       { Token Lex.Lt _ _ }
  '+'       { Token Lex.Add _ _ }
  '-'       { Token Lex.Sub _ _ }
  '*'       { Token Lex.Mult _ _ }
  '/'       { Token Lex.Div _ _ }
  '%'       { Token Lex.Rest _ _ }
  '('       { Token Lex.Opar _ _ }
  ')'       { Token Lex.Cpar _ _ }
  '{'       { Token Lex.Obrace _ _ }
  '}'       { Token Lex.Cbrace _ _ }
  '['       { Token Lex.Osquare _ _ }
  ']'       { Token Lex.Csquare _ _ }
  ','       { Token Lex.Comma _ _ }
  ';'       { Token Lex.Semi _ _ }
  '\''      { Token Lex.Tick _ _ }

%expect 0

%%

Module : Defns Entry  { ParsedModule (reverse $1) $2 }

-- COMMON

Id : id { Id (range $1) (tokenLexeme $1) }

Ct : capid  { Id (range $1) (tokenLexeme $1) }

Params_ : Params_ ',' Id  { $3 : $1 }
        | Id              { [$1] }

Params : Params_  { toNonEmptyPARTIAL (reverse $1) }

OptParams : Params      { $1 }
          | {- empty -} { Id NoRange "_" :| [] }

OptSemi : ';'         {}
        | {- empty -} {}

-- PATTERN

Patterns_ : Patterns_ ',' Pattern { $3 : $1 }
          | Pattern               { [$1] }

Patterns : Patterns_  { toNonEmptyPARTIAL (reverse $1) }

IntPatt : nat     { LiteralP (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }
        | nonnat  { LiteralP (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }

PropPatterns : PropPatterns ',' PropPattern { $3 : $1 }
             | PropPattern                  { [$1] }
             | {- empty -}                  { [] }

PropPattern : Id '=' Pattern  { ($1, $3) }
            | '=' Id          { ($2, Capture $2) }

Pattern : Ct          { DataP (range $1) $1 [] }
        | '(' ')'     { LiteralP (range $1 <> range $2) Unit }
        | '[' ']'     { ListP (range $1 <> range $2) [] }
        | IntPatt     { $1 }
        | floatlit    { LiteralP (range $1) (Float $ read $ Text.unpack $ tokenLexeme $1) }
        | true        { LiteralP (range $1) (Bool True) }
        | false       { LiteralP (range $1) (Bool False) }
        | strlit      { LiteralP (range $1) (Str $ extractStr $1) }
        | Id          { Capture $1 }
        | discard     { Discard (range $1) }
        | DeepPattern { $1 }

DeepPattern : Ct '(' Patterns ')'   { DataP (range $1 <> range $4) $1 (NonEmpty.toList $3) }
            | '(' Patterns ')'      { if NonEmpty.length $2 >= 2 then uncurry3 (TupleP (range $1 <> range $3)) (uncons2 $2) else NonEmpty.head $2 }
            | '{' PropPatterns '}'  { RecordP (range $1 <> range $3) (reverse $2) }
            | '[' Patterns ']'      { ListP (range $1 <> range $3) (NonEmpty.toList $2) }

-- BLOCK

Stmts : Stmts Stmt  { $2 : $1 }
      | Stmt        { [$1] }

Stmt : let Id '=' Expr ';'                  { Let False $2 $4 }
     | let mut Id '=' Expr ';'              { Let True $3 $5 }
     | let DeepPattern '=' Expr ';'         { LetPatt () $2 $4 }
     | Expr ';'                             { Do $1 }
     | Id '=' Expr ';'                      { Mut $1 $3 }
     | debug Expr ';'                       { Debug $2 }
     | while Expr '{' VoidBlock '}' OptSemi { Loop $2 $4 }

Block : Stmts Expr { foldl' (&) (Return $2) $1 }

VoidBlock : Stmts { foldl' (&) Void $1 }

-- EXPR

Int : nat     { Literal (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }
    | nonnat  { Literal (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }

Props : Props ',' Prop  { $3 : $1 }
      | Prop            { [$1] }
      | {- empty -}     { [] }

Prop : Id '=' Expr  { ($1, $3) }
     | '=' Id       { ($2, Var (range $2) $2) }

Exprs_ : Exprs_ ',' Expr  { $3 : $1 }
       | Expr             { [$1] }

Exprs : Exprs_  { toNonEmptyPARTIAL (reverse $1) }

Matches : Matches Match ';' { $2 : $1 }
        | Match ';'         { [$1] }

Match : Pattern '->' Expr { ($1, $3) }

Expr : Equation                         { equationToExpr $1 }
     | if Expr then Expr else Expr      { Cond (range $1 <> range $6) $2 $4 $6 }
     | fn '(' OptParams ')' '->' Expr   { Fun (range $1 <> range $6) $3 $6 }
     | fn '\'' '(' Params ')' '->' Expr { GenFun (range $1 <> range $7) $4 $7 }

Arg : Expr    { Right $1 }
    | discard { Left (range $1) }

Args_ : Args_ ',' Arg { $3 : $1 }
      | Arg           { [$1] }

Args : Args_  { toNonEmptyPARTIAL (reverse $1) }

App : App '(' Args ')'        { mkAppOrFun (range $1 <> range $4) $1 $3 }
    | App '(' ')'             { mkAppOrFun (range $1 <> range $3) $1 ((Right $ Literal (range $2 <> range $3) Unit) :| []) }
    | App '\'' '(' Types ')'  { GenApp (range $1 <> range $5) $1 $4 }
    | App '.' Id              { Access (range $1 <> range $3) $1 $3 }
    | App '.' nat             { Index (range $1 <> range $3) $1 (read $ Text.unpack $ tokenLexeme $3) }
    | Atom                    { $1 }

Atom ::                                   { Expr Parsed }
Atom : '(' Exprs ')'                      { if NonEmpty.length $2 >= 2 then uncurry3 (Tuple (range $1 <> range $3)) (uncons2 $2) else NonEmpty.head $2 }
     | '(' ')'                            { Literal (range $1 <> range $2) Unit }
     | '{' Props '}'                      { Record (range $1 <> range $3) (reverse $2) }
     | '{' Block '}'                      { Block (range $1 <> range $3) $2 }
     | '{' Expr '}'                       { $2 }
     | '[' Exprs ']'                      { List (range $1 <> range $3) (NonEmpty.toList $2) }
     | '[' ']'                            { List (range $1 <> range $2) [] }
     | Int                                { $1 }
     | floatlit                           { Literal (range $1) (Float $ read $ Text.unpack $ tokenLexeme $1) }
     | true                               { Literal (range $1) (Bool True) }
     | false                              { Literal (range $1) (Bool False) }
     | strlit                             { Literal (range $1) (Str $ extractStr $1) }
     | Id                                 { Var (range $1) $1 }
     | Ct                                 { Var (range $1) $1 }
     | match Expr '{' Matches '}'         { PatternMatching (range $1 <> range $5) $2 (toNonEmptyPARTIAL (reverse $4)) }
     | fn '(' OptParams ')' '{' Expr '}'  { Fun (range $1 <> range $7) $3 $6 }
     | fn '(' OptParams ')' '{' Block '}' { Fun (range $1 <> range $7) $3 (Block (range $5 <> range $7) $6) }
     | fn '(' PartialEquation ')'         { PartialEquation (range $1 <> range $4) $3 }

Equation : App Op Equation  { Operation $1 $2 $3 }
         | App              { Operand $1 }

Op : '|>' { Pipe }
   | '&&' { And }
   | '||' { Or }
   | '<=' { Le }
   | '>=' { Ge }
   | '==' { Eq }
   | '!=' { Neq }
   | '<'  { Lt }
   | '>'  { Gt }
   | '+'  { Add }
   | '-'  { Sub }
   | '*'  { Mult }
   | '/'  { Div }
   | '%'  { Rest }
   | '@'  { Concat }

PartialEquation : PartialOperand Op PartialEquation_  { Operation $1 $2 $3 }

PartialEquation_ : PartialOperand Op PartialEquation_ { Operation $1 $2 $3 }
                 | PartialOperand                     { Operand $1 }

PartialOperand : App      { Right $1 }
               | discard  { Left (range $1) }

-- TYPE

PropTypes : PropTypes ',' PropType  { $3 : $1 }
          | PropType                { [$1] }
          | {- empty -}             { [] }

PropType : Id ':' Type  { ($1, $3) }

Types_ : Types_ ',' Type  { $3 : $1 }
       | Type             { [$1] }

Types : Types_  { toNonEmptyPARTIAL (reverse $1) }

TVars : TVars Id  { $2 : $1 }
      | Id        { [$1] }

Type : forall TVars '.' Type      { Forall (range $1 <> range $4) (toNonEmptyPARTIAL (reverse $2)) $4 }
     | fn '(' Types ')' '->' Type { FunT (range $1 <> range $6) $3 $6 }
     | TApp '->' Type             { FunT (range $1 <> range $3) ($1 :| []) $3 }
     | TApp                       { $1 }

TApp : TApp '(' Types ')' { TApp (range $1 <> range $4) $1 $3 }
     | TAtom              { $1 }

TAtom ::                  { Type Parsed }
TAtom : '(' Types ')'     { if NonEmpty.length $2 >= 2 then uncurry3 (TupleT (range $1 <> range $3)) (uncons2 $2) else NonEmpty.head $2 }
      | '{' PropTypes '}' { RecordT (range $1 <> range $3) (reverse $2) }
      | '(' ')'           { LiteralT (range $1 <> range $2) UnitT }
      | '[' Type ']'      { ListT (range $1 <> range $3) $2 }
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

Defns : Defns Defn OptSemi  { $2 : $1 }
      | {- empty -}         { [] }

Defn : type Id '(' Params ')' '=' Type      { TypeDefn (TypeBind $2 (TFun (range $2 <> range $7) $4 $7)) }
     | type Id '=' Type                     { TypeDefn (TypeBind $2 $4) }
     | type Ct '(' Params ')' '{' Ctors '}' { mkDataDefn $2 (Just $4) $7 }
     | type Ct '{' Ctors '}'                { mkDataDefn $2 Nothing $4 }
     | let foreign Id ':' Type '=' strlit   { Defn (ForeignBind $3 $5 (extractStr $7)) }
     | let MutRecBinds                      { if NonEmpty.length $2 > 1 then MutRecDefns $2 else Defn (NonEmpty.head $2) }

MutRecBinds_ : MutRecBinds_ and ExprBind  { $3 : $1 }
             | ExprBind                   { [$1] }

MutRecBinds : MutRecBinds_  { toNonEmptyPARTIAL (reverse $1) }

ExprBind : Id ':' Type '=' Expr { ExprBind $1 $3 $5 }

Ctors_ : Ctors_ Ctor OptSemi  { $2 : $1 }
       | Ctor OptSemi         { [$1] }

Ctors : Ctors_  { toNonEmptyPARTIAL (reverse $1) }

Ctor : Ct               { ($1, []) }
     | Ct '(' Types ')' { ($1, NonEmpty.toList $3) }

{
extractStr = Text.tail . Text.init . tokenLexeme

tryUnblock _ (Return expr) = expr
tryUnblock r block = Block r block

uncurry3 f (x, y, z) = f x y z

uncons2 (x :| (y : zs)) = (x, y, zs)

equationToExpr (Operand expr) = expr
equationToExpr equation = Equation NoRange equation

parseError tokens = error . show . head $ tokens
}
