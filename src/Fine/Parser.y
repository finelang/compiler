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
import Fine.Syntax.Name (irrelevant, matchedParam)
import Fine.Syntax.Utils (mkDataDefn)
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  debug     { Token Lex.Debug _ _ }
  else      { Token Lex.Else _ _ }
  forall    { Token Lex.Forall _ _ }
  foreign   { Token Lex.Foreign _ _ }
  if        { Token Lex.If _ _ }
  let       { Token Lex.Let _ _ }
  match     { Token Lex.Match _ _ }
  mut       { Token Lex.Mut _ _ }
  run       { Token Lex.Run _ _ }
  then      { Token Lex.Then _ _ }
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
  '\\'      { Token Lex.BSlash _ _ }
  '|>'      { Token Lex.Pipe _ _ }
  '<|'      { Token Lex.RPipe _ _ }
  '<.'      { Token Lex.Comp _ _ }
  '.>'      { Token Lex.RComp _ _ }
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
  ','       { Token Lex.Comma _ _ }
  ';'       { Token Lex.Semi _ _ }

%expect 0

%%

Module : Defns Entry  { ParsedModule (reverse $1) $2 }

-- COMMON

Id : id { Id (range $1) (tokenLexeme $1) }

TyId : capid  { Id (range $1) (tokenLexeme $1) }

Params_ : Params_ Id  { $2 : $1 }
        | Id          { [$1] }

Params : Params_  { toNonEmptyPARTIAL (reverse $1) }

OptParams : Params      { $1 }
          | {- empty -} { irrelevant :| [] }

-- PATTERN

Pattern : '.' Id PAtoms { DataP (range $1 <> range (head $3)) $2 (reverse $3) }
        | PAtom         { $1 }

PAtom : '.' Id                { DataP (range $1 <> range $2) $2 [] }
      | '(' CommaPatterns ')' { mkTupleP (range $1 <> range $3) (reverse $2) }
      | '{' PropPatterns '}'  { RecordP (range $1 <> range $3) (reverse $2) }
      | IntPatt               { $1 }
      | floatlit              { LiteralP (range $1) (Float $ read $ Text.unpack $ tokenLexeme $1) }
      | true                  { LiteralP (range $1) (Bool True) }
      | false                 { LiteralP (range $1) (Bool False) }
      | strlit                { LiteralP (range $1) (Str $ extractStr $1) }
      | Id                    { Capture $1 }
      | discard               { Discard (range $1) }

PAtoms : PAtoms PAtom { $2 : $1 }
       | PAtom        { [$1] }

IntPatt : nat     { LiteralP (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }
        | nonnat  { LiteralP (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }

PropPatterns : PropPatterns ',' PropPattern { $3 : $1 }
             | PropPattern                  { [$1] }
             | {- empty -}                  { [] }

PropPattern : Id '=' Pattern  { ($1, $3) }
            | '=' Id          { ($2, Capture $2) }

CommaPatterns : CommaPatterns ',' Pattern { $3 : $1 }
              | Pattern                   { [$1] }
              | {- empty -}               { [] }

-- BLOCK

Block : Stmts Expr { foldl (&) (Return $2) $1 }

VoidBlock : Stmts { foldl (&) Void $1 }

Stmts : Stmts Stmt ';'  { $2 : $1 }
      | Stmt ';'        { [$1] }

Stmt : let mut Id '=' Expr            { LetMut $3 $5 }
     | Id '=' Expr                    { Mut $1 $3 }
     | let Pattern '=' Expr           { Let () $2 $4 }
     | Expr                           { Do $1 }
     | debug Expr                     { Debug (range $1) $2 }
     | while Access '{' VoidBlock '}' { Loop $2 $4 }

-- EXPR

Expr : Equation                     { equationToExpr $1 }
     | if Expr then Expr else Expr  { Cond () (range $1 <> range $6) $2 $4 $6 }
     | '\\' OptParams '->' Expr     { foldr (Fun ()) $4 $2 }

Equation : App Op Equation  { Operation $1 $2 $3 }
         | App              { Operand $1 }

Op : '|>' { Pipe }
   | '<|' { RPipe }
   | '<.' { Comp }
   | '.>' { RComp }
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

App : App Access  { App () $1 $2 }
    | Access      { $1 }

Access : Access '.' Id  { Access () $1 $3 }
       | Access '.' nat { Index () (range $1 <> range $3) $1 (read $ Text.unpack $ tokenLexeme $3) }
       | Atom           { $1 }

Atom ::                             { Expr Parsed }
Atom : '(' OptExprs ')'             { mkTuple (range $1 <> range $3) (reverse $2) }
     | '{' OptProps '}'             { Record () (range $1 <> range $3) (reverse $2) }
     | '{' Block '}'                { Block () (range $1 <> range $3) $2 }
     | '{' Expr '}'                 { $2 }
     | Int                          { $1 }
     | floatlit                     { Literal () (range $1) (Float $ read $ Text.unpack $ tokenLexeme $1) }
     | true                         { Literal () (range $1) (Bool True) }
     | false                        { Literal () (range $1) (Bool False) }
     | strlit                       { Literal () (range $1) (Str $ extractStr $1) }
     | Id                           { Var () $1 }
     | match Access '{' Matches '}' { PatternMatching () (range $1 <> range $5) () $2 $4 }
     | '\\' match '{' Matches '}'   { mkMatchFun (range $1 <> range $5) (matchedParam (range $2)) $4 }
-- TODO: gen app

Matches_ : Matches_ ';' Match { $3 : $1 }
         | Match              { [$1] }

Matches : Matches_  { toNonEmptyPARTIAL (reverse $1) }

Match : Pattern '->' Expr { ($1, $3) }

Int : nat     { Literal () (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }
    | nonnat  { Literal () (range $1) (Int $ read $ Text.unpack $ tokenLexeme $1) }

Exprs : Exprs ',' Expr  { $3 : $1 }
      | Expr            { [$1] }

OptExprs : Exprs        { $1 }
         | {- empty -}  { [] }                

Props : Props ',' Prop  { $3 : $1 }
      | Prop            { [$1] }

Prop : Id '=' Expr  { ($1, $3) }
     | '=' Id       { ($2, Var () $2) }

OptProps : Props        { $1 }
         | {- empty -}  { [] }

-- TYPE

PropTypes : PropTypes ',' PropType  { $3 : $1 }
          | PropType                { [$1] }
          | {- empty -}             { [] }

PropType : Id ':' Type  { ($1, $3) }

Types_ : Types_ ',' Type  { $3 : $1 }
       | Type             { [$1] }

Types : Types_  { toNonEmptyPARTIAL (reverse $1) }

Forall : forall Params '.' Type { Forall () (range $1 <> range $4) $2 $4 }
       | Type                   { $1 }

Type : TApp '->' Type { FunT () $1 $3 }
     | TApp           { $1 }

TApp : TApp TAtom { TApp () $1 $2 }
     | TAtom      { $1 }

TAtom ::                  { Type Parsed }
TAtom : '(' Types ')'     { mkTupleT (range $1 <> range $3) $2 }
      | '{' PropTypes '}' { RecordT () (range $1 <> range $3) (reverse $2) }
      | '(' ')'           { LiteralT () (range $1 <> range $2) UnitT }
      | bool              { LiteralT () (range $1) BoolT }
      | int               { LiteralT () (range $1) IntT }
      | str               { LiteralT () (range $1) StrT }
      | float             { LiteralT () (range $1) FloatT }
      | void              { VoidT () (range $1) }
      | Id                { TVar () $1 }
      | TyId              { TVar () $1 }

TAtoms : TAtoms TAtom { $2 : $1 }
       | TAtom        { [$1] }

-- MODULE

Entry : run Expr    { Just $2 }
      | {- empty -} { Nothing }

Defns : Defns Defn  { $2 : $1 }
      | {- empty -} { [] }

Defn : let TyId Params '=' Type       { TypeDefn (TypeBind $2 (foldr (TFun ()) $5 $3)) }
     | let TyId '=' Type              { TypeDefn (TypeBind $2 $4) }
     | let TyId Params '{' Ctors '}'  { mkDataDefn $2 (Just $3) $5 }
     | let TyId '{' Ctors '}'         { mkDataDefn $2 Nothing $4 }
     | let Id ':' Forall              { TypingDefn $2 $4 }
     | let foreign Id '=' strlit      { ForeignDefn $3 (extractStr $5) }
     | let Id '=' Expr                { ValueDefn $2 $4 }
     | let Id Params '=' Expr         { ValueDefn $2 (foldr (Fun ()) $5 $3) }

Ctors_ : Ctors_ ';' Ctor  { $3 : $1 }
       | Ctor             { [$1] }

Ctors : Ctors_  { toNonEmptyPARTIAL (reverse $1) }

Ctor : Id         { ($1, []) }
     | Id TAtoms  { ($1, reverse $2) }

{
extractStr = Text.tail . Text.init . tokenLexeme

tryUnblock _ (Return expr) = expr
tryUnblock r block = Block () r block

snoc (x :| xs) y = x :| xs ++ [y]

equationToExpr (Operand expr) = expr
equationToExpr equation = Equation (range equation) () equation

mkTuple r [] = Literal () r Unit
mkTuple _ [expr] = expr
mkTuple r (e1 : e2 : exprs) = Tuple () r e1 e2 exprs

mkTupleP r [] = LiteralP r Unit
mkTupleP _ [p] = p
mkTupleP r (p1 : p2 : ps) = TupleP r p1 p2 ps

mkTupleT r (t :| []) = t
mkTupleT r (t1 :| (t2 : ts)) = TupleT () r t1 t2 ts

mkMatchFun r p ms = Fun () p (PatternMatching () r () (Var () p) ms)

parseError tokens = error . show . head $ tokens
}
