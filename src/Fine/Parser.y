{
{-# LANGUAGE NoStrictData #-}
module Fine.Parser (parseTokens) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.List.NonEmpty2 (NonEmpty2 (NonEmpty2))
import qualified Data.Text as T
import Fine.Lexer (Token (..), TokenType (..))
import Fine.Syntax.Common
import Fine.Syntax.Concrete
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  ext     { Token ExtTok _ _ }
  debug   { Token DebugTok _ _ }
  run     { Token Run _ _ }
  boolt   { Token BoolTTok _ _ }
  do      { Token DoTok _ _ }
  else    { Token Else _ _ }
  false   { Token FalseTok _ _ }
  forall  { Token ForallTok _ _ }
  if      { Token If _ _ }
  infix   { Token Infix _ _ }
  infixl  { Token Infixl _ _ }
  infixr  { Token Infixr _ _ }
  intt    { Token IntTTok _ _ }
  fn      { Token Fn _ _ }
  floatt  { Token FloatTTok _ _ }
  match   { Token Match _ _ }
  mut     { Token MutTok _ _ }
  strt    { Token StrTTok _ _ }
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
  ':'     { Token Colon _ _ }
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

Atom : '(' Exprs ')'              { if length $2 == 1 then NEL.head $2 else mkTuple $2 (range $1 <> range $3) }
     | '(' ')'                    { Literal Unit (range $1 <> range $2) }
     | '{' Obj '}'                { Record $2 (range $1 <> range $3) }
     | '{' Expr '}'               { $2 }
     | do '{' Stmts ';' Expr '}'  { Block (asNonEmpty (reverse $3)) $5 (range $1 <> range $6) }
     | Prefix                     { Var $1 }
     | '(' op ')'                 { Var $ Id (tokenLexeme $2) (range $1 <> range $3) }
     | Int                        { $1 }
     | float                      { Literal (Float $ read $ T.unpack $ tokenLexeme $1) (range $1) }
     | false                      { Literal (Bool False) (range $1) }
     | true                       { Literal (Bool True) (range $1) }
     | str                        { mkStr $1 }
     | discard                    { Discard (range $1) }

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

Stmt : Expr                           { Do $1 }
     | Prefix OptTyping '=' Expr      { Let False $1 $2 $4 }
     | mut Prefix OptTyping '=' Expr  { Let True $2 $3 $5 }

OptTyping : ':' Type    { Just $2 }
          | {- empty -} { Nothing }

Type : forall Params '.' Type { Forall (kindedVars $2) $4 (range $1 <> range $4) }
     | TApp '->' Type         { FunT $1 $3 }
     | TApp                   { $1 }

TApp : TApp TAtom { TApp $1 $2 }
     | TAtom      { $1 }

TAtom : '(' Types ')' { if length $2 == 1 then NEL.head $2 else mkTupleT $2 (range $1 <> range $3) }
      | '(' ')'       { LiteralT UnitT (range $1 <> range $2) }
      | '{' TObj '}'  { RecordT $2 (range $1 <> range $3) }
      | Prefix        { TVar $1 }
      | intt          { LiteralT IntT (range $1) }
      | floatt        { LiteralT FloatT (range $1) }
      | boolt         { LiteralT BoolT (range $1) }
      | strt          { LiteralT StrT (range $1) }

Types_ : Types_ ',' Type  { $3 : $1 }
       | Type             { [$1] }

Types : Types_  { asNonEmpty (reverse $1) }

TArgs_ : TArgs_ TAtom { $2 : $1 }
       | {- empty -}  { [] }

TArgs : TArgs_  { reverse $1 }

TObj : TProps { asNonEmpty (reverse $1) }

TProps : TProps ',' TProp { $3 : $1 }
       | TProp            { [$1] }

TProp : Prefix ':' Type { ($1, $3) }

Entry : run Expr    { Just $2 }
      | {- empty -} { Nothing }

Defns : Defns Defn ';'  { $2 : $1 }
      | {- empty -}     { [] }

Defn : Fix '(' op ')'                       { FixDefn $1 (Id (tokenLexeme $3) (range $2 <> range $4)) }
     | type Prefix with OptBar Ctors        { mkDataDefn $2 [] $5 }
     | type Prefix Params with OptBar Ctors { mkDataDefn $2 (NEL.toList $3) $6 }
     | type Prefix '=' Type                 { TypeDefn (Bind $2 (KLit (range $2)) $4) }
     | type Prefix Params '=' Type          { mkTypeDefn $2 $3 $5 }
     | Prefix ':' Type                      { TypingDefn $1 $3 }
     | Prefix '=' TopExpr                   { Defn $1 $3 }
     | Prefix Params '=' TopExpr            { Defn $1 (Fun $2 $4 (range $1 <> range $4)) }
     | Prefix Infix Prefix '=' TopExpr      { Defn $2 (Fun ($1 :| [$3]) $5 (range $1 <> range $5)) }

Fix : Assoc nat { Fixity $1 (read $ T.unpack $ tokenLexeme $2) }

Assoc : infix   { NonAssoc }
      | infixl  { LeftAssoc }
      | infixr  { RightAssoc }

Ctors_ : Ctors_ '|' Ctor  { $3 : $1 }
       | Ctor             { [$1] }

Ctors : Ctors_  { asNonEmpty (reverse $1) }

Ctor : Prefix TArgs { ($1, $2) }

{
mkIdn tok = Id (tokenLexeme tok) (range tok)

mkStr tok = Literal (Str $ transformStr $ tokenLexeme tok) (range tok)

mkTuple (x :| (y : zs)) r = Tuple (NonEmpty2 x y zs) r

mkTupleT (x :| (y : zs)) r = TupleT (NonEmpty2 x y zs) r

mkDataDefn tbder tparams ctors =
  let tctor = mkTypeCtor tbder tparams
      kind = mkFunK tparams (KLit $ range tbder)
      retType = foldl TApp (TVar tbder) (map TVar tparams)
      ctorBinds = NEL.map
                (\(ct, targs) -> Bind ct (mkForall tparams $ foldr FunT retType targs) (mkDataCtor ct targs))
                ctors
   in DataDefn (Bind tbder kind tctor) ctorBinds

mkForall tparams type' = case tparams of
  [] -> type'
  (tp : tps) -> Forall (kindedVars (tp :| tps)) type' (range tp <> range type')

mkTypeCtor tbder tparams =
  let tdata = TData tbder (map TVar tparams) (range tbder)
   in case tparams of
        [] -> tdata
        (tparam : tparams') -> mkTFun (tparam :| tparams') tdata

mkDataCtor bder [] = Data bder [] InvalidRange
mkDataCtor bder targs =
  let params = map (\ix -> Id (T.cons '_' $ T.pack $ show ix) InvalidRange) [0 .. (length targs - 1)]
   in Fun (asNonEmpty params) (Data bder (map Var params) InvalidRange) InvalidRange

mkTypeDefn tbder tparams type' =
  let type'' = mkTFun tparams type'
      kind = mkFunK tparams (KLit $ range tbder)
   in TypeDefn (Bind tbder kind type'')

mkTFun tparams type' = foldr TFun type' tparams

mkFunK tparams kind = foldr FunK kind (fmap (KLit . range) tparams)

transformStr = T.tail . T.init

kindedVars = NEL.map (\v -> (v, KLit $ range v))

asNonEmpty (x : xs) = x :| xs

chainToExpr (Operand' expr) = expr
chainToExpr chain = Chain (fromLRChain chain)

parseError tokens = error . show . head $ tokens
}
