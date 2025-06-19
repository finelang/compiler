module Fine.Codegen.Ready (readyModule) where

import Data.Either (partitionEithers, rights)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra (unsnoc)
import Fine.Syntax (
  Bind (ExprBind, ForeignBind),
  BindType (OfExpr),
  Block (..),
  Expr (..),
  Id,
  Lit (Bool, Str, Unit),
  Module (Module),
  Op (And, Eq),
  Pattern (..),
  Phase (Ready, Typed),
  Range (NoRange),
  nameText,
 )
import Fine.Syntax.Name (matchedVar, tagProp)
import Fine.Syntax.Utils (patternBoundVars, unqualified)

data PathEnd
  = EqualsTo (Expr Ready)
  | Is Id

data PathPiece
  = PropTo Id
  | IndexTo Int

data PatternPath
  = End PathEnd
  | Continue PathPiece PatternPath

indexedPaths :: [Pattern] -> [PatternPath]
indexedPaths patts =
  concat $
    zipWith
      (\patt ix -> map (Continue $ IndexTo ix) (extractPaths patt))
      patts
      [(0 :: Int) ..]

extractPaths :: Pattern -> [PatternPath]
extractPaths (LiteralP _ lit) = [End $ EqualsTo (Literal () NoRange lit)]
extractPaths (DataP _ tag patts) =
  let lit = Str (nameText tag)
      fromTag =
        Continue
          (PropTo $ tagProp)
          (End $ EqualsTo $ Literal () NoRange lit)
   in fromTag : indexedPaths patts
extractPaths (RecordP _ props) =
  foldMap
    (\(prop, patt) -> map (Continue $ PropTo prop) (extractPaths patt))
    props
extractPaths (TupleP _ fst' snd' rest) = indexedPaths (fst' : snd' : rest)
extractPaths (Capture var) = [End $ Is var]
extractPaths (Discard _) = []

type Stmt = Block Ready -> Block Ready

type Cond = Expr Ready

applyPath :: Bool -> Expr Ready -> PatternPath -> Either Cond Stmt
applyPath mut matched path =
  let (pieces, end) = splitPath path []
      matched' = foldl' applyPiece matched pieces
   in applyEnd matched' end
 where
  splitPath (End ct) pieces = (reverse pieces, ct)
  splitPath (Continue piece pp) pieces = splitPath pp (piece : pieces)

  applyPiece expr (PropTo prop) = Access () expr prop
  applyPiece expr (IndexTo ix) = Index () NoRange expr ix

  applyEnd expr (EqualsTo expr') = Left (Bin () Eq expr expr')
  applyEnd expr (Is var) = Right $ (if mut then Mut else LetImmut ()) var expr

matchedVar' :: Id
matchedVar' = matchedVar NoRange

matchedExpr :: Expr Ready
matchedExpr = Var () (unqualified matchedVar')

transformMatches :: Expr Ready -> (NonEmpty (Pattern, Expr Ready)) -> Block Ready
transformMatches matched matches =
  let ifStmts = (flip NonEmpty.map) matches $ \(pattern, cont) ->
        let paths = extractPaths pattern
            (conds, lets') = partitionEithers $ map (applyPath False matched) paths
            ifBlock = foldr ($) (Return cont) lets'
            cond = case conds of
              [] -> Literal () NoRange (Bool True)
              (c : cs) ->
                let (cs', c') = unsnoc (c :| cs)
                 in foldr (Bin () And) c' cs'
         in If () cond ifBlock
   in foldr ($) Void ifStmts

readyBlock :: Block Typed -> Block Ready
readyBlock (Return expr) = Return $ readyExpr expr
readyBlock Void = Void
readyBlock (Do action block) = Do (readyExpr action) (readyBlock block)
readyBlock (Mut var expr block) =
  Mut var (readyExpr expr) (readyBlock block)
readyBlock (LetMut binder value block) =
  LetMut binder (readyExpr value) (readyBlock block)
readyBlock (Let _ (Capture binder) value block) =
  LetImmut () binder (readyExpr value) (readyBlock block)
readyBlock (Let _ patt expr block) =
  let expr' = readyExpr expr
      block' = readyBlock block
   in case patternBoundVars patt of
        [] -> Do expr' block'
        vars ->
          let lets = map (\var -> LetMut var $ Literal () NoRange Unit) vars
              paths = extractPaths patt
              stmts = rights $ map (applyPath True matchedExpr) paths
              setterBlock = foldr ($) Void (LetImmut () matchedVar' expr' : stmts)
           in foldr ($) (Do (Block () NoRange setterBlock) block') lets
readyBlock (Debug r expr block) = Debug r (readyExpr expr) (readyBlock block)
readyBlock (Loop cond actions block) =
  Loop (readyExpr cond) (readyBlock actions) (readyBlock block)

readyExpr :: Expr Typed -> Expr Ready
readyExpr (Literal _ r lit) = Literal () r lit
readyExpr (Data _ tag exprs) = Data () tag (map readyExpr exprs)
readyExpr (Record _ r props) = Record () r $ (map . fmap) readyExpr props
readyExpr (Tuple _ r fst' snd' rest) =
  Tuple () r (readyExpr fst') (readyExpr snd') (map readyExpr rest)
readyExpr (Var _ var) = Var () var
readyExpr (Bin _ op left right) = Bin () op (readyExpr left) (readyExpr right)
readyExpr (App _ f arg) = App () (readyExpr f) (readyExpr arg)
readyExpr (GenApp _ _ fname _) = Var () fname
readyExpr (Access _ expr prop) = Access () (readyExpr expr) prop
readyExpr (Index _ r expr ix) = Index () r (readyExpr expr) ix
readyExpr (Cond _ r cond yes no) =
  Cond () r (readyExpr cond) (readyExpr yes) (readyExpr no)
readyExpr (PatternMatching _ r _ expr matches) =
  let expr' = readyExpr expr
      matches' = (NonEmpty.map . fmap) readyExpr matches
      block = LetImmut () matchedVar' expr' $ transformMatches matchedExpr matches'
   in Block () r block
readyExpr (Fun _ param body) = Fun () param (readyExpr body)
readyExpr (GenFun _ _ _ _ body) = readyExpr body
readyExpr (Block _ r block) = Block () r (readyBlock block)

readyBind :: Bind OfExpr Typed -> Bind OfExpr Ready
readyBind (ExprBind binder _ expr) = ExprBind binder () (readyExpr expr)
readyBind (ForeignBind binder _ code) = ForeignBind binder () code

readyModule :: Module Typed -> Module Ready
readyModule (Module values _ entry typeCtors) =
  Module (map readyBind values) () (fmap readyExpr entry) typeCtors
