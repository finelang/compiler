module Fine.Codegen.Ready (getModuleReady) where

import Data.Either (partitionEithers, rights)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Extra (unsnoc)
import Fine.Syntax (
  Bind (ExprBind, ForeignBind),
  BindType (OfExpr),
  Block (..),
  Expr (..),
  Id (Id, idText),
  Lit (Bool, Str),
  Module (Module),
  Pattern (..),
  Phase (Ready, Transformed),
  Range (NoRange),
 )

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
extractPaths (LiteralP _ lit) = [End $ EqualsTo (Literal () lit)]
extractPaths (DataP _ tag patts) =
  let fromTag = Continue (PropTo $ Id NoRange "$tag") (End $ EqualsTo $ Literal () $ Str $ idText tag)
   in fromTag : indexedPaths patts
extractPaths (RecordP _ props) =
  foldMap
    (\(prop, patt) -> map (Continue $ PropTo prop) (extractPaths patt))
    props
extractPaths (TupleP _ patts) = indexedPaths (NonEmpty.toList patts)
extractPaths (Capture var) = [End $ Is var]
extractPaths (Discard _) = []

type Stmt = Block Ready -> Block Ready

type Cond = Expr Ready

applyPath :: Expr Ready -> PatternPath -> Either Cond Stmt
applyPath matched path =
  let (pieces, end) = splitPath path []
      matched' = foldl' applyPiece matched pieces
   in applyEnd matched' end
 where
  splitPath (End ct) pieces = (reverse pieces, ct)
  splitPath (Continue piece pp) pieces = splitPath pp (piece : pieces)

  applyPiece expr (PropTo prop) = Access () expr prop
  applyPiece expr (IndexTo ix) = Index () expr ix

  applyEnd expr (EqualsTo expr') = Left (Equals () expr expr')
  applyEnd expr (Is var) = Right (Let False var expr)

type Typed = Transformed -- TODO: remove this line (and import 'Typed' phase) after typer impl

transformMatches :: Expr Ready -> (NonEmpty (Pattern, Expr Ready)) -> Block Ready
transformMatches matched matches =
  let (matches', (lastPattern, lastCont)) = unsnoc matches
      ifStmts = (flip map) matches' $ \(pattern, cont) ->
        let paths = extractPaths pattern
            (conds, lets') = partitionEithers $ map (applyPath matched) paths
            ifBlock = mergeToExpr lets' cont
            cond = case conds of
              [] -> Literal () (Bool True)
              (c : cs) -> Conj () (c :| cs)
         in If () cond ifBlock
      -- last match turns into a simple block because PM should be exhaustive
      lastLets = rights $ map (applyPath matched) $ extractPaths lastPattern
      lastBlock = mergeToExpr lastLets lastCont
   in foldr ($) lastBlock ifStmts
 where
  mergeToExpr stmts (Block _ block) = foldr ($) block stmts
  mergeToExpr stmts expr = foldr ($) (Return expr) stmts

getBlockReady :: Block Typed -> Block Ready
getBlockReady (Return expr) = Return $ getExprReady expr
getBlockReady Void = Void
getBlockReady (Do action block) = Do (getExprReady action) (getBlockReady block)
getBlockReady (Mut var expr block) =
  Mut var (getExprReady expr) (getBlockReady block)
getBlockReady (Debug expr block) = Debug (getExprReady expr) (getBlockReady block)
getBlockReady (Let isMut binder value block) =
  Let isMut binder (getExprReady value) (getBlockReady block)
getBlockReady (Loop cond actions block) =
  Loop (getExprReady cond) (getBlockReady actions) (getBlockReady block)

getExprReady :: Expr Typed -> Expr Ready
getExprReady (Literal _ lit) = Literal () lit
getExprReady (Data _ tag exprs) = Data () tag (map getExprReady exprs)
getExprReady (Record _ props) = Record () $ (fmap . fmap) getExprReady props
getExprReady (Tuple _ exprs) = Tuple () (NonEmpty.map getExprReady exprs)
getExprReady (Var _ var) = Var () var
getExprReady (App _ f args) = App () (getExprReady f) (NonEmpty.map getExprReady args)
getExprReady (GenApp _ f _) = getExprReady f
getExprReady (Access _ expr prop) = Access () (getExprReady expr) prop
getExprReady (Index _ expr ix) = Index () (getExprReady expr) ix
getExprReady (Cond _ cond yes no) =
  Cond () (getExprReady cond) (getExprReady yes) (getExprReady no)
getExprReady (PatternMatching _ expr matches) =
  let expr' = getExprReady expr
      matches' = (NonEmpty.map . fmap) getExprReady matches
      block = case expr' of
        Var _ _ -> transformMatches expr' matches'
        _ ->
          let matchedId = Id NoRange "$$matched"
              matched = Var () matchedId
           in Let False matchedId expr' $ transformMatches matched matches'
   in Block () block
getExprReady (Fun _ params body) = Fun () params (getExprReady body)
getExprReady (GenFun _ _ body) = getExprReady body
getExprReady (Block _ block) = Block () (getBlockReady block)

getBindReady :: Bind OfExpr Typed -> Bind OfExpr Ready
getBindReady (ExprBind binder _ expr) = ExprBind binder () (getExprReady expr)
getBindReady (ForeignBind binder _ code) = ForeignBind binder () code

getModuleReady :: Module Typed -> Module Ready
getModuleReady (Module values _ _ entry) =
  Module (map getBindReady values) () () (fmap getExprReady entry)
