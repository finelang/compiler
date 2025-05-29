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
  Lit (Bool, Int, Str, Unit),
  LitT (UnitT),
  Module (Module),
  Op (And, Eq),
  Pattern (..),
  Phase (Ready, Typed),
  Range (NoRange),
  Type (..),
  idText,
  typeof,
 )
import Fine.Syntax.Name (lengthProp, matchedVar, tagProp)
import Fine.Syntax.Utils (patternBoundVars)
import Fine.Typer.J (boolType, litType)

readyType :: Type Typed -> Type Ready
readyType (LiteralT _ litT) = LiteralT () litT
readyType (VoidT _) = VoidT ()
readyType (TupleT _ fst' snd' rest) = TupleT () (readyType fst') (readyType snd') (map readyType rest)
readyType (ListT _ type') = ListT () (readyType type')
readyType (RecordT _ propTypes) = RecordT () $ (map . fmap) readyType propTypes
readyType (FunT _ argTypes retType) = FunT () (NonEmpty.map readyType argTypes) (readyType retType)
readyType (Forall _ univars type') = Forall () (NonEmpty.map fst univars) (readyType type')
readyType (DataT _ tag types) = DataT () tag (map readyType types)
readyType (TVar _ var) = TVar () var
readyType (TApp _ tfun targs) = TApp () (readyType tfun) (NonEmpty.map readyType targs)
readyType (TFun _ tparams tbody) = TFun () tparams (readyType tbody)

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
extractPaths (LiteralP _ lit) = [End $ EqualsTo (Literal (readyType $ litType NoRange lit) lit)]
extractPaths (DataP _ tag patts) =
  let lit = Str (idText tag)
      fromTag =
        Continue
          (PropTo $ tagProp)
          (End $ EqualsTo $ Literal (readyType $ litType NoRange lit) $ lit)
   in fromTag : indexedPaths patts
extractPaths (RecordP _ props) =
  foldMap
    (\(prop, patt) -> map (Continue $ PropTo prop) (extractPaths patt))
    props
extractPaths (TupleP _ fst' snd' rest) = indexedPaths (fst' : snd' : rest)
extractPaths (ListP _ patts) =
  let lit = Int (length patts)
      lenCheck =
        Continue
          (PropTo $ lengthProp)
          (End $ EqualsTo $ Literal (readyType $ litType NoRange lit) $ lit)
   in lenCheck : indexedPaths patts
extractPaths (Capture var) = [End $ Is var]
extractPaths (Discard _) = []

type Stmt = Block Ready -> Block Ready

type Cond = Expr Ready

readyBoolType :: Type Ready
readyBoolType = readyType (boolType NoRange)

readyUnitType :: Type Ready
readyUnitType = readyType (litType NoRange Unit)

applyPath :: Bool -> Expr Ready -> PatternPath -> Either Cond Stmt
applyPath mut matched path =
  let (pieces, end) = splitPath path []
      matched' = foldl' applyPiece matched pieces
   in applyEnd matched' end
 where
  splitPath (End ct) pieces = (reverse pieces, ct)
  splitPath (Continue piece pp) pieces = splitPath pp (piece : pieces)

  applyPiece expr (PropTo prop) = Access (unsafePropType expr prop) expr prop
  applyPiece expr (IndexTo ix) = Index (unsafeTupleInnerType expr ix) expr ix

  applyEnd expr (EqualsTo expr') = Left (Bin readyBoolType () Eq expr expr')
  applyEnd expr (Is var) = Right $ (if mut then Mut else Let False) var expr

  unsafePropType _ _ = LiteralT () UnitT -- TODO when type checking
  unsafeTupleInnerType _ _ = LiteralT () UnitT -- TODO when type checking

matchedVar' :: Id
matchedVar' = matchedVar NoRange

matchedExpr :: Type Ready -> Expr Ready
matchedExpr t = Var t matchedVar'

transformMatches :: Expr Ready -> (NonEmpty (Pattern, Expr Ready)) -> Block Ready
transformMatches matched matches =
  let ifStmts = (flip NonEmpty.map) matches $ \(pattern, cont) ->
        let paths = extractPaths pattern
            (conds, lets') = partitionEithers $ map (applyPath False matched) paths
            ifBlock = mergeToExpr lets' cont
            cond = case conds of
              [] -> Literal readyBoolType (Bool True)
              (c : cs) ->
                let (cs', c') = unsnoc (c :| cs)
                 in foldr (Bin readyBoolType () And) c' cs'
         in If () cond ifBlock
   in foldr ($) Void ifStmts
 where
  mergeToExpr stmts (Block _ block) = foldr ($) block stmts
  mergeToExpr stmts expr = foldr ($) (Return expr) stmts

readyBlock :: Block Typed -> Block Ready
readyBlock (Return expr) = Return $ readyExpr expr
readyBlock Void = Void
readyBlock (Do action block) = Do (readyExpr action) (readyBlock block)
readyBlock (Mut var expr block) =
  Mut var (readyExpr expr) (readyBlock block)
readyBlock (Debug expr block) = Debug (readyExpr expr) (readyBlock block)
readyBlock (Let isMut binder value block) =
  Let isMut binder (readyExpr value) (readyBlock block)
readyBlock (Loop cond actions block) =
  Loop (readyExpr cond) (readyBlock actions) (readyBlock block)
readyBlock (LetPatt _ patt expr block) =
  let expr' = readyExpr expr
      block' = readyBlock block
   in case patternBoundVars patt of
        [] -> Do expr' block'
        vars ->
          let lets = map (\var -> Let True var $ Literal readyUnitType Unit) vars
              paths = extractPaths patt
              stmts = rights $ map (applyPath True $ matchedExpr $ typeof expr') paths
              setterBlock = foldr ($) Void (Let False matchedVar' expr' : stmts)
           in foldr ($) (Do (Block readyUnitType setterBlock) block') lets

readyExpr :: Expr Typed -> Expr Ready
readyExpr (Literal (_, t) lit) = Literal (readyType t) lit
readyExpr (Data (_, t) tag exprs) = Data (readyType t) tag (map readyExpr exprs)
readyExpr (Record (_, t) props) = Record (readyType t) $ (map . fmap) readyExpr props
readyExpr (Tuple (_, t) fst' snd' rest) =
  Tuple (readyType t) (readyExpr fst') (readyExpr snd') (map readyExpr rest)
readyExpr (List (_, t) exprs) = List (readyType t) (map readyExpr exprs)
readyExpr (Var (_, t) var) = Var (readyType t) var
readyExpr (Bin (_, t) _ op left right) = Bin (readyType t) () op (readyExpr left) (readyExpr right)
readyExpr (App (_, t) f args) = App (readyType t) (readyExpr f) (NonEmpty.map readyExpr args)
readyExpr (GenApp (_, t) f targs) = GenApp (readyType t) (readyExpr f) (NonEmpty.map readyType targs)
readyExpr (Access (_, t) expr prop) = Access (readyType t) (readyExpr expr) prop
readyExpr (Index (_, t) expr ix) = Index (readyType t) (readyExpr expr) ix
readyExpr (Cond (_, t) cond yes no) =
  Cond (readyType t) (readyExpr cond) (readyExpr yes) (readyExpr no)
readyExpr (PatternMatching (_, t) _ expr matches) =
  let expr' = readyExpr expr
      matches' = (NonEmpty.map . fmap) readyExpr matches
      block = Let False matchedVar' expr' $ transformMatches (matchedExpr $ typeof expr') matches'
   in Block (readyType t) block
readyExpr (Fun (_, t) params body) = Fun (readyType t) params (readyExpr body)
readyExpr (GenFun (_, t) _ tparams body) = GenFun (readyType t) () tparams (readyExpr body)
readyExpr (Block (_, t) block) = Block (readyType t) (readyBlock block)

readyBind :: Bind OfExpr Typed -> Bind OfExpr Ready
readyBind (ExprBind binder type' expr) = ExprBind binder (readyType type') (readyExpr expr)
readyBind (ForeignBind binder type' code) = ForeignBind binder (readyType type') code

readyModule :: Module Typed -> Module Ready
readyModule (Module values _ entry) =
  Module (map readyBind values) () (fmap readyExpr entry)
