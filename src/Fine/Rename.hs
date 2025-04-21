module Fine.Rename (runRenamer) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.RS (RS, ask, asks, get, local, put, runRS, withReader)
import Data.Char (ord)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Fine.Syntax (
  Bind (..),
  BindType (OfExpr),
  Block (..),
  Expr (..),
  Id (Id, Op),
  Module (Module, moduleEntry, moduleExprs),
  Pattern (..),
  Phase (Transformed),
  binder,
 )
import Fine.Syntax.Utils (boundVars, isCtor)

type Count = Integer

type Substts = Map Id Id

resetCount :: RS r Count ()
resetCount = put 0

resumeCount :: Count -> RS r Count ()
resumeCount = put

withSubstt :: Id -> Id -> RS Substts s a -> RS Substts s a
withSubstt old new = local (Map.insert old new)

withSubstts :: [(Id, Id)] -> RS Substts s a -> RS Substts s a
withSubstts substts = local (Map.union $ Map.fromList substts)

substt :: Id -> RS Substts s Id
substt name = asks (Map.findWithDefault name name)

rename :: Id -> RS r Count Id
rename (Id r name) = do
  count <- get
  put (count + 1)
  return (Id r [i|#{name}$#{count}|])
rename op = return op -- operators are not renamed

renameBlock :: Block Transformed -> RS Substts Count (Block Transformed)
renameBlock (Return expr) = Return <$> renameExpr expr
renameBlock block@Void = return block
renameBlock (Do action block) = Do <$> renameExpr action <*> renameBlock block
renameBlock (Mut var expr block) =
  Mut <$> substt var <*> renameExpr expr <*> renameBlock block
renameBlock (Debug expr block) = Debug <$> renameExpr expr <*> renameBlock block
renameBlock (Let isMut bound value block) = do
  value' <- renameExpr value
  bound' <- rename bound
  block' <- withSubstt bound bound' (renameBlock block)
  return (Let isMut bound' value' block')
renameBlock (Loop cond actions block) =
  Loop <$> renameExpr cond <*> renameBlock actions <*> renameBlock block

renamePatt :: Pattern -> RS Substts Count Pattern
renamePatt patt@(LiteralP _ _) = return patt
renamePatt (DataP r tag patts) = DataP r tag <$> mapM renamePatt patts
renamePatt (RecordP r props) = RecordP r <$> (mapM . mapM) renamePatt props
renamePatt (TupleP r patts) = TupleP r <$> mapM renamePatt patts
renamePatt (Capture name) = Capture <$> substt name
renamePatt patt@(Discard _) = return patt

renameMatch :: (Pattern, Expr Transformed) -> RS Substts Count (Pattern, Expr Transformed)
renameMatch (patt, expr) = do
  count <- get
  resetCount
  substts <- mapM (\var -> (,) var <$> rename var) (boundVars patt)
  patt' <- withSubstts substts (renamePatt patt)
  expr' <- withSubstts substts $ case expr of
    Block ext block -> Block ext <$> renameBlock block
    _ -> renameExpr expr
  resumeCount count
  return (patt', expr')

renameExpr :: Expr Transformed -> RS Substts Count (Expr Transformed)
renameExpr expr@(Literal _ _) = return expr
renameExpr (Data ext tag exprs) = Data ext tag <$> mapM renameExpr exprs
renameExpr (Record ext props) = Record ext <$> (mapM . mapM) renameExpr props
renameExpr (Tuple ext exprs) = Tuple ext <$> mapM renameExpr exprs
renameExpr (Var ext name) = Var ext <$> substt name
renameExpr (App ext f args) = App ext <$> renameExpr f <*> mapM renameExpr args
renameExpr (GenApp ext f typeArgs) = GenApp ext <$> renameExpr f <*> return typeArgs
renameExpr (Access ext expr prop) = Access ext <$> renameExpr expr <*> return prop
renameExpr (Index ext expr ix) = Index ext <$> renameExpr expr <*> return ix
renameExpr (Cond ext cond yes no) =
  Cond ext <$> renameExpr cond <*> renameExpr yes <*> renameExpr no
renameExpr (Fun ext params body) = do
  count <- get
  resetCount
  params' <- mapM rename params
  let substts = NonEmpty.toList (NonEmpty.zip params params')
  body' <- withSubstts substts $ case body of
    Block ext' block -> Block ext' <$> renameBlock block
    _ -> renameExpr body
  resumeCount count
  return (Fun ext params' body')
renameExpr (GenFun ext typeParams body) = GenFun ext typeParams <$> renameExpr body
renameExpr (Block ext block) = do
  count <- get
  resetCount
  block' <- renameBlock block
  resumeCount count
  return (Block ext block')
renameExpr (PatternMatching ext expr matches) =
  PatternMatching ext <$> renameExpr expr <*> mapM renameMatch matches

renameBind :: Bind OfExpr Transformed -> RS Substts Count (Bind OfExpr Transformed)
renameBind (ExprBind binder' type' expr) = do
  binder'' <- substt binder'
  expr' <- (if isCtor expr then return else renameExpr) expr
  return (ExprBind binder'' type' expr')
renameBind (ForeignBind binder' type' code) = do
  binder'' <- substt binder'
  return (ForeignBind binder'' type' code)

handleOperator :: Id -> Maybe Id
handleOperator (Id _ _) = Nothing
handleOperator (Op r name) =
  let codes = map (Text.pack . show . ord) (Text.unpack name)
   in Just $ Id r $ Text.append "op$" $ Text.intercalate "_" codes

type InvalidNames = Set Text

handleInvalid :: InvalidNames -> Id -> Maybe Id
handleInvalid _ (Op _ _) = Nothing
handleInvalid invalidNames (Id r name) =
  if Set.member name invalidNames
    then Just $ Id r $ Text.append "var$" name
    else Nothing

tryCollectSubstt :: Id -> RS InvalidNames s (Maybe (Id, Id))
tryCollectSubstt idn = do
  invalidNames <- ask
  return $ (,) idn <$> (handleOperator idn <|> handleInvalid invalidNames idn)

renameModule :: Module Transformed -> RS (Substts, InvalidNames) Count (Module Transformed)
renameModule mdule@(Module exprs _ _ entry) = do
  substts <-
    (Map.fromList . catMaybes)
      <$> withReader snd (mapM (tryCollectSubstt . binder) exprs)
  let reader = Map.union substts . fst
  exprs' <- withReader reader (mapM renameBind exprs)
  entry' <- withReader reader (mapM renameExpr entry)
  return (mdule{moduleExprs = exprs', moduleEntry = entry'})

runRenamer :: InvalidNames -> Module Transformed -> Module Transformed
runRenamer invalidNames mdule = runRS (renameModule mdule) (Map.empty, invalidNames) 0
