module Fine.Rename (runRenamer) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.RS (RS, ask, asks, get, local, put, runRS, withReader)
import Data.Char (isSymbol, ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Fine.Syntax (
  Bind (Bind),
  Block (..),
  Expr (..),
  Id (Id),
  Module (Module, moduleEntry, moduleValues),
  Pass (Typed),
  Pattern (..),
  TypeOfBind (OfValue),
  binder,
 )
import Fine.Syntax.Utils (boundVars)

type Count = Integer

type Substts = Map Id Id

resetCount :: RS r Count ()
resetCount = put 0

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

renameBlock :: Block Typed -> RS Substts Count (Block Typed)
renameBlock (Return expr) = Return <$> renameExpr expr
renameBlock (Do action block) = Do <$> renameExpr action <*> renameBlock block
renameBlock (Let isMut bound value block) = do
  value' <- renameExpr value
  bound' <- rename bound
  block' <- withSubstt bound bound' (renameBlock block)
  return (Let isMut bound' value' block')
renameBlock (Loop cond actions block) =
  Loop <$> renameExpr cond <*> renameBlock actions <*> renameBlock block
renameBlock block@(Void _) = return block

renamePatt :: Pattern -> RS Substts Count Pattern
renamePatt patt@(LiteralP _ _) = return patt
renamePatt (DataP r tag patts) = DataP r tag <$> mapM renamePatt patts
renamePatt (RecordP r props) = RecordP r <$> (mapM . mapM) renamePatt props
renamePatt (TupleP r patts) = TupleP r <$> mapM renamePatt patts
renamePatt (Capture r name) = Capture r <$> substt name
renamePatt patt@(Discard _) = return patt

renameMatch :: (Pattern, Expr Typed) -> RS Substts Count (Pattern, Expr Typed)
renameMatch (patt, expr) = do
  substts <- mapM (\var -> (,) var <$> rename var) (boundVars patt)
  patt' <- withSubstts substts (renamePatt patt)
  expr' <- withSubstts substts (renameExpr expr)
  return (patt', expr')

renameExpr :: Expr Typed -> RS Substts Count (Expr Typed)
renameExpr expr@(Literal _ _) = return expr
renameExpr (Data ext tag exprs) = Data ext tag <$> mapM renameExpr exprs
renameExpr (Record ext props) = Record ext <$> (mapM . mapM) renameExpr props
renameExpr (Tuple ext exprs) = Tuple ext <$> mapM renameExpr exprs
renameExpr (Var ext name) = Var ext <$> substt name
renameExpr (Mut ext name expr) = Mut ext <$> substt name <*> renameExpr expr
renameExpr (App ext f arg) = App ext <$> renameExpr f <*> renameExpr arg
renameExpr (Access ext expr prop) = Access ext <$> renameExpr expr <*> return prop
renameExpr (Index ext expr ix) = Index ext <$> renameExpr expr <*> return ix
renameExpr (Cond ext cond yes no) =
  Cond ext <$> renameExpr cond <*> renameExpr yes <*> renameExpr no
renameExpr (Fun ext param body) = do
  param' <- rename param
  body' <- withSubstt param param' (renameExpr body)
  return (Fun ext param' body')
renameExpr (Block ext block) = Block ext <$> renameBlock block
renameExpr (PatternMatch ext expr matches) =
  PatternMatch ext <$> renameExpr expr <*> mapM renameMatch matches
renameExpr (Debug ext expr) = Debug ext <$> renameExpr expr
renameExpr expr@(External _ _ _) = return expr

renameBind :: Bind OfValue Typed -> RS Substts Count (Bind OfValue Typed)
renameBind (Bind binder' type' expr) = do
  binder'' <- substt binder'
  resetCount
  expr' <- renameExpr expr
  return (Bind binder'' type' expr')

handleOperator :: Id -> Maybe Id
handleOperator (Id r name) = do
  let chars = Text.unpack name
  if all isSymbol chars
    then
      let codes = map (Text.pack . show . ord) chars
       in Just $ Id r $ Text.append "op$" $ Text.intercalate "_" codes
    else Nothing

type InvalidNames = Set Text

handleInvalid :: InvalidNames -> Id -> Maybe Id
handleInvalid invalidNames (Id r name) =
  if Set.member name invalidNames
    then Just $ Id r $ Text.append "var$" name
    else Nothing

tryCollectSubstt :: Id -> RS InvalidNames s (Maybe (Id, Id))
tryCollectSubstt idn = do
  invalidNames <- ask
  return $ (,) idn <$> (handleOperator idn <|> handleInvalid invalidNames idn)

renameModule :: Module Typed -> RS (Substts, InvalidNames) Count (Module Typed)
renameModule mdule@(Module values _ _ entry) = do
  substts <-
    (Map.fromList . catMaybes)
      <$> withReader snd (mapM (tryCollectSubstt . binder) values)
  let reader = Map.union substts . fst
  values' <- withReader reader (mapM renameBind values)
  resetCount
  entry' <- withReader reader (mapM renameExpr entry)
  return (mdule{moduleValues = values', moduleEntry = entry'})

runRenamer :: InvalidNames -> Module Typed -> Module Typed
runRenamer invalidNames mdule = runRS (renameModule mdule) (Map.empty, invalidNames) 0
