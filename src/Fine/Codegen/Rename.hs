module Fine.Codegen.Rename (runRenamer) where

import Control.Monad.Trans.Reader (Reader, asks, runReader)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Data.Text (Text)
import Data.Text qualified as Text
import Fine.Syntax (
  Bind (..),
  BindType (OfExpr),
  Block (..),
  Expr (..),
  Id (Id),
  Module (Module, moduleEntry, moduleExprs),
  Pattern (..),
  Phase (Parsed),
  Range (NoRange),
 )
import Fine.Syntax.Utils (isCtor)

type Substts = Map Id Id

substt :: Id -> Reader Substts Id
substt name = asks (Map.findWithDefault name name)

renameBlock :: Block Parsed -> Reader Substts (Block Parsed)
renameBlock (Return expr) = Return <$> renameExpr expr
renameBlock block@Void = return block
renameBlock (Do action block) = Do <$> renameExpr action <*> renameBlock block
renameBlock (Mut var expr block) =
  Mut <$> substt var <*> renameExpr expr <*> renameBlock block
renameBlock (Debug expr block) = Debug <$> renameExpr expr <*> renameBlock block
renameBlock (Let isMut bound value block) =
  Let isMut <$> substt bound <*> renameExpr value <*> renameBlock block
renameBlock (Loop cond actions block) =
  Loop <$> renameExpr cond <*> renameBlock actions <*> renameBlock block
renameBlock (LetPatt _ patt expr block) =
  LetPatt () <$> renamePatt patt <*> renameExpr expr <*> renameBlock block

renamePatt :: Pattern -> Reader Substts Pattern
renamePatt patt@(LiteralP _ _) = return patt
renamePatt (DataP r tag patts) = DataP r tag <$> mapM renamePatt patts
renamePatt (RecordP r props) = RecordP r <$> (mapM . mapM) renamePatt props
renamePatt (TupleP r patts) = TupleP r <$> mapM renamePatt patts
renamePatt (ListP r patts) = ListP r <$> mapM renamePatt patts
renamePatt (Capture name) = Capture <$> substt name
renamePatt patt@(Discard _) = return patt

renameMatch :: (Pattern, Expr Parsed) -> Reader Substts (Pattern, Expr Parsed)
renameMatch (patt, expr) = (,) <$> renamePatt patt <*> renameExpr expr

renameExpr :: Expr Parsed -> Reader Substts (Expr Parsed)
renameExpr expr@(Literal _ _) = return expr
renameExpr (Data ext tag exprs) = Data ext tag <$> mapM renameExpr exprs
renameExpr (Record ext props) = Record ext <$> (mapM . mapM) renameExpr props
renameExpr (Tuple ext exprs) = Tuple ext <$> mapM renameExpr exprs
renameExpr (List ext exprs) = List ext <$> mapM renameExpr exprs
renameExpr (Var ext name) = Var ext <$> substt name
renameExpr (Bin ext op left right) = Bin ext op <$> renameExpr left <*> renameExpr right
renameExpr (App ext f args) = App ext <$> renameExpr f <*> mapM renameExpr args
renameExpr (GenApp ext f typeArgs) = GenApp ext <$> renameExpr f <*> return typeArgs
renameExpr (Access ext expr prop) = Access ext <$> renameExpr expr <*> return prop
renameExpr (Index ext expr ix) = Index ext <$> renameExpr expr <*> return ix
renameExpr (Cond ext cond yes no) =
  Cond ext <$> renameExpr cond <*> renameExpr yes <*> renameExpr no
renameExpr (Fun ext params body) =
  Fun ext <$> mapM substt params <*> renameExpr body
renameExpr (GenFun ext typeParams body) = GenFun ext typeParams <$> renameExpr body
renameExpr (Block ext block) =
  Block ext <$> renameBlock block
renameExpr (PatternMatching ext expr matches) =
  PatternMatching ext <$> renameExpr expr <*> mapM renameMatch matches

renameBind :: Bind OfExpr Parsed -> Reader Substts (Bind OfExpr Parsed)
renameBind (ExprBind binder' type' expr) = do
  binder'' <- substt binder'
  expr' <- (if isCtor expr then return else renameExpr) expr
  return (ExprBind binder'' type' expr')
renameBind (ForeignBind binder' type' code) = do
  binder'' <- substt binder'
  return (ForeignBind binder'' type' code)

renameModule :: Module Parsed -> Reader Substts (Module Parsed)
renameModule mdule@(Module exprs _ entry) = do
  exprs' <- mapM renameBind exprs
  entry' <- mapM renameExpr entry
  return (mdule{moduleExprs = exprs', moduleEntry = entry'})

runRenamer :: [Text] -> Module Parsed -> Module Parsed
runRenamer invalidNames mdule =
  let initialSubtss = map (\text -> (Id NoRange text, Id NoRange $ Text.cons '$' text)) invalidNames
   in runReader (renameModule mdule) (Map.fromList initialSubtss)
