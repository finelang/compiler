module Fine.Codegen (runCodegen) where

import Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Fine.Codegen.Lit (genLitCode)
import Fine.Codegen.Pattern (extractCondsAndBinds)
import Fine.Codegen.TailRec (tryOptimize)
import Fine.Syntax (
  Bind (Bind),
  Block (..),
  Expr (..),
  Id (Id),
  Module (Module),
  Pass (Typed),
  Pattern (..),
  Range (NoRange),
  TypeOfBind (OfValue),
 )

type Indentation = Text

withIndentation :: Indentation -> Reader Indentation a -> Reader Indentation a
withIndentation = local . const

increaseIndentation :: Reader Indentation Text
increaseIndentation = do
  indent <- ask
  return (indent <> "  ")

type Expr' = Expr Typed

type Block' = Block Typed

genPropCode :: (Id, Expr') -> Reader Indentation Text
genPropCode (prop, value) = do
  value' <- genExprCode value
  return [i|#{prop}: #{value'}|]

genPropsCode :: [(Id, Expr')] -> Reader Indentation Text
genPropsCode props = Text.intercalate ", " <$> mapM genPropCode props

genIndexedPropsCode :: [Expr'] -> Reader Indentation Text
genIndexedPropsCode values =
  let indexes = map (Id NoRange . Text.pack . show) [(0 :: Integer) ..]
   in genPropsCode (zip indexes values)

genBlockCode :: Block' -> Reader Indentation Text
genBlockCode (Return expr) = do
  expr' <- genExprCode expr
  indent <- ask
  return [i|#{indent}return #{expr'};\n|]
genBlockCode (Do stmt block) = do
  stmt' <- genExprCode stmt
  block' <- genBlockCode block
  indent <- ask
  return [i|#{indent}#{stmt'};\n#{block'}|]
genBlockCode (Let isMut bound expr block) = do
  let keyword = if isMut then "let" else "const" :: Text
  expr' <- genExprCode expr
  block' <- genBlockCode block
  indent <- ask
  return [i|#{indent}#{keyword} #{bound} = #{expr'};\n#{block'}|]
genBlockCode (Void _) = return ""
genBlockCode (Loop cond actions block) = do
  cond' <- genExprCode cond
  actions' <- genStmtsCode actions
  block' <- genBlockCode block
  indent <- ask
  return [i|#{indent}while (#{cond'}) {\n#{actions'}#{indent}}\n#{block'}|]

genStmtsCode :: Block' -> Reader Indentation Text
genStmtsCode block = do
  indent <- increaseIndentation
  withIndentation indent (genBlockCode block)

genMatchCode :: Text -> (Pattern, Expr') -> Reader Indentation Text
genMatchCode name (patt, expr) = do
  oldIndent <- ask
  indent <- increaseIndentation
  let (conds, binds) = extractCondsAndBinds name patt
  let cond = if null conds then "true" else Text.intercalate " && " conds
  let binds' = Text.concat $ map (\stmt -> [i|#{indent}#{stmt};\n|]) binds
  case expr of
    Block _ block -> do
      stmts <- genStmtsCode block
      return [i|if (#{cond}) {\n#{binds'}#{stmts}#{oldIndent}}|]
    _ -> do
      expr' <- withIndentation indent (genExprCode expr)
      return [i|if (#{cond}) {\n#{binds'}#{indent}return #{expr'};\n#{oldIndent}}|]

genFunCode :: Id -> Expr' -> Reader Indentation Text
genFunCode param body = case body of
  Block _ block -> do
    body' <- genStmtsCode block
    indent <- ask
    return [i|(#{param} => {\n#{body'}#{indent}})|]
  _ -> do
    body' <- genExprCode body
    return [i|(#{param} => #{body'})|]

genExprCode :: Expr' -> Reader Indentation Text
genExprCode (Literal _ lit) = return (genLitCode lit)
genExprCode (Data _ tag exprs) = do
  let tagged = [i|$tag: "#{tag}"|] :: Text
  exprs' <- genIndexedPropsCode exprs
  return $
    if null exprs
      then [i|({#{tagged}})|]
      else [i|({#{tagged}, #{exprs'}})|]
genExprCode (Record _ props) = do
  props' <- genPropsCode (NonEmpty.toList props)
  return [i|({#{props'}})|]
genExprCode (Tuple _ exprs) = do
  exprs' <- genIndexedPropsCode (NonEmpty.toList exprs)
  return [i|({#{exprs'}})|]
genExprCode (Var _ (Id _ name)) = return name
genExprCode (Mut _ name expr) = do
  expr' <- genExprCode expr
  return [i|(#{name} = #{expr'})|]
genExprCode (App _ f arg) = do
  f' <- genExprCode f
  arg' <- genExprCode arg
  return [i|#{f'}(#{arg'})|]
genExprCode (Access _ expr (Id _ prop)) = do
  expr' <- genExprCode expr
  return [i|#{expr'}.#{prop}|]
genExprCode (Index _ expr ix) = do
  expr' <- genExprCode expr
  return [i|#{expr'}[#{ix}]|]
genExprCode (Cond _ cond yes no) = do
  cond' <- genExprCode cond
  yes' <- genExprCode yes
  no' <- genExprCode no
  return [i|#{cond'} ? #{yes'} : #{no'}|]
genExprCode (PatternMatch _ expr matches) = do
  oldIndent <- ask
  indent <- increaseIndentation
  expr' <- withIndentation indent (genExprCode expr)
  let name = "$$obj"
  matches' <- withIndentation indent (mapM (genMatchCode name) $ NonEmpty.toList matches)
  let matches'' = Text.intercalate " else " matches'
  return [i|(#{name} => {\n#{indent}#{matches''}\n#{oldIndent}})(#{expr'})|]
genExprCode (Fun _ param body) = genFunCode param body
genExprCode (Block _ block) = do
  content <- genStmtsCode block
  indent <- ask
  return [i|(() => {\n#{content}#{indent}})()|]
genExprCode (Debug _ expr) = do
  expr' <- genExprCode expr
  return [i|console.debug(#{expr'})|]
genExprCode (External _ params code) = do
  let params' = Text.concat $ map (\param -> [i|(#{param}) => |]) params
  return [i|#{params'}#{code}|]

genBindCode :: Bind OfValue Typed -> Reader Indentation Text
genBindCode (Bind binder' _ expr) = do
  expr' <- genExprCode (fromMaybe expr $ tryOptimize binder' expr)
  return [i|const #{binder'} = #{expr'};|]

genModuleCode :: Module Typed -> Reader Indentation Text
genModuleCode (Module values _ _ entry) = do
  defns <- fmap (Text.intercalate "\n\n") (mapM genBindCode values)
  case entry of
    Nothing -> return defns
    Just expr -> do
      expr' <- genExprCode expr
      return [i|#{defns}\n\n#{expr'};|]

runCodegen :: Module Typed -> Text
runCodegen mdule = runReader (genModuleCode mdule) ""
