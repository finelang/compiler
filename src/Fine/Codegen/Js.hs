module Fine.Codegen.Js (runCodegen) where

import Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Fine.Codegen.Ready (getModuleReady)
import Fine.Codegen.Rename (runRenamer)
import Fine.Error (errorUNREACHABLE)
import Fine.Syntax (
  Bind (ExprBind, ForeignBind),
  BindType (OfExpr),
  Block (..),
  Expr (..),
  Id (Id),
  Lit (..),
  Module (Module),
  Op (..),
  Phase (Ready, Transformed),
  Range (NoRange),
  idText,
 )

type Typed = Transformed -- TODO: remove this line (and import 'Typed' phase) after typer impl

type Indentation = Text

withIndentation :: Indentation -> Reader Indentation a -> Reader Indentation a
withIndentation = local . const

increaseIndentation :: Reader Indentation Text
increaseIndentation = do
  indent <- ask
  return (indent <> "  ")

type Expr' = Expr Ready

type Block' = Block Ready

genLitCode :: Lit -> Text
genLitCode (Int v) = Text.pack $ show v
genLitCode (Float v) = Text.pack $ show v
genLitCode (Bool True) = "true"
genLitCode (Bool False) = "false"
genLitCode (Str s) = [i|"#{s}"|]
genLitCode (Unit) = "null"

genOpCode :: Op -> Text
genOpCode And = "&&"
genOpCode Or = "||"
genOpCode Le = "<="
genOpCode Ge = ">="
genOpCode Eq = "==="
genOpCode Neq = "!=="
genOpCode Lt = "<"
genOpCode Gt = ">"
genOpCode Add = "+"
genOpCode Sub = "-"
genOpCode Mult = "*"
genOpCode Div = "/"
genOpCode Rest = "%"
genOpCode Concat = "+"
genOpCode Pipe = errorUNREACHABLE "Pipe operation generates function application code."
genOpCode RPipe = errorUNREACHABLE "Reverse pipe operation generates function application code."

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
genBlockCode Void = return Text.empty
genBlockCode (Do stmt block) = do
  stmt' <- genExprCode stmt
  block' <- genBlockCode block
  indent <- ask
  return [i|#{indent}#{stmt'};\n#{block'}|]
genBlockCode (Mut var expr block) = do
  expr' <- genExprCode expr
  block' <- genBlockCode block
  indent <- ask
  return [i|#{indent}#{var} = #{expr'};\n#{block'}|]
genBlockCode (Debug expr block) = do
  expr' <- genExprCode expr
  block' <- genBlockCode block
  indent <- ask
  return [i|#{indent}console.log(#{expr'});\n#{block'}|]
genBlockCode (Let isMut bound expr block) = do
  let keyword = if isMut then "let" else "const" :: Text
  expr' <- genExprCode expr
  block' <- genBlockCode block
  indent <- ask
  return [i|#{indent}#{keyword} #{bound} = #{expr'};\n#{block'}|]
genBlockCode (Loop cond actions block) = do
  cond' <- genExprCode cond
  actions' <- genStmtsCode actions
  block' <- genBlockCode block
  indent <- ask
  return [i|#{indent}while (#{cond'}) {\n#{actions'}#{indent}}\n#{block'}|]
genBlockCode (If _ cond ifBlock block) = do
  cond' <- genExprCode cond
  ifBlock' <- genStmtsCode ifBlock
  block' <- genBlockCode block
  indent <- ask
  return [i|#{indent}if (#{cond'}) {\n#{ifBlock'}#{indent}}\n#{block'}|]

genStmtsCode :: Block' -> Reader Indentation Text
genStmtsCode block = do
  indent <- increaseIndentation
  withIndentation indent (genBlockCode block)

genFunCode :: NonEmpty Id -> Expr' -> Reader Indentation Text
genFunCode params body =
  let params' = Text.intercalate "," $ map idText $ NonEmpty.toList params
   in case body of
        Block _ block -> do
          body' <- genStmtsCode block
          indent <- ask
          return [i|((#{params'}) => {\n#{body'}#{indent}})|]
        _ -> do
          body' <- genExprCode body
          return [i|((#{params'}) => #{body'})|]

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
  props' <- genPropsCode props
  return [i|({#{props'}})|]
genExprCode (Tuple _ fst' snd' rest) = do
  exprs' <- genIndexedPropsCode (fst' : snd' : rest)
  return [i|({#{exprs'}})|]
genExprCode (List _ exprs) = do
  exprs' <- Text.intercalate ", " <$> mapM genExprCode exprs
  return [i|[#{exprs'}]|]
genExprCode (Var _ var) = return (idText var)
genExprCode (Bin _ Pipe arg f) = genExprCode (App () f (arg :| []))
genExprCode (Bin _ RPipe f arg) = genExprCode (App () f (arg :| []))
genExprCode (Bin _ op left right) = do
  let op' = genOpCode op
  left' <- genExprCode left
  right' <- genExprCode right
  return [i|(#{left'} #{op'} #{right'})|]
genExprCode (App _ f args) = do
  f' <- genExprCode f
  args' <- Text.intercalate ", " <$> mapM genExprCode (NonEmpty.toList args)
  return [i|#{f'}(#{args'})|]
genExprCode (Access _ expr prop) = do
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
genExprCode (Fun _ params body) = genFunCode params body
genExprCode (Block _ block) = do
  content <- genStmtsCode block
  indent <- ask
  return [i|(() => {\n#{content}#{indent}})()|]

genBindCode :: Bind OfExpr Ready -> Reader Indentation Text
genBindCode (ExprBind binder' _ expr) = do
  expr' <- genExprCode expr
  return [i|const #{binder'} = #{expr'};|]
genBindCode (ForeignBind binder' _ code) =
  return [i|const #{binder'} = #{code};|]

genModuleCode :: Module Ready -> Reader Indentation Text
genModuleCode (Module values _ entry) = do
  case (values, entry) of
    ([], Nothing) -> return ""
    ([], Just expr) -> (<> ";\n") <$> genExprCode expr
    (_, Nothing) -> do
      defns <- fmap (Text.intercalate "\n\n") (mapM genBindCode values)
      return defns
    (_, Just expr) -> do
      defns <- fmap (Text.intercalate "\n\n") (mapM genBindCode values)
      expr' <- genExprCode expr
      return [i|#{defns}\n\n#{expr'};\n|]

runCodegen :: Module Typed -> Text
runCodegen mdule =
  let renamed = runRenamer reservedJsWords mdule
      ready = getModuleReady renamed
   in runReader (genModuleCode ready) ""

reservedJsWords :: [Text]
reservedJsWords = [] -- TODO: fill
