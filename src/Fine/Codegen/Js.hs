module Fine.Codegen.Js (runCodegen) where

import Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Fine.Codegen.Ready (readyModule)
import Fine.Codegen.Rename (runRenamer)
import Fine.Syntax (
  Bind (ExprBind, ForeignBind),
  BindType (OfExpr),
  Block (..),
  Expr (..),
  Id (Id),
  Lit (..),
  Module (Module),
  Op (..),
  Phase (Ready, Typed),
  Range (NoRange),
  idText,
 )

type Indentation = Text

withIndentation :: Indentation -> Reader Indentation a -> Reader Indentation a
withIndentation = local . const

increaseIndentation :: Reader Indentation Text
increaseIndentation = do
  indent <- ask
  pure (indent <> "  ")

type Expr' = Expr Ready

type Block' = Block Ready

genLitCode :: Lit -> Text
genLitCode (Int v) = Text.pack $ show v
genLitCode (Float v) = Text.pack $ show v
genLitCode (Bool True) = "true"
genLitCode (Bool False) = "false"
genLitCode (Str s) = [i|"#{s}"|]
genLitCode (Unit) = "null"

genOpCode :: Op Ready -> Text
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

genPropCode :: (Id, Expr') -> Reader Indentation Text
genPropCode (prop, value) = do
  value' <- genExprCode value
  pure [i|#{prop}: #{value'}|]

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
  pure [i|#{indent}return #{expr'};\n|]
genBlockCode Void = pure Text.empty
genBlockCode (Do stmt block) = do
  stmt' <- genExprCode stmt
  block' <- genBlockCode block
  indent <- ask
  pure [i|#{indent}#{stmt'};\n#{block'}|]
genBlockCode (Mut var expr block) = do
  expr' <- genExprCode expr
  block' <- genBlockCode block
  indent <- ask
  pure [i|#{indent}#{var} = #{expr'};\n#{block'}|]
genBlockCode (LetMut bound expr block) = do
  expr' <- genExprCode expr
  block' <- genBlockCode block
  indent <- ask
  pure [i|#{indent}let #{bound} = #{expr'};\n#{block'}|]
genBlockCode (Debug _ expr block) = do
  expr' <- genExprCode expr
  block' <- genBlockCode block
  indent <- ask
  pure [i|#{indent}console.log(#{expr'});\n#{block'}|]
genBlockCode (Loop cond actions block) = do
  cond' <- genExprCode cond
  actions' <- genStmtsCode actions
  block' <- genBlockCode block
  indent <- ask
  pure [i|#{indent}while (#{cond'}) {\n#{actions'}#{indent}}\n#{block'}|]
genBlockCode (If _ cond ifBlock block) = do
  cond' <- genExprCode cond
  ifBlock' <- genStmtsCode ifBlock
  block' <- genBlockCode block
  indent <- ask
  pure [i|#{indent}if (#{cond'}) {\n#{ifBlock'}#{indent}}\n#{block'}|]
genBlockCode (LetImmut _ bound expr block) = do
  expr' <- genExprCode expr
  block' <- genBlockCode block
  indent <- ask
  pure [i|#{indent}const #{bound} = #{expr'};\n#{block'}|]

genStmtsCode :: Block' -> Reader Indentation Text
genStmtsCode block = do
  indent <- increaseIndentation
  withIndentation indent (genBlockCode block)

genExprCode :: Expr' -> Reader Indentation Text
genExprCode (Literal _ _ lit) = pure (genLitCode lit)
genExprCode (Data _ tag exprs) = do
  let tagged = [i|$tag: "#{tag}"|] :: Text
  exprs' <- genIndexedPropsCode exprs
  pure $
    if null exprs
      then [i|({#{tagged}})|]
      else [i|({#{tagged}, #{exprs'}})|]
genExprCode (Record _ _ props) = do
  props' <- genPropsCode props
  pure [i|({#{props'}})|]
genExprCode (Tuple _ _ fst' snd' rest) = do
  exprs' <- genIndexedPropsCode (fst' : snd' : rest)
  pure [i|({#{exprs'}})|]
genExprCode (Var _ var) = pure (idText var)
genExprCode (Bin _ _ op left right) = do
  let op' = genOpCode op
  left' <- genExprCode left
  right' <- genExprCode right
  pure [i|(#{left'} #{op'} #{right'})|]
genExprCode (App _ f arg) = do
  f' <- genExprCode f
  arg' <- genExprCode arg
  pure [i|#{f'}(#{arg'})|]
genExprCode (Access _ expr prop) = do
  expr' <- genExprCode expr
  pure [i|#{expr'}.#{prop}|]
genExprCode (Index _ _ expr ix) = do
  expr' <- genExprCode expr
  pure [i|#{expr'}[#{ix}]|]
genExprCode (Cond _ _ cond yes no) = do
  cond' <- genExprCode cond
  yes' <- genExprCode yes
  no' <- genExprCode no
  pure [i|#{cond'} ? #{yes'} : #{no'}|]
genExprCode (Fun _ param body) = do
  body' <- genExprCode body
  pure [i|(#{param} => #{body'})|]
genExprCode (Block _ _ block) = do
  content <- genStmtsCode block
  indent <- ask
  pure [i|(() => {\n#{content}#{indent}})()|]

genBindCode :: Bind OfExpr Ready -> Reader Indentation Text
genBindCode (ExprBind binder' _ expr) = do
  expr' <- genExprCode expr
  pure [i|const #{binder'} = #{expr'};|]
genBindCode (ForeignBind binder' _ code) =
  pure [i|const #{binder'} = #{code};|]

genModuleCode :: Module Ready -> Reader Indentation Text
genModuleCode (Module values _ entry) = do
  case (values, entry) of
    ([], Nothing) -> pure ""
    ([], Just expr) -> (<> ";\n") <$> genExprCode expr
    (_, Nothing) -> do
      defns <- fmap (Text.intercalate "\n\n") (mapM genBindCode values)
      pure defns
    (_, Just expr) -> do
      defns <- fmap (Text.intercalate "\n\n") (mapM genBindCode values)
      expr' <- genExprCode expr
      pure [i|#{defns}\n\n#{expr'};\n|]

runCodegen :: Module Typed -> Text
runCodegen mdule =
  let renamed = runRenamer reservedJsWords mdule
      ready = readyModule renamed
   in runReader (genModuleCode ready) ""

reservedJsWords :: [Text]
reservedJsWords = [] -- TODO: fill
