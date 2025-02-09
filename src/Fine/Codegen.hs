module Fine.Codegen (runGenCode) where

import Control.Monad.Trans.Reader (Reader, ask, asks, local, runReader, withReaderT)
import Data.List.NonEmpty (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import Fine.Codegen.Lit (genLitCode)
import Fine.Codegen.Pattern (extractCondsAndBinds)
import Fine.Syntax.Abstract
  ( Block (..),
    Closure (Closure),
    Expr (..),
    Module (..),
    Pattern (..),
  )
import Fine.Syntax.Common
  ( Bind (..),
    Ext (Ext),
    Prop (..),
    Var (Var),
    varName,
  )

class CodeGens t ctx where
  genCode :: t -> Reader ctx Text

data Ctx = Ctx
  { indentation :: Text,
    separator :: Char,
    symNames :: Map Char Text
  }

withIndentation :: Text -> Ctx -> Ctx
withIndentation ind ctx = ctx {indentation = ind}

increaseIndentation :: Reader Ctx Text
increaseIndentation = do
  indent <- asks indentation
  return (indent <> "  ")

sanitize :: Text -> Reader (Map Char Text) Text
sanitize name = do
  syms <- ask
  let f = go syms
  return (T.concat $ map f $ T.unpack name)
  where
    go names ch = M.findWithDefault (T.singleton ch) ch names

instance (CodeGens t Ctx) => CodeGens (Prop t) Ctx where
  genCode :: Prop t -> Reader Ctx Text
  genCode (NamedProp name value) = do
    value' <- genCode value
    return [i|#{name}: #{value'}|]
  genCode (SpreadProp value) = do
    value' <- genCode value
    return [i|...#{value'}|]

genPropsCode :: (CodeGens t Ctx) => [Prop t] -> Reader Ctx Text
genPropsCode props = T.intercalate ", " <$> mapM genCode props

genMatchCode :: Text -> (Pattern, Expr) -> Reader Ctx Text
genMatchCode name (patt, expr) = do
  oldIndent <- asks indentation
  indent <- increaseIndentation
  let (conds, binds) = extractCondsAndBinds name patt
  let cond = if null conds then "true" else T.intercalate " && " conds
  let binds' = T.concat $ map (\stmt -> [i|#{indent}#{stmt};\n|]) binds
  case expr of
    Block block _ -> do
      stmts <- genStmtsCode block
      return [i|if (#{cond}) {\n#{binds'}#{stmts}#{oldIndent}}|]
    _ -> do
      expr' <- local (withIndentation indent) (genCode expr)
      return [i|if (#{cond}) {\n#{binds'}#{indent}return #{expr'};\n#{oldIndent}}|]

genBlockCode :: Block -> Reader Ctx Text
genBlockCode (Return expr) = do
  expr' <- genCode expr
  indent <- asks indentation
  return [i|#{indent}return #{expr'};\n|]
genBlockCode (Do stmt block) = do
  stmt' <- genCode stmt
  block' <- genBlockCode block
  indent <- asks indentation
  return [i|#{indent}#{stmt'};\n#{block'}|]
genBlockCode (Let bound () expr block) = do
  expr' <- genCode expr
  block' <- genBlockCode block
  indent <- asks indentation
  return [i|#{indent}const #{bound} = #{expr'};\n#{block'}|]

genStmtsCode :: Block -> Reader Ctx Text
genStmtsCode block = do
  indent <- increaseIndentation
  local (withIndentation indent) (genBlockCode block)

genFunCode :: Bool -> [Var] -> Expr -> Reader Ctx Text
genFunCode areObjParams params body = do
  let params' = T.intercalate ", " (map varName params)
  let params'' = if areObjParams then [i|{#{params'}}|] else params'
  case body of
    Block block _ -> do
      body' <- genStmtsCode block
      indent <- asks indentation
      return [i|(#{params''}) => {\n#{body'}#{indent}}|]
    _ -> do
      body' <- genCode body
      return [i|(#{params''}) => #{body'}|]

instance CodeGens Expr Ctx where
  genCode :: Expr -> Reader Ctx Text
  genCode (Literal lit _) = return (genLitCode lit)
  genCode (Obj props _) = do
    props' <- genPropsCode props
    return [i|({#{props'}})|]
  genCode (Variant tag props _) = do
    let tagged = [i|$tag: "#{tag}"|] :: Text
    props' <- genPropsCode props
    return $
      if null props
        then [i|({#{tagged}})|]
        else [i|({#{tagged}, #{props'}})|]
  genCode (Tuple exprs _) = do
    exprs' <- fmap (T.intercalate ", ") (mapM genCode $ toList exprs)
    return [i|[#{exprs'}]|]
  genCode (Id (Var name _)) = withReaderT symNames (sanitize name)
  genCode (App f args _) = do
    f' <- genCode f
    args' <- (T.intercalate ", ") <$> mapM genCode args
    return [i|#{f'}(#{args'})|]
  genCode (Access expr (Var prop _)) = do
    expr' <- genCode expr
    return [i|#{expr'}.#{prop}|]
  genCode (Cond cond yes no _) = do
    cond' <- genCode cond
    yes' <- genCode yes
    no' <- genCode no
    return [i|#{cond'} ? #{yes'} : #{no'}|]
  genCode (PatternMatch expr matches _) = do
    oldIndent <- asks indentation
    indent <- increaseIndentation
    expr' <- local (withIndentation indent) (genCode expr)
    let name = "obj"
    matches' <- local (withIndentation indent) (mapM (genMatchCode name) $ toList matches)
    let matches'' = T.intercalate [i|\n#{indent}|] matches'
    return [i|((#{name}) => {\n#{indent}#{matches''}\n#{oldIndent}})(#{expr'})|]
  genCode (Fun params body _) = genFunCode False params body
  genCode (Block block _) = do
    content <- genStmtsCode block
    indent <- asks indentation
    return [i|(() => {\n#{content}#{indent}})()|]
  genCode (ExtExpr (Ext code _)) = return code
  genCode (Debug expr _) = do
    expr' <- genCode expr
    return [i|fine$debug(#{expr'})|]
  genCode (Closed (Closure _ expr _)) = genCode expr

instance CodeGens (Bind () Expr) Ctx where
  genCode :: Bind () Expr -> Reader Ctx Text
  genCode (Bind (Var name _) _ expr) = do
    name' <- withReaderT symNames (sanitize name)
    expr' <- genCode expr
    return [i|const #{name'} = #{expr'};|]

instance CodeGens Module Ctx where
  genCode :: Module -> Reader Ctx Text
  genCode (Module binds _) = do
    stmts <- mapM genCode binds
    return (T.intercalate "\n\n" stmts)
  genCode (EntryModule binds fixs expr) = do
    code <- genCode (Module binds fixs)
    entry <- genCode expr
    return [i|#{code}\n\n#{entry};|]

runGenCode :: (CodeGens t Ctx) => Text -> t -> Text
runGenCode codeInjection x =
  let code =
        runReader
          (genCode x)
          Ctx
            { indentation = "",
              separator = '$',
              symNames =
                M.fromList
                  [ ('+', "$plus"),
                    ('-', "$mnus"),
                    ('*', "$ast"),
                    ('/', "$sol"),
                    ('%', "$pcnt"),
                    ('^', "$hat"),
                    ('|', "$bar"),
                    ('&', "$amp"),
                    ('<', "$lt"),
                    ('>', "$gt"),
                    ('=', "$eq"),
                    (':', "$coln"),
                    ('\\', "$bsol"),
                    ('?', "$qust"),
                    ('!', "$excl"),
                    ('$', "$dllr"),
                    ('@', "$at"),
                    ('~', "$tild"),
                    ('.', "$dot")
                  ]
            }
   in T.intercalate "\n\n" [codeInjection, code] <> "\n"
