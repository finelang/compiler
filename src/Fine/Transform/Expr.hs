module Fine.Transform.Expr (runTransform) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.RW (RW, ask, asks, runRW, tell, withReader)
import Data.List.Extra (repeated)
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Fine.Error
  ( Error (..),
    Errors,
    Warning (DebugKeywordUsage),
    collectError,
    collectErrors,
    collectWarning,
  )
import Fine.Syntax.Abstract (Block (..), Expr (..), Pattern, boundVars)
import Fine.Syntax.Common
  ( OpChain (..),
    Prop (..),
    Var (Var),
    justNamedProp,
    justSpreadProp,
  )
import qualified Fine.Syntax.Concrete as C
import Fine.Transform.Common (Fixities, VariantSpec (VariantSpec), VariantSpecs)
import qualified Fine.Transform.Pattern as TP
import Fine.Transform.ShuntingYard (runSy)

data Ctx = Ctx
  { fixities :: Fixities,
    variantSpecs :: VariantSpecs
  }

shuntingYard :: OpChain Expr -> RW Fixities Errors Expr
shuntingYard chain = do
  ctx <- ask
  let (expr, errors) = runSy ctx chain
  tell errors
  return expr

transformChain :: OpChain C.Expr -> RW Ctx Errors (OpChain Expr)
transformChain (Operand expr) = Operand <$> transform expr
transformChain (Operation left op chain) = do
  left' <- transform left
  chain' <- transformChain chain
  return (Operation left' op chain')

checkVariant :: Var -> [Prop t] -> RW VariantSpecs Errors ()
checkVariant tag props = do
  spec <- asks (M.lookup tag)
  case spec of
    Nothing -> tell (collectError $ UndefinedVariant tag)
    Just (VariantSpec _ varntNames) -> do
      let names = S.fromList (mapMaybe (fmap fst . justNamedProp) props)
      let varntNames' = S.fromList varntNames
      when
        (null $ mapMaybe justSpreadProp props)
        (tell $ collectErrors $ map (RequiredProp tag) $ S.toList $ S.difference varntNames' names)
      tell (collectErrors $ map (InvalidProp tag) $ S.toList $ S.difference names varntNames')

transformToPatt :: C.Expr -> RW VariantSpecs Errors Pattern
transformToPatt expr = do
  specs <- ask
  let (patt, errors) = TP.runTransform specs expr
  tell errors
  return patt

transformProp :: Prop C.Expr -> RW Ctx Errors (Prop Expr)
transformProp (NamedProp name expr) = do
  expr' <- transform expr
  return (NamedProp name expr')
transformProp (SpreadProp expr) = SpreadProp <$> transform expr

transformProps :: [Prop C.Expr] -> RW Ctx Errors [Prop Expr]
transformProps props = do
  tell
    (collectErrors $ map RepeatedProp $ repeated $ mapMaybe (fmap fst . justNamedProp) props)
  mapM transformProp props

transformBlock :: [C.Stmt] -> C.Expr -> RW Ctx Errors Block
transformBlock [] expr = Return <$> transform expr
transformBlock (C.Do action : stmts) expr =
  Do <$> transform action <*> transformBlock stmts expr
transformBlock (C.Let bound _ val : stmts) expr =
  Let bound () <$> transform val <*> transformBlock stmts expr

transform :: C.Expr -> RW Ctx Errors Expr
transform (C.Literal lit r) = return (Literal lit r)
transform (C.Obj props r) = do
  props' <- transformProps props
  return (Obj props' r)
transform (C.Variant tag@(Var name _) props r) = do
  props' <- transformProps props
  withReader variantSpecs (checkVariant tag props')
  return $
    if null props'
      then Id (Var name r)
      else Variant tag props' r
transform (C.Tuple exprs r) = do
  exprs' <- mapM transform exprs
  return (Tuple exprs' r)
transform (C.Id var) = return (Id var)
transform (C.App f args r) = do
  f' <- transform f
  args' <- mapM transform args
  return (App f' args' r)
transform (C.Access expr prop) = do
  expr' <- transform expr
  return (Access expr' prop)
transform (C.Cond cond yes no r) = do
  cond' <- transform cond
  yes' <- transform yes
  no' <- transform no
  return (Cond cond' yes' no' r)
transform (C.PatternMatch expr matches r) = do
  expr' <- transform expr
  patterns' <- withReader variantSpecs (mapM (transformToPatt . fst) matches)
  forM_
    patterns'
    (tell . collectErrors . map RepeatedVar . repeated . boundVars)
  exprs' <- mapM (transform . snd) matches
  return $ PatternMatch expr' (L.zip patterns' exprs') r
transform (C.Fun params body r) = do
  tell (collectErrors $ map RepeatedParam $ repeated params)
  body' <- transform body
  return (Fun params body' r)
transform (C.Block stmts expr r) = do
  block <- transformBlock stmts expr
  return (Block block r)
transform (C.Chain chain) = do
  chain' <- transformChain chain
  withReader fixities (shuntingYard chain')
transform (C.ExtExpr ext) = return (ExtExpr ext)
transform (C.Debug expr r) = do
  tell (collectWarning $ DebugKeywordUsage r)
  expr' <- transform expr
  return (Debug expr' r)

runTransform :: Fixities -> VariantSpecs -> C.Expr -> (Expr, Errors)
runTransform fixs specs expr = runRW (transform expr) (Ctx fixs specs)
