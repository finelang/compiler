module Fine.Transform.Expr (runTransform) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.RW (RW, ask, asks, runRW, tell, withReader)
import Data.List.Extra (repeated)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Fine.Error (Error (..), Errors, Warning (DebugKeywordUsage), collectError, collectErrors, collectWarning)
import Fine.Syntax.Abstract (Expr (..), Pattern, VariantSpec (..), boundVars)
import Fine.Syntax.Common
  ( Fixity,
    OpChain (..),
    Prop (..),
    Var (Var),
    justNamedProp,
    justSpreadProp,
  )
import qualified Fine.Syntax.Concrete as C
import qualified Fine.Transform.Pattern as TP
import Fine.Transform.ShuntingYard (runSy)

type Fixities = Map Var Fixity

type VariantSpecs = Map Var VariantSpec

data Ctx = Ctx
  { fixities :: Fixities,
    variantSpecs :: VariantSpecs,
    transformEmptyVariant :: Bool
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

transform :: C.Expr -> RW Ctx Errors Expr
transform (C.Literal lit r) = return (Literal lit r)
transform (C.Obj props r) = do
  props' <- transformProps props
  return (Obj props' r)
transform (C.Variant tag@(Var name _) props r) = do
  tev <- asks transformEmptyVariant
  if tev && null props
    then return (Id $ Var name r)
    else do
      props' <- transformProps props
      withReader variantSpecs (checkVariant tag props')
      return (Variant tag props' r)
transform (C.Tuple fst' snd' rest r) = do
  fst'' <- transform fst'
  snd'' <- transform snd'
  rest' <- mapM transform rest
  return (Tuple fst'' snd'' rest' r)
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
transform (C.Parens expr) = do
  expr' <- transform expr
  return $ case expr' of
    Parens _ -> expr'
    Block _ _ -> expr'
    Obj _ _ -> expr'
    _ -> Parens expr'
transform (C.Block exprs r) = do
  exprs' <- mapM transform exprs
  return $ case exprs' of
    expr :| [] -> expr
    _ -> Block exprs' r
transform (C.Chain chain) = do
  chain' <- transformChain chain
  withReader fixities (shuntingYard chain')
transform (C.ExtId ext) = return (ExtId ext)
transform (C.ExtOpApp ext l r) = do
  l' <- transform l
  r' <- transform r
  return (ExtOpApp ext l' r')
transform (C.Debug expr r) = do
  tell (collectWarning $ DebugKeywordUsage r)
  expr' <- transform expr
  return (Debug expr' r)

runTransform :: Fixities -> VariantSpecs -> Bool -> C.Expr -> (Expr, Errors)
runTransform fixs specs tev expr = runRW (transform expr) (Ctx fixs specs tev)
