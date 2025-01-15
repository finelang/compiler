module Fine.Transform.Expr (runTransform) where

import Control.Monad (forM_, when)
import Control.Monad.Trans.RW (RW, ask, asks, runRW, tell, withReader)
import Data.List.Extra (repeated)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Fine.Error (Error (..), Errors, Warning (DebugKeywordUsage), collectError, collectErrors, collectWarning)
import Fine.Syntax (Expr (..), Pattern, boundVars)
import Fine.Syntax.Common
  ( Fixities,
    OpChain (..),
    Prop (..),
    Var,
    VariantSpec (..),
    VariantSpecs,
    justNamedProp,
    justSpreadProp,
  )
import qualified Fine.Syntax.Parsed as P
import qualified Fine.Transform.Pattern as PattT
import Fine.Transform.ShuntingYard (runSy)

shuntingYard :: OpChain Expr -> RW Fixities Errors Expr
shuntingYard chain = do
  ctx <- ask
  let (expr, errors) = runSy ctx chain
  tell errors
  return expr

data Ctx = Ctx
  { fixities :: Fixities,
    variantSpecs :: VariantSpecs
  }

transformChain :: OpChain P.Expr -> RW Ctx Errors (OpChain Expr)
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
    Just (VariantSpec _ varntNames _ _) -> do
      let names = S.fromList (mapMaybe (fmap fst . justNamedProp) props)
      let varntNames' = S.fromList varntNames
      when
        (null $ mapMaybe justSpreadProp props)
        (tell $ collectErrors $ map (RequiredProp tag) $ S.toList $ S.difference varntNames' names)
      tell (collectErrors $ map (InvalidProp tag) $ S.toList $ S.difference names varntNames')

transformToPatt :: P.Expr -> RW VariantSpecs Errors Pattern
transformToPatt expr = do
  specs <- ask
  let (patt, errors) = PattT.runTransform specs expr
  tell errors
  return patt

transformProp :: Prop P.Expr -> RW Ctx Errors (Prop Expr)
transformProp (NamedProp name expr) = do
  expr' <- transform expr
  return (NamedProp name expr')
transformProp (SpreadProp expr) = SpreadProp <$> transform expr

transformProps :: [Prop P.Expr] -> RW Ctx Errors [Prop Expr]
transformProps props = do
  tell
    (collectErrors $ map RepeatedProp $ repeated $ mapMaybe (fmap fst . justNamedProp) props)
  mapM transformProp props

transform :: P.Expr -> RW Ctx Errors Expr
transform (P.Int v r) = return (Int v r)
transform (P.Float v r) = return (Float v r)
transform (P.Str s r) = return (Str s r)
transform (P.Unit r) = return (Unit r)
transform (P.Obj props r) = do
  props' <- transformProps props
  return (Obj props' r)
transform (P.Variant tag props r) =
  if null props
    then return (Id tag)
    else do
      props' <- transformProps props
      withReader variantSpecs (checkVariant tag props')
      return (Variant tag props' r)
transform (P.Tuple fst' snd' rest r) = do
  fst'' <- transform fst'
  snd'' <- transform snd'
  rest' <- mapM transform rest
  return (Tuple fst'' snd'' rest' r)
transform (P.Id var) = return (Id var)
transform (P.App f args r) = do
  f' <- transform f
  args' <- mapM transform args
  return (App f' args' r)
transform (P.Access expr prop) = do
  expr' <- transform expr
  return (Access expr' prop)
transform (P.Cond cond yes no r) = do
  cond' <- transform cond
  yes' <- transform yes
  no' <- transform no
  return (Cond cond' yes' no' r)
transform (P.PatternMatch expr matches r) = do
  expr' <- transform expr
  patterns' <- withReader variantSpecs (mapM (transformToPatt . fst) matches)
  forM_
    patterns'
    (tell . collectErrors . map RepeatedVar . repeated . boundVars)
  exprs' <- mapM (transform . snd) matches
  return $ PatternMatch expr' (L.zip patterns' exprs') r
transform (P.Fun params body r) = do
  tell (collectErrors $ map RepeatedParam $ repeated params)
  body' <- transform body
  return (Fun params body' r)
transform (P.Parens expr) = do
  expr' <- transform expr
  return $ case expr' of
    Parens _ -> expr'
    Block _ _ -> expr'
    Obj _ _ -> expr'
    _ -> Parens expr'
transform (P.Block exprs r) = do
  exprs' <- mapM transform exprs
  return $ case exprs' of
    expr :| [] -> expr
    _ -> Block exprs' r
transform (P.Chain chain) = do
  chain' <- transformChain chain
  withReader fixities (shuntingYard chain')
transform (P.ExtId ext) = return (ExtId ext)
transform (P.ExtOpApp ext l r) = do
  l' <- transform l
  r' <- transform r
  return (ExtOpApp ext l' r')
transform (P.Debug expr r) = do
  tell (collectWarning $ DebugKeywordUsage r)
  expr' <- transform expr
  return (Debug expr' r)

runTransform :: Fixities -> VariantSpecs -> P.Expr -> (Expr, Errors)
runTransform fixs specs expr = runRW (transform expr) (Ctx fixs specs)
