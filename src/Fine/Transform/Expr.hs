module Fine.Transform.Expr (runTransform) where

import Control.Monad.Trans.RW (RW, ask, asks, runRW, tell, withReader)
import Data.List.Extra (repeated)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as M
import qualified Data.Set as S
import Fine.Error (Error (..), Errors, collectErrors)
import Fine.Syntax.Common (Data (Data), Fixities, OpChain (..), VariantSpec (VariantSpec), VariantSpecs)
import Fine.Syntax.Expr (Expr (..))
import qualified Fine.Syntax.Parsed as P
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

transform :: P.Expr -> RW Ctx Errors Expr
transform (P.Int v r) = return (Int v r)
transform (P.Float v r) = return (Float v r)
transform (P.Str s r) = return (Str s r)
transform (P.Obj (Data members) r) = do
  let keys = map fst members
  tell (collectErrors $ map RepeatedProp $ repeated $ keys)
  values <- mapM (transform . snd) members
  return $ Obj (Data $ zip keys values) r
transform (P.Variant tag (Data members) r) = do
  let (names, values) = unzip members
  tell (collectErrors $ map RepeatedProp $ repeated names)
  spec <- asks (M.lookup tag . variantSpecs)
  case spec of
    Nothing -> tell (collectErrors [UndefinedVariant tag])
    Just (VariantSpec _ memberNames _ _) -> do
      let memberNames' = S.fromList memberNames
      let names' = S.fromList names
      tell (collectErrors $ map (RequiredProp tag) $ S.toList $ S.difference memberNames' names')
      tell (collectErrors $ map (InvalidProp tag) $ S.toList $ S.difference names' memberNames')
  values' <- mapM transform values
  let members' = zip names values'
  return $ Variant tag (Data members') r
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
transform (P.Cond cond yes no r) = do
  cond' <- transform cond
  yes' <- transform yes
  no' <- transform no
  return (Cond cond' yes' no' r)
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
transform (P.ExtExpr ext) = return (ExtExpr ext)

runTransform :: Fixities -> VariantSpecs -> P.Expr -> (Expr, Errors)
runTransform fixs specs expr = runRW (transform expr) (Ctx fixs specs)
