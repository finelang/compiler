module Fine.Transform.Pattern (runTransform, checkVariant) where

import Control.Monad.Trans.RW (RW, asks, runRW, tell)
import Data.List.Extra (repeated)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Fine.Error
  ( Error
      ( InvalidPattern,
        InvalidProp,
        MultipleSpreadPatterns,
        RepeatedProp,
        UndefinedVariant
      ),
    Errors,
    collectError,
    collectErrors,
  )
import Fine.Syntax.Abstract (Pattern (..), PropsPattern (PropsPattern), VariantSpec (..))
import Fine.Syntax.Common
  ( Lit (Unit),
    Prop (..),
    Range,
    Var,
    getRange,
    justNamedProp,
    justSpreadProp,
  )
import Fine.Syntax.Concrete (Expr (..))

type VariantSpecs = Map Var VariantSpec

errorPattern :: Range -> Pattern
errorPattern = LiteralPatt Unit

checkVariant :: Var -> [Prop t] -> RW VariantSpecs Errors ()
checkVariant tag props = do
  spec <- asks (M.lookup tag)
  case spec of
    Nothing -> tell (collectError $ UndefinedVariant tag)
    Just (VariantSpec _ varntNames) -> do
      let names = S.fromList (mapMaybe (fmap fst . justNamedProp) props)
      let varntNames' = S.fromList varntNames
      tell (collectErrors $ map (InvalidProp tag) $ S.toList $ S.difference names varntNames')

transformNamedProps :: [Prop Expr] -> RW VariantSpecs Errors [(Var, Pattern)]
transformNamedProps props = do
  let (names, exprs) = unzip (mapMaybe justNamedProp props)
  tell (collectErrors $ map RepeatedProp $ repeated names)
  patts <- mapM transform exprs
  return (zip names patts)

extractObjCapture :: [Prop Expr] -> RW r Errors (Maybe Var)
extractObjCapture props = do
  let exprs = mapMaybe justSpreadProp props
  case exprs of
    [] -> return Nothing
    [Id name] -> return (Just name)
    [expr] -> do
      tell (collectError $ InvalidPattern $ getRange expr)
      return Nothing
    _ -> do
      tell (collectError $ MultipleSpreadPatterns $ map getRange exprs)
      return Nothing

transform :: Expr -> RW VariantSpecs Errors Pattern
transform (Literal lit r) = return (LiteralPatt lit r)
transform (Obj props r) = do
  named <- transformNamedProps props
  objCapture <- extractObjCapture props
  return (ObjPatt (PropsPattern named objCapture) r)
transform (Variant tag props r) = do
  checkVariant tag props
  named <- transformNamedProps props
  objCapture <- extractObjCapture props
  return (VariantPatt tag (PropsPattern named objCapture) r)
transform (Tuple fst' snd' rest r) = do
  fst'' <- transform fst'
  snd'' <- transform snd'
  rest' <- mapM transform rest
  return (TuplePatt fst'' snd'' rest' r)
transform (Id var) = return (Capture var)
transform (Parens expr) = transform expr
transform expr = do
  let r = getRange expr
  tell (collectError $ InvalidPattern r)
  return (errorPattern r)

runTransform :: VariantSpecs -> Expr -> (Pattern, Errors)
runTransform specs expr = runRW (transform expr) specs
