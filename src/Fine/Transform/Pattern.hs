module Fine.Transform.Pattern (runTransform, checkVariant) where

import Control.Monad (when)
import Control.Monad.Trans.RW (RW, asks, runRW, tell)
import Data.List.Extra (repeated)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Fine.Error
  ( Error
      ( InvalidPattern,
        InvalidProp,
        MultipleSpreadPatterns,
        RepeatedProp,
        RequiredProp,
        UndefinedVariant
      ),
    Errors,
    collectError,
    collectErrors,
  )
import Fine.Syntax.Common
  ( HasRange,
    Prop (..),
    Range,
    Var,
    VariantSpec (VariantSpec),
    VariantSpecs,
    getRange,
    justNamedProp,
    justSpreadProp,
  )
import Fine.Syntax.ParsedExpr (Expr (..))
import Fine.Syntax.Pattern (Pattern)
import qualified Fine.Syntax.Pattern as Patt

errorPattern :: Range -> Pattern
errorPattern = Patt.Unit

checkMultipleSpreadProps :: (HasRange t) => [Prop t] -> RW r Errors ()
checkMultipleSpreadProps props =
  case mapMaybe justSpreadProp props of
    [] -> return ()
    [_] -> return ()
    terms -> tell (collectError $ MultipleSpreadPatterns $ map getRange terms)

checkRepeatedNamedProps :: [Prop t] -> RW r Errors ()
checkRepeatedNamedProps props = do
  let names = (mapMaybe (fmap fst . justNamedProp) props)
  tell (collectErrors $ map RepeatedProp $ repeated names)

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

transformProp :: Prop Expr -> RW VariantSpecs Errors (Prop Pattern)
transformProp (NamedProp name expr) = do
  patt <- transform expr
  return (NamedProp name patt)
transformProp (SpreadProp expr) = do
  patt <- transform expr
  case patt of
    (Patt.Capture _) -> return (SpreadProp patt)
    _ -> do
      tell (collectError $ InvalidPattern $ getRange expr)
      return $ SpreadProp $ errorPattern (getRange expr)
transformProp (SelfProp name) = do
  tell (collectError $ InvalidPattern (getRange name))
  return (SelfProp name)

transform :: Expr -> RW VariantSpecs Errors Pattern
transform (Int v r) = return (Patt.Int v r)
transform (Float v r) = return (Patt.Float v r)
transform (Str s r) = return (Patt.Str s r)
transform (Unit r) = return (Patt.Unit r)
transform (Obj props r) = do
  checkRepeatedNamedProps props
  checkMultipleSpreadProps props
  props' <- mapM transformProp props
  return (Patt.Obj props' r)
transform (Variant tag props r) = do
  checkRepeatedNamedProps props
  checkMultipleSpreadProps props
  checkVariant tag props
  props' <- mapM transformProp props
  return (Patt.Variant tag props' r)
transform (Tuple fst' snd' rest r) = do
  fst'' <- transform fst'
  snd'' <- transform snd'
  rest' <- mapM transform rest
  return (Patt.Tuple fst'' snd'' rest' r)
transform (Id var) = return (Patt.Capture var)
transform (Parens expr) = transform expr
transform expr = do
  let r = getRange expr
  tell (collectError $ InvalidPattern r)
  return (errorPattern r)

runTransform :: VariantSpecs -> Expr -> (Pattern, Errors)
runTransform specs expr = runRW (transform expr) specs
