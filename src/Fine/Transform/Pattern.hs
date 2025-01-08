module Fine.Transform.Pattern (runTransform, handleVariant) where

import Control.Monad (when)
import Control.Monad.Trans.RW (RW, asks, runRW, tell)
import Data.List.Extra (repeated)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing, mapMaybe)
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
    collectErrors,
  )
import Fine.Syntax.Common
  ( Prop (..),
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

extractSpreadProp :: [Prop Expr] -> RW r Errors (Maybe Var)
extractSpreadProp props =
  case mapMaybe justSpreadProp props of
    [] -> return Nothing
    [expr] -> case expr of
      Id name -> return (Just name)
      _ -> do
        tell (collectErrors [InvalidPattern $ getRange expr])
        return Nothing
    terms -> do
      tell (collectErrors [MultipleSpreadPatterns $ map getRange terms])
      return Nothing

extractNamedProps :: [Prop Expr] -> RW VariantSpecs Errors [(Var, Pattern)]
extractNamedProps props = do
  let (names, exprs) = unzip (mapMaybe justNamedProp props)
  tell (collectErrors $ map RepeatedProp $ repeated names)
  patts <- mapM transform exprs
  return (zip names patts)

handleVariant :: Bool -> Var -> [Prop t] -> RW VariantSpecs Errors ()
handleVariant requireAllProps tag props = do
  spec <- asks (M.lookup tag)
  case spec of
    Nothing -> tell (collectErrors [UndefinedVariant tag])
    Just (VariantSpec _ varntNames _ _) -> do
      let names = S.fromList (mapMaybe (fmap fst . justNamedProp) props)
      let varntNames' = S.fromList varntNames
      when
        requireAllProps
        (tell $ collectErrors $ map (RequiredProp tag) $ S.toList $ S.difference varntNames' names)
      tell (collectErrors $ map (InvalidProp tag) $ S.toList $ S.difference names varntNames')

transform :: Expr -> RW VariantSpecs Errors Pattern
transform (Int v r) = return (Patt.Int v r)
transform (Float v r) = return (Patt.Float v r)
transform (Str s r) = return (Patt.Str s r)
transform (Unit r) = return (Patt.Unit r)
transform (Obj props r) = do
  named <- extractNamedProps props
  spread <- extractSpreadProp props
  return (Patt.Obj named spread r)
transform (Variant tag props r) = do
  spread <- extractSpreadProp props
  handleVariant (isNothing spread) tag props
  named <- extractNamedProps props
  return (Patt.Variant tag named spread r)
transform (Tuple fst' snd' rest r) = do
  fst'' <- transform fst'
  snd'' <- transform snd'
  rest' <- mapM transform rest
  return (Patt.Tuple fst'' snd'' rest' r)
transform (Id var) = return (Patt.Capture var)
transform (Parens expr) = transform expr
transform expr = do
  let r = getRange expr
  tell (collectErrors [InvalidPattern r])
  return (errorPattern r)

runTransform :: VariantSpecs -> Expr -> (Pattern, Errors)
runTransform specs expr = runRW (transform expr) specs
