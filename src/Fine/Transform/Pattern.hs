module Fine.Transform.Pattern (runTransform) where

import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Fine.Error (Error (InvalidPattern), Errors, collectErrors)
import Fine.Syntax.Common (Data (Data), Range, getRange)
import Fine.Syntax.ParsedExpr (Expr (..))
import Fine.Syntax.Pattern (Pattern)
import qualified Fine.Syntax.Pattern as Patt

errorPattern :: Range -> Pattern
errorPattern = Patt.Unit

transformToDataPattern :: Data Expr -> Writer Errors (Data Pattern)
transformToDataPattern (Data members) = do
  let props = map fst members
  values <- mapM (transform . snd) members
  return (Data $ zip props values)

transform :: Expr -> Writer Errors Pattern
transform (Int v r) = return (Patt.Int v r)
transform (Float v r) = return (Patt.Float v r)
transform (Str s r) = return (Patt.Str s r)
transform (Unit r) = return (Patt.Unit r)
transform (Obj d r) = do
  d' <- transformToDataPattern d
  return (Patt.Obj d' r)
transform (Variant tag d r) = do
  d' <- transformToDataPattern d
  return (Patt.Variant tag d' r)
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

runTransform :: Expr -> (Pattern, Errors)
runTransform = runWriter . transform
