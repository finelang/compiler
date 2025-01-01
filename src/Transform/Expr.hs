module Transform.Expr (runTransform) where

import Control.Monad.Trans.RW (RW, ask, runRW, tell)
import Data.List.Extra (repeated)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Error (Error (..), Errors, collectErrors)
import Syntax.Common (Data (Data), Fixities, OpChain (..))
import Syntax.Expr (Expr (..))
import qualified Syntax.Parsed as P
import Transform.ShuntingYard (runSy)

shuntingYard :: OpChain Expr -> RW Fixities Errors Expr
shuntingYard chain = do
  ctx <- ask
  let (expr, errors) = runSy ctx chain
  tell errors
  return expr

transformChain :: OpChain P.Expr -> RW Fixities Errors (OpChain Expr)
transformChain (Operand expr) = Operand <$> transform expr
transformChain (Operation left op chain) = do
  left' <- transform left
  chain' <- transformChain chain
  return (Operation left' op chain')

transform :: P.Expr -> RW Fixities Errors Expr
transform (P.Int v r) = return (Int v r)
transform (P.Float v r) = return (Float v r)
transform (P.Str s r) = return (Str s r)
transform (P.Obj (Data members) r) = do
  let keys = map fst members
  tell (collectErrors $ map RepeatedMember $ repeated $ keys)
  values <- mapM (transform . snd) members
  return $ Obj (Data $ zip keys values) r
transform (P.Variant tag (Data members) r) = do
  let (names, values) = unzip members
  values' <- mapM transform values
  let members' = zip names values'
  return $ Variant tag (Data members') r
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
transform (P.Chain chain) = transformChain chain >>= shuntingYard
transform (P.ExtExpr ext) = return (ExtExpr ext)

runTransform :: Fixities -> P.Expr -> (Expr, Errors)
runTransform fixs expr = runRW (transform expr) fixs
