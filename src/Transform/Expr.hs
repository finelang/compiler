module Transform.Expr (runTransform, repeated) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as S
import Error (Error (..), Errors, collectErrors)
import Syntax.Common (Data (Data), Fixities, OpChain (..))
import Syntax.Expr (Expr (..))
import qualified Syntax.Parsed as P
import Transform.ShuntingYard (runSy)

repeated :: (Ord a) => [a] -> [a]
repeated xs = reverse (go xs S.empty)
  where
    go [] _ = []
    go (y : ys) s | S.member y s = y : go ys s
    go (y : ys) s = go ys (S.insert y s)

shuntingYard :: OpChain Expr -> ReaderT Fixities (Writer Errors) Expr
shuntingYard chain = do
  ctx <- ask
  let (expr, errors) = runSy ctx chain
  lift (tell errors)
  return expr

transformChain :: OpChain P.Expr -> ReaderT Fixities (Writer Errors) (OpChain Expr)
transformChain (Operand expr) = Operand <$> transform expr
transformChain (Operation left op chain) = do
  left' <- transform left
  chain' <- transformChain chain
  return (Operation left' op chain')

transform :: P.Expr -> ReaderT Fixities (Writer Errors) Expr
transform (P.Int v r) = return (Int v r)
transform (P.Float v r) = return (Float v r)
transform (P.Str s r) = return (Str s r)
transform (P.Obj (Data members) r) = do
  let keys = map fst members
  lift (tell $ collectErrors $ map RepeatedMember $ repeated $ keys)
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
  lift (tell $ collectErrors $ map RepeatedParam $ repeated params)
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
runTransform fixs expr = runWriter $ runReaderT (transform expr) fixs
