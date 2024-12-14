module Transform (transformParsedExpr) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Syntax.Common (Binder (binderName), Fixity, OpChain (..))
import Syntax.Expr (Expr (..))
import qualified Syntax.Parsed as P

data Ctx = Ctx
  { binders :: S.Set Text,
    fixities :: M.Map Text Fixity
  }

emptyCtx :: Ctx
emptyCtx = Ctx S.empty M.empty

addBinders :: S.Set Text -> Ctx -> Ctx
addBinders bs' (Ctx bs fs) = Ctx (S.union bs bs') fs

transformChain :: OpChain P.Expr -> ReaderT Ctx (Writer [Text]) (OpChain Expr)
transformChain (Operand expr) = Operand <$> transform expr
transformChain (Operation chain op right) = do
  chain' <- transformChain chain
  right' <- transform right
  return (Operation chain' op right')

transform :: P.Expr -> ReaderT Ctx (Writer [Text]) Expr
transform (P.Int v r) = return (Int v r)
transform (P.Float v r) = return (Float v r)
transform (P.Id name r) = do
  ctx <- asks binders
  unless (S.member name ctx) (lift $ tell [name <> " not found"])
  return (Id name r)
transform (P.App f args r) = do
  f' <- transform f
  args' <- mapM transform args
  return (App f' args' r)
transform (P.Fun params body r) = do
  let params' = S.fromList (map binderName params)
  unless (S.size params' == length params) (lift $ tell ["duplicate params"])
  body' <- local (addBinders params') (transform body)
  return (Fun params body' r)
transform (P.Parens expr _) = transform expr
transform (P.Chain chain) = do
  chain' <- transformChain chain
  return undefined

transformParsedExpr :: P.Expr -> Either [Text] Expr
transformParsedExpr pexpr =
  let (expr, errors) = runWriter $ runReaderT (transform pexpr) emptyCtx
   in if null errors then Right expr else Left errors
