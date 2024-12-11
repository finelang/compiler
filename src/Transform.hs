module Transform (transformParsedExpr) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, local)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import qualified Data.Set as S
import Data.Text (Text)
import Syntax.Common (binderName)
import Syntax.Expr (Expr (..))
import qualified Syntax.Parsed as P

type Ctx = S.Set Text

transform :: P.Expr -> ReaderT Ctx (Writer [Text]) Expr
transform (P.Int v r) = return (Int v r)
transform (P.Float v r) = return (Float v r)
transform (P.Id name r) = do
  ctx <- ask
  unless (S.member name ctx) (lift $ tell [name <> " not found"])
  return (Id name r)
transform (P.App f args r) = do
  f' <- transform f
  args' <- mapM transform args
  return (App f' args' r)
transform (P.Fun params body r) = do
  let params' = S.fromList (map binderName params)
  unless (S.size params' == length params) (lift $ tell ["duplicate params"])
  body' <- local (S.union params') (transform body)
  return (Fun params body' r)
transform (P.Parens expr _) = transform expr
transform (P.Chain _) = error "Not implemented"

transformParsedExpr :: P.Expr -> Either [Text] Expr
transformParsedExpr pexpr =
  let (expr, errors) = runWriter $ runReaderT (transform pexpr) S.empty
   in if null errors then Right expr else Left errors
