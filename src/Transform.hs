module Transform (transformParsedExpr) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.List.NonEmpty (singleton)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Error (ErrorCollection (ErrorCollection), SemanticError (..), SemanticWarning, collectError)
import ShuntingYard (runSy)
import Syntax.Common (Fixity, OpChain (..), Operator (Operator), binderName)
import Syntax.Expr (Expr (..))
import qualified Syntax.Parsed as P

type Errors = ErrorCollection SemanticError SemanticWarning

data Ctx = Ctx
  { binders :: S.Set Text,
    fixities :: M.Map Text Fixity
  }

emptyCtx :: Ctx
emptyCtx = Ctx S.empty M.empty

addBinders :: S.Set Text -> Ctx -> Ctx
addBinders bs' (Ctx bs fs) = Ctx (S.union bs bs') fs

shuntingYard :: OpChain Expr -> ReaderT Ctx (Writer Errors) Expr
shuntingYard chain = do
  ctx <- asks fixities
  let (expr, errors) = runSy ctx chain
  lift (tell errors)
  return expr

transformChain :: OpChain P.Expr -> ReaderT Ctx (Writer Errors) (OpChain Expr)
transformChain (Operand expr) = Operand <$> transform expr
transformChain (Operation left op@(Operator name range) chain) = do
  left' <- transform left
  ctx <- asks binders
  unless (S.member name ctx) (lift $ tell $ collectError $ UndefinedVar name range)
  chain' <- transformChain chain
  return (Operation left' op chain')

transform :: P.Expr -> ReaderT Ctx (Writer Errors) Expr
transform (P.Int v r) = return (Int v r)
transform (P.Float v r) = return (Float v r)
transform (P.Id name r) = do
  ctx <- asks binders
  unless (S.member name ctx) (lift $ tell $ collectError $ UndefinedVar name r)
  return (Id name r)
transform (P.App f args r) = do
  f' <- transform f
  args' <- mapM transform args
  return (App f' args' r)
transform (P.Fun params body r) = do
  let params' = S.fromList (map binderName params)
  unless (S.size params' == length params) (lift $ tell $ collectError $ RepeatedParams (singleton "") r)
  body' <- local (addBinders params') (transform body)
  return (Fun params body' r)
transform (P.Parens expr _) = transform expr
transform (P.Chain chain) = transformChain chain >>= shuntingYard

transformParsedExpr :: P.Expr -> (Either [SemanticError] Expr, [SemanticWarning])
transformParsedExpr pexpr =
  let (expr, ErrorCollection errors warnings) = runWriter $ runReaderT (transform pexpr) emptyCtx
   in (if null errors then Right expr else Left errors, warnings)
