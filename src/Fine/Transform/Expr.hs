module Fine.Transform.Expr (runTransform) where

import Control.Monad.Trans.RW (RW, ask, runRW, tell, withReader)
import qualified Data.List.NonEmpty as NEL
import Fine.Error
  ( Error (..),
    Errors,
    Warning (DebugKeywordUsage),
    collectError,
    collectWarning,
  )
import Fine.Syntax.Abstract (Block (..), Expr (..), Pattern)
import Fine.Syntax.Common (HasRange (range), Lit (Unit), OpChain (..))
import qualified Fine.Syntax.Concrete as C
import Fine.Transform.Common (CtBinders, Fixities)
import qualified Fine.Transform.Pattern as TP
import Fine.Transform.ShuntingYard (runSy)

data Ctx = Ctx
  { fixities :: Fixities,
    ctBinders :: CtBinders
  }

shuntingYard :: OpChain Expr -> RW Fixities Errors Expr
shuntingYard chain = do
  ctx <- ask
  let (expr, errors) = runSy ctx chain
  tell errors
  return expr

transformChain :: OpChain C.Expr -> RW Ctx Errors (OpChain Expr)
transformChain (Operand expr) = Operand <$> transform expr
transformChain (Operation left op chain) = do
  left' <- transform left
  chain' <- transformChain chain
  return (Operation left' op chain')

transformToPatt :: C.Expr -> RW CtBinders Errors Pattern
transformToPatt expr = do
  cts <- ask
  let (patt, errors) = TP.runTransform cts expr
  tell errors
  return patt

transformBlock :: [C.Stmt] -> C.Expr -> RW Ctx Errors Block
transformBlock [] expr = Return <$> transform expr
transformBlock (C.Do action : stmts) expr =
  Do <$> transform action <*> transformBlock stmts expr
transformBlock (C.Let isMut bound _ val : stmts) expr =
  Let isMut bound () <$> transform val <*> transformBlock stmts expr

transform :: C.Expr -> RW Ctx Errors Expr
transform (C.Literal lit r) = return (Literal lit r)
transform (C.Record props r) = do
  props' <- (mapM . mapM) transform props
  return (Record props' r)
transform (C.Tuple exprs r) = do
  exprs' <- mapM transform exprs
  return (Tuple exprs' r)
transform (C.Var var) = return (Var var)
transform (C.Discard r) = do
  tell (collectError $ DiscardUsage r)
  return (Literal Unit r)
transform (C.Mut var expr) = Mut var <$> transform expr
transform (C.App f arg) = do
  f' <- transform f
  arg' <- transform arg
  return (App f' arg')
transform (C.Access expr prop) = do
  expr' <- transform expr
  return (Access expr' prop)
transform (C.Index expr ix r) = do
  expr' <- transform expr
  return (Index expr' ix r)
transform (C.Cond cond yes no r) = do
  cond' <- transform cond
  yes' <- transform yes
  no' <- transform no
  return (Cond cond' yes' no' r)
transform (C.PatternMatch expr matches r) = do
  expr' <- transform expr
  patterns' <- withReader ctBinders (mapM (transformToPatt . fst) matches)
  exprs' <- mapM (transform . snd) matches
  return $ PatternMatch expr' (NEL.zip patterns' exprs') r
transform (C.Fun params body _) = do
  body' <- transform body
  return (foldr Fun body' params)
transform (C.Block stmts expr r) = do
  block <- transformBlock (NEL.toList stmts) expr
  return (Block block r)
transform (C.Debug expr r) = do
  tell (collectWarning $ DebugKeywordUsage $ range expr)
  expr' <- transform expr
  return (Debug expr' r)
transform (C.Chain chain) = do
  chain' <- transformChain chain
  withReader fixities (shuntingYard chain')
transform (C.ExtExpr ext) = return (ExtExpr ext)

runTransform :: Fixities -> CtBinders -> C.Expr -> (Expr, Errors)
runTransform fixs cts expr = runRW (transform expr) (Ctx fixs cts)
