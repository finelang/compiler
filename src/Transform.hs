module Transform (transformModule) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks, local, withReaderT)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)
import Error
  ( Error (InvalidPrecedence, RepeatedFixity, RepeatedVar),
    Errors,
    Warning (UnusedVar),
    collectErrors,
    collectWarnings,
  )
import Syntax.Common (Bind (Bind), Fixities, Fixity (Fixity), OpChain (..), Var, binder, boundValue)
import Syntax.Expr (Closure (Closure), Expr (..), Module (Module), closureVars)
import qualified Syntax.Parsed as P
import Transform.FreeVars (runFreeVars)
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
transform (P.Id var) = return (Id var)
transform (P.App f args r) = do
  f' <- transform f
  args' <- mapM transform args
  return (App f' args' r)
transform (P.Fun params body r) = do
  lift (tell $ collectErrors $ map RepeatedVar $ repeated params)
  body' <- transform body
  return (Fun params body' r)
transform (P.Parens expr) = do
  expr' <- transform expr
  return $ case expr' of
    Parens _ -> expr'
    Block _ _ -> expr'
    _ -> Parens expr'
transform (P.Block exprs r) = do
  exprs' <- mapM transform exprs
  return $ case exprs' of
    expr :| [] -> expr
    _ -> Block exprs' r
transform (P.Chain chain) = transformChain chain >>= shuntingYard

data Ctx = Ctx
  { vars :: Set Var,
    fixities :: Fixities
  }

transformBinds :: [Bind () P.Expr] -> ReaderT Ctx (Writer Errors) [Bind () (Closure [Var] Expr)]
transformBinds [] = return []
transformBinds (Bind bder _ v : bs) = do
  currentFreeVars <- do
    vs <- asks vars
    if S.member bder vs
      then lift (tell $ collectErrors [RepeatedVar bder]) >> return vs
      else return (S.insert bder vs)
  let (vFreeVars, fvErrors) = runFreeVars currentFreeVars v
  lift (tell fvErrors)
  v' <- withReaderT fixities (transform v)
  let b' = Bind bder () (Closure (S.toList vFreeVars) v')
  bs' <- local (\ctx -> ctx {vars = S.union currentFreeVars vFreeVars}) (transformBinds bs)
  return (b' : bs')

checkPrecedence :: Int -> Int -> (Fixity, Var) -> Maybe Error
checkPrecedence lb ub (Fixity _ prec, op) =
  if lb <= prec && prec < ub
    then Nothing
    else Just (InvalidPrecedence lb ub op)

checkFixDefns :: [(Fixity, Var)] -> Errors
checkFixDefns fixDefns =
  let precErrors = collectErrors $ mapMaybe (checkPrecedence 0 10) fixDefns -- TODO get from some config
      repeatedErrors = collectErrors (map RepeatedFixity $ repeated $ map snd fixDefns)
   in precErrors <> repeatedErrors

checkUnusedTopBinds :: [Bind () (Closure [Var] v)] -> Errors
checkUnusedTopBinds bs =
  let used = S.fromList $ concat $ map (closureVars . boundValue) bs
      binders = S.fromList $ map binder bs
   in collectWarnings $ map UnusedVar $ S.toList $ S.difference binders used

transformModule :: P.Module -> (Either [Error] Module, [Warning])
transformModule (P.Module defns) =
  let fixDefns = mapMaybe P.justFixDefn defns
      fixDefnErrors = checkFixDefns fixDefns
      fixs = M.fromList (map swap fixDefns)

      writer = runReaderT (transformBinds $ mapMaybe P.justBind defns) (Ctx S.empty fixs)
      (bindings', bindErrors) = runWriter writer

      (errors, warnings) = fixDefnErrors <> bindErrors <> checkUnusedTopBinds bindings'
   in (if null errors then Right (Module bindings' fixs) else Left errors, warnings)
