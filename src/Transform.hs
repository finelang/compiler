module Transform (try, transformModule) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks, local, withReaderT)
import Control.Monad.Trans.Writer (Writer, runWriter, tell)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import Error (Error (..), Errors, Warning (UnusedVar), collectErrors, collectWarnings)
import ShuntingYard (runSy)
import Syntax.Common (Bind (..), Fixity (Fixity), OpChain (..), Var (varName))
import Syntax.Expr (Closure (Closure), Expr (..), Fixities, Module (Module))
import Syntax.Parsed (defnBind)
import qualified Syntax.Parsed as P

repeated :: (Ord a) => [a] -> [a]
repeated xs = reverse (go xs S.empty)
  where
    go [] _ = []
    go (y : ys) s | S.member y s = y : go ys s
    go (y : ys) s = go ys (S.insert y s)

-- key: name of a binder (Var)
-- value: all Var values inside Expr.Id or OpChain.Operation
type Vars = Map Text [Var]

singleton' :: Text -> Var -> Vars
singleton' k v = M.singleton k [v]

union' :: Vars -> Vars -> Vars
union' = M.unionWith (++)

unions' :: (Foldable t) => t Vars -> Vars
unions' = foldl union' M.empty

chainFreeVars :: OpChain P.Expr -> Writer Errors Vars
chainFreeVars (Operand expr) = freeVars expr
chainFreeVars (Operation left op chain) = do
  leftFvs <- freeVars left
  chainFvs <- chainFreeVars chain
  return $ unions' [leftFvs, singleton' (varName op) op, chainFvs]

freeVars :: P.Expr -> Writer Errors Vars
freeVars (P.Int _ _) = return M.empty
freeVars (P.Float _ _) = return M.empty
freeVars (P.Id var) = return (singleton' (varName var) var)
freeVars (P.App f args _) = do
  fVars <- freeVars f
  argVars <- mapM freeVars args
  return $ unions' (fVars : argVars)
freeVars (P.Fun params body _) = do
  tell (collectErrors $ map RepeatedVar $ repeated params)
  let params' = M.fromList $ map (\v -> (varName v, v)) params
  bodyVars <- freeVars body
  tell (collectWarnings $ map UnusedVar $ M.elems $ M.difference params' bodyVars)
  return (M.difference bodyVars params')
freeVars (P.Parens expr) = freeVars expr
freeVars (P.Block exprs _) = unions' <$> (mapM freeVars exprs)
freeVars (P.Chain chain) = chainFreeVars chain

bindingsFreeVars :: [Bind t P.Expr] -> ReaderT (Map Text Var) (Writer Errors) [[Var]]
bindingsFreeVars [] = return []
bindingsFreeVars (Bind b _ v : bs) = do
  available <- asks (M.insert (varName b) b)
  let (value's, errors) = runWriter (freeVars v)
  lift (tell errors)
  lift $ do
    let undefined' = concat $ M.elems $ M.difference value's available
    tell (collectErrors $ map UndefinedVar undefined')
  let bind's = mapMaybe (available M.!?) (M.keys value's)
  (bind's :) <$> local (const available) (bindingsFreeVars bs)

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

transformBind :: Bind () P.Expr -> ReaderT Fixities (Writer Errors) (Bind () Expr)
transformBind bind@(Bind _ _ v) = do
  value' <- transform v
  return (bind {boundValue = value'})

varAndFixity :: P.Defn -> Maybe (Var, Fixity)
varAndFixity (P.OpDefn (Bind var _ _) fix) = Just (var, fix)
varAndFixity _ = Nothing

validFixity :: Int -> Int -> Fixity -> Bool
validFixity lb ub (Fixity _ prec) = lb <= prec && prec < ub

transformModule :: P.Module -> ReaderT a (Writer Errors) Module
transformModule (P.Module defns) = do
  let bindings = map defnBind defns
  let binders = map binder bindings

  -- error about repeated top bindings
  lift (tell $ collectErrors $ map RepeatedVar $ repeated binders)

  -- collect free vars and warn about unused top bindings
  freeVars' <- withReaderT (const M.empty) (bindingsFreeVars bindings)
  lift $ do
    let unused = S.toList $ S.difference (S.fromList binders) (S.fromList $ concat freeVars')
    tell (collectWarnings $ map UnusedVar unused)

  -- error about invalid operator precedences and make the ctx for transformation
  let pairedFixities = mapMaybe varAndFixity defns
  lift $ do
    let (lb, ub) = (0, 10) -- TODO: get from some config
    let invalid = not . validFixity lb ub
    tell (collectErrors $ map (InvalidPrecedence lb ub . fst) $ filter (invalid . snd) pairedFixities)
  let fixities = M.fromList pairedFixities

  bindings' <- withReaderT (const fixities) (mapM transformBind bindings)
  let closureBinds = zipWith (\vs b -> b {boundValue = Closure vs (boundValue b)}) freeVars' bindings'

  -- TODO: handle possible recursive values (not functions)

  return (Module closureBinds fixities)

try :: a -> (p -> ReaderT a (Writer Errors) q) -> p -> (Either [Error] q, [Warning])
try ctx f x =
  let (y, (errors, warnings)) = runWriter (runReaderT (f x) ctx)
   in (if null errors then Right y else Left errors, warnings)
