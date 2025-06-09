module Fine.Typer.Kinder (runKinder) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Errors (Errors, runErrors)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT, withReaderT)

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Fine.Error (Error, Warning)
import Fine.Syntax (
  Bind (..),
  BindType (..),
  Block (..),
  Expr (..),
  Kind,
  Module (Module),
  Phase (Kinded, Transformed),
  Type (..),
  typeof,
 )
import Fine.Typer.Common (Env, fromEnv)
import Fine.Typer.Kinder.J (runKindChecker, runKindInferrer)

type RE r e a = ReaderT r (Errors e) a

type KindEnv = Env (Kind Kinded)

kindedBlock :: Block Transformed -> RE KindEnv Error (Block Kinded)
kindedBlock (Return expr) = Return <$> kindedExpr expr
kindedBlock Void = return Void
kindedBlock (Do action block) = Do <$> kindedExpr action <*> kindedBlock block
kindedBlock (Mut var expr block) =
  Mut var <$> kindedExpr expr <*> kindedBlock block
kindedBlock (Debug expr block) = Debug <$> kindedExpr expr <*> kindedBlock block
kindedBlock (Let isMut binder value block) =
  Let isMut binder <$> kindedExpr value <*> kindedBlock block
kindedBlock (Loop cond actions block) =
  Loop <$> kindedExpr cond <*> kindedBlock actions <*> kindedBlock block
kindedBlock (LetPatt _ patt expr block) =
  LetPatt () patt <$> kindedExpr expr <*> kindedBlock block

kindedExpr :: Expr Transformed -> RE KindEnv Error (Expr Kinded)
kindedExpr (Literal r lit) = return (Literal r lit)
kindedExpr (Data r tag exprs) = Data r tag <$> mapM kindedExpr exprs
kindedExpr (Record r props) = Record r <$> (mapM . mapM) kindedExpr props
kindedExpr (Tuple r fst' snd' rest) =
  Tuple r <$> kindedExpr fst' <*> kindedExpr snd' <*> mapM kindedExpr rest
kindedExpr (List r exprs) = List r <$> mapM kindedExpr exprs
kindedExpr (Var r var) = return (Var r var)
kindedExpr (Bin r _ op left right) = Bin r () op <$> kindedExpr left <*> kindedExpr right
kindedExpr (App r f args) = App r <$> kindedExpr f <*> mapM kindedExpr args
kindedExpr (GenApp r f types) =
  GenApp r <$> kindedExpr f <*> do
    kindEnv <- ask
    mapM (lift . runKindChecker kindEnv) types
kindedExpr (Access r expr' prop) = Access r <$> kindedExpr expr' <*> return prop
kindedExpr (Index r expr' ix) = Index r <$> kindedExpr expr' <*> return ix
kindedExpr (Cond r cond yes no) =
  Cond r <$> kindedExpr cond <*> kindedExpr yes <*> kindedExpr no
kindedExpr (Fun r params body) = Fun r params <$> kindedExpr body
kindedExpr (GenFun r _ tparams body) = GenFun r () tparams <$> kindedExpr body
kindedExpr (Block r block) = Block r <$> kindedBlock block
kindedExpr (PatternMatching r _ matched matches) =
  PatternMatching r () <$> kindedExpr matched <*> (mapM . mapM) kindedExpr matches

kindedExprBind :: Bind OfExpr Transformed -> RE KindEnv Error (Bind OfExpr Kinded)
kindedExprBind (ExprBind binder type' expr) = do
  kindEnv <- ask
  type'' <- lift $ runKindChecker kindEnv type'
  expr' <- case (type'', expr) of
    (Forall _ univars _, GenFun ext _ tparams body) -> do
      let localKindEnv = Map.fromList $ NonEmpty.toList $ NonEmpty.zip tparams (NonEmpty.map snd univars)
      body' <- local (Map.union localKindEnv) (kindedExpr body)
      return $ GenFun ext () tparams body'
    _ -> kindedExpr expr
  return $ ExprBind binder type'' expr'
kindedExprBind (ForeignBind binder type' code) = do
  kindEnv <- ask
  type'' <- lift $ runKindChecker kindEnv type'
  return $ ForeignBind binder type'' code

collectTypes :: [Bind OfType Transformed] -> Env (Type Transformed)
collectTypes binds = Map.fromList $ map (\(TypeBind binder type') -> (binder, type')) binds

kindedModule :: Module Transformed -> RE () Error (Module Kinded)
kindedModule (Module exprBinds typeBinds entry) = do
  let types = collectTypes typeBinds
  typeEnv <- lift $ runKindInferrer types
  let typeBinds' = (flip map) typeBinds $
        \(TypeBind binder _) -> TypeBind binder (fromEnv binder typeEnv)
  withReaderT (const $ Map.map typeof typeEnv) $ do
    exprBinds' <- mapM kindedExprBind exprBinds
    entry' <- mapM kindedExpr entry
    return $ Module exprBinds' typeBinds' entry'

runKinder :: Module Transformed -> (Either (NonEmpty Error) (Module Kinded), [Warning])
runKinder mdule = (runErrors $ runReaderT (kindedModule mdule) (), [])
