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
kindedBlock Void = pure Void
kindedBlock (Do action block) = Do <$> kindedExpr action <*> kindedBlock block
kindedBlock (Mut var expr block) =
  Mut var <$> kindedExpr expr <*> kindedBlock block
kindedBlock (LetMut binder value block) =
  LetMut binder <$> kindedExpr value <*> kindedBlock block
kindedBlock (Let _ patt expr block) =
  Let () patt <$> kindedExpr expr <*> kindedBlock block
kindedBlock (Debug r expr block) = Debug r <$> kindedExpr expr <*> kindedBlock block
kindedBlock (Loop cond actions block) =
  Loop <$> kindedExpr cond <*> kindedBlock actions <*> kindedBlock block

kindedExpr :: Expr Transformed -> RE KindEnv Error (Expr Kinded)
kindedExpr (Literal ext r lit) = pure (Literal ext r lit)
kindedExpr (Data ext tag exprs) = Data ext tag <$> mapM kindedExpr exprs
kindedExpr (Record ext r props) = Record ext r <$> (mapM . mapM) kindedExpr props
kindedExpr (Tuple ext r fst' snd' rest) =
  Tuple ext r <$> kindedExpr fst' <*> kindedExpr snd' <*> mapM kindedExpr rest
kindedExpr (Var ext var) = pure (Var ext var)
kindedExpr (Bin ext _ op left right) = Bin ext () op <$> kindedExpr left <*> kindedExpr right
kindedExpr (App ext f arg) = App ext <$> kindedExpr f <*> kindedExpr arg
kindedExpr (GenApp ext _ fname types) =
  GenApp ext () fname <$> do
    kindEnv <- ask
    mapM (lift . runKindChecker kindEnv) types
kindedExpr (Access ext expr' prop) = Access ext <$> kindedExpr expr' <*> pure prop
kindedExpr (Index ext r expr' ix) = Index ext r <$> kindedExpr expr' <*> pure ix
kindedExpr (Cond ext r cond yes no) =
  Cond ext r <$> kindedExpr cond <*> kindedExpr yes <*> kindedExpr no
kindedExpr (Fun ext param body) = Fun ext param <$> kindedExpr body
kindedExpr (GenFun ext _ _ tparams body) = GenFun ext () () tparams <$> kindedExpr body
kindedExpr (Block ext r block) = Block ext r <$> kindedBlock block
kindedExpr (PatternMatching ext r _ matched matches) =
  PatternMatching ext r () <$> kindedExpr matched <*> (mapM . mapM) kindedExpr matches

kindedExprBind :: Bind OfExpr Transformed -> RE KindEnv Error (Bind OfExpr Kinded)
kindedExprBind (ExprBind binder type' expr) = do
  kindEnv <- ask
  type'' <- lift $ runKindChecker kindEnv type'
  expr' <- case (type'', expr) of
    (Forall _ _ univars _, GenFun ext _ _ tparams body) -> do
      let localKindEnv = Map.fromList $ NonEmpty.toList $ NonEmpty.zip tparams (NonEmpty.map snd univars)
      body' <- local (Map.union localKindEnv) (kindedExpr body)
      pure $ GenFun ext () () tparams body'
    _ -> kindedExpr expr
  pure $ ExprBind binder type'' expr'
kindedExprBind (ForeignBind binder type' code) = do
  kindEnv <- ask
  type'' <- lift $ runKindChecker kindEnv type'
  pure $ ForeignBind binder type'' code

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
    pure $ Module exprBinds' typeBinds' entry'

runKinder :: Module Transformed -> (Either (NonEmpty Error) (Module Kinded), [Warning])
runKinder mdule = (runErrors $ runReaderT (kindedModule mdule) (), [])
