module Fine.Transform (runTransform) where

import Control.Monad (unless, when)
import Control.Monad.Trans.SW (SW, gets, modify, runSW, tell)
import qualified Data.List.NonEmpty as NEL
import qualified Data.List.NonEmpty2 as NEL2
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Fine.Error
  ( Error (..),
    Errors,
    Warning (UnusedVar),
    collectError,
    collectWarnings,
  )
import Fine.Syntax.Abstract
  ( Expr (..),
    Module (EntryModule, Module),
  )
import Fine.Syntax.Common (Bind (..), Fixity (Fixity), Id)
import qualified Fine.Syntax.Concrete as C
import Fine.Transform.Common (CtBinders, Fixities)
import qualified Fine.Transform.Expr as TE (runTransform)
import Fine.Transform.Vars (handleVars)

data State = State
  { env :: Set Id,
    fixities :: Fixities,
    ctBinders :: CtBinders
  }

transformExpr :: C.Expr -> SW State Errors Expr
transformExpr expr = do
  fixs <- gets fixities
  cts <- gets ctBinders
  let (expr', transfErrors) = TE.runTransform fixs cts expr
  tell transfErrors
  return expr'

transformBind :: Bind t C.Expr -> SW State Errors (Bind t Expr)
transformBind (Bind bound t v) = do
  value <- transformExpr v
  currentEnv <- do
    vs <- gets env
    when (S.member bound vs) (tell $ collectError $ AlreadyInScope bound)
    return (S.insert bound vs)
  let (valueEnv, errors) = handleVars currentEnv value
  tell errors
  case value of
    Fun _ _ -> return ()
    _ -> when (S.member bound valueEnv) (tell $ collectError $ UsageBeforeInit bound)
  let selfBinder = if S.member bound valueEnv then Just bound else Nothing
  let value' =
        if isNothing selfBinder && S.null valueEnv
          then value
          else Closure valueEnv value selfBinder
  modify (\st -> st {env = currentEnv})
  return (Bind bound t value')

transformDefn :: C.Defn -> SW State Errors [Bind () Expr]
transformDefn (C.FixDefn fix@(Fixity _ prec) op) = do
  unless (0 <= prec && prec < 10) (tell $ collectError $ InvalidPrecedence 0 10 op) -- TODO: read from some config
  fixities' <- gets fixities
  if M.member op fixities'
    then tell (collectError $ RepeatedFixity op)
    else modify (\ctx -> ctx {fixities = M.insert op fix fixities'})
  return []
transformDefn (C.Defn bind) = do
  bind' <- transformBind bind
  return [bind']
transformDefn (C.DataDefn ctorDefns) = NEL.toList <$> mapM transformCtorDefn ctorDefns
transformDefn (C.MRDefns binds) = do
  let binds' = NEL2.toList binds
  let binders = S.fromList (map binder binds')
  currentEnv <- gets env
  binds'' <-
    mapM
      ( \b -> do
          modify (\st -> st {env = S.union currentEnv (S.delete (binder b) binders)})
          b' <- transformBind b
          modify (\st -> st {env = currentEnv})
          return b'
      )
      binds'
  modify (\st -> st {env = S.union currentEnv binders})
  return binds''

transformCtorDefn :: C.CtorDefn -> SW State Errors (Bind () Expr)
transformCtorDefn (C.CtorDefn tag params r) = do
  let value =
        let exprs = map Var params
            data' = Data tag exprs r
         in case params of
              [] -> data'
              _ -> foldr Fun data' params
  modify
    ( \st ->
        st
          { env = S.insert tag (env st),
            ctBinders = S.insert tag (ctBinders st)
          }
    )
  return (Bind tag () value)

transformEntryExpr :: C.Expr -> SW State Errors Expr
transformEntryExpr expr = do
  expr' <- transformExpr expr
  exprEnv <- do
    currentEnv <- gets env
    let (vars, errors) = handleVars currentEnv expr'
    tell errors
    return vars
  return $
    if S.null exprEnv
      then expr'
      else Closure exprEnv expr' Nothing

closureVars :: Expr -> Set Id
closureVars (Closure clEnv _ Nothing) = clEnv
closureVars (Closure clEnv _ (Just self)) = S.insert self clEnv
closureVars _ = S.empty

checkUnusedTopBinds :: [Bind () Expr] -> Maybe Expr -> Errors
checkUnusedTopBinds bs optExpr =
  let used = foldMap (closureVars . boundValue) bs
      used' = case fmap closureVars optExpr of
        Just more -> S.union used more
        _ -> used
      binders = S.fromList $ map binder bs
   in collectWarnings $ map UnusedVar $ S.toList $ S.difference binders used'

transform :: C.Module -> SW State Errors Module
transform (C.Module defns optExpr) = do
  bindings <- concat <$> mapM transformDefn defns
  optExpr' <- mapM transformEntryExpr optExpr
  tell (checkUnusedTopBinds bindings optExpr')
  fixities' <- gets fixities
  return $ case optExpr' of
    Nothing -> Module bindings fixities'
    Just expr -> EntryModule bindings fixities' expr

runTransform :: C.Module -> (Either [Error] Module, [Warning])
runTransform mdule =
  let st = State S.empty M.empty S.empty
      (mdule', _, (errors, warnings)) = runSW (transform mdule) st
   in (if null errors then Right mdule' else Left errors, warnings)
