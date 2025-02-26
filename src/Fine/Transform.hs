module Fine.Transform (runTransform) where

import Control.Monad (unless, when)
import Control.Monad.Trans.SW (SW, gets, modify, runSW, tell)
import qualified Data.List.NonEmpty as NEL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
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
    Module (Module),
  )
import Fine.Syntax.Common
  ( Bind (..),
    Fixity (Fixity),
    Id,
    Kind (..),
    LitT (UnitT),
    Range (InvalidRange),
    Type (..),
  )
import qualified Fine.Syntax.Concrete as C
import Fine.Transform.Common (CtBinders, Fixities)
import qualified Fine.Transform.Expr as TE (runTransform)
import Fine.Transform.Vars (handleVars)

data State = State
  { completeValueEnv :: Set Id, -- all binders to make available for functions
    completeTypeEnv :: Set Id, -- all binders to make available for type functions
    currentValueEnv :: Set Id,
    _currentTypeEnv :: Set Id,
    typings :: Map Id Type,
    fixities :: Fixities,
    ctBinders :: CtBinders
  }

initState :: [C.Defn] -> SW State Errors ()
initState [] = return ()
initState (defn : defns) = do
  case defn of
    C.FixDefn _ _ -> return ()
    C.Defn bder _ -> modify (\st -> st {completeValueEnv = S.insert bder (completeValueEnv st)})
    C.TypingDefn bder type' -> do
      typings' <- gets typings
      if M.member bder typings'
        then tell (collectError $ RepeatedTyping bder)
        else modify (\st -> st {typings = M.insert bder type' typings'})
    C.TypeDefn (Bind bder _ _) -> modify (\st -> st {completeTypeEnv = S.insert bder (completeTypeEnv st)})
    C.DataDefn (Bind tbder _ _) ctors -> do
      let ctNames = NEL.map binder ctors
      modify
        ( \st ->
            st
              { completeTypeEnv = S.insert tbder (completeTypeEnv st),
                completeValueEnv = foldr S.insert (completeValueEnv st) ctNames
              }
        )
  initState defns

transformExpr :: C.Expr -> SW State Errors Expr
transformExpr expr = do
  fixs <- gets fixities
  cts <- gets ctBinders
  let (expr', transfErrors) = TE.runTransform fixs cts expr
  tell transfErrors
  return expr'

transformValueBind :: Bind Type C.Expr -> SW State Errors (Bind Type Expr)
transformValueBind (Bind binder' type' value) = do
  do
    currentEnv <- gets currentValueEnv
    if S.member binder' currentEnv
      then tell (collectError $ AlreadyInScope binder')
      else modify (\st -> st {currentValueEnv = S.insert binder' currentEnv})
  value' <- transformExpr value
  valueEnv <- case value' of
    Fun _ _ -> do
      env <- gets completeValueEnv
      let (vEnv, errors) = handleVars env value'
      tell errors
      return vEnv
    _ -> do
      env <- gets currentValueEnv
      let (vEnv, errors) = handleVars env value'
      tell errors
      when (S.member binder' vEnv) (tell $ collectError $ UsageBeforeInit binder')
      return vEnv
  let value'' =
        if S.null valueEnv
          then value'
          else Closure valueEnv value' (if S.member binder' valueEnv then Just binder' else Nothing)
  return (Bind binder' type' value'')

data AnyBind
  = VBind (Bind Type Expr)
  | TBind (Bind Kind Type)

justVBind :: AnyBind -> Maybe (Bind Type Expr)
justVBind (VBind bind) = Just bind
justVBind _ = Nothing

justTBind :: AnyBind -> Maybe (Bind Kind Type)
justTBind (TBind bind) = Just bind
justTBind _ = Nothing

-- transformBind :: Bind t C.Expr -> SW State Errors (Bind t Expr)
-- transformBind (Bind bound t v) = do
--   value <- transformExpr v
--   currentEnv <- do
--     vs <- gets env
--     when (S.member bound vs) (tell $ collectError $ AlreadyInScope bound)
--     return (S.insert bound vs)
--   let (valueEnv, errors) = handleVars currentEnv value
--   tell errors
--   case value of
--     Fun _ _ -> return ()
--     _ -> when (S.member bound valueEnv) (tell $ collectError $ UsageBeforeInit bound)
--   let selfBinder = if S.member bound valueEnv then Just bound else Nothing
--   let value' =
--         if isNothing selfBinder && S.null valueEnv
--           then value
--           else Closure valueEnv value selfBinder
--   modify (\st -> st {env = currentEnv})
--   return (Bind bound t value')

errorType :: Type
errorType = LiteralT UnitT InvalidRange

transformDefn :: C.Defn -> SW State Errors [AnyBind]
transformDefn (C.FixDefn fix@(Fixity _ prec) op) = do
  unless (0 <= prec && prec < 10) (tell $ collectError $ InvalidPrecedence 0 10 op) -- TODO: read from some config
  fixities' <- gets fixities
  if M.member op fixities'
    then tell (collectError $ RepeatedFixity op)
    else modify (\ctx -> ctx {fixities = M.insert op fix fixities'})
  return []
transformDefn (C.Defn bder value) = do
  type' <- do
    optT <- gets (M.lookup bder . typings)
    case optT of
      Just t -> return t
      Nothing -> tell (collectError $ MissingTyping bder) >> return errorType
  bind <- transformValueBind (Bind bder type' value)
  return [VBind bind]
transformDefn (C.TypingDefn _ _) = return []
transformDefn (C.TypeDefn tbind) = return [TBind tbind]
transformDefn (C.DataDefn tbind ctBinds) = do
  do
    let ctBinders' = NEL.map binder ctBinds
    modify (\st -> st {ctBinders = foldr S.insert (ctBinders st) ctBinders'})
  ctBinds' <- (mapM transformValueBind ctBinds)
  return (TBind tbind : (map VBind . NEL.toList) ctBinds')

transformEntryExpr :: C.Expr -> SW State Errors Expr
transformEntryExpr expr = do
  expr' <- transformExpr expr
  exprEnv <- do
    env <- gets currentValueEnv
    let (vars, errors) = handleVars env expr'
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

checkUnusedTopBinds :: [Bind Type Expr] -> Maybe Expr -> Errors
checkUnusedTopBinds bs optExpr =
  let used = foldMap (closureVars . boundValue) bs
      used' = case fmap closureVars optExpr of
        Just more -> S.union used more
        _ -> used
      binders = S.fromList $ map binder bs
   in collectWarnings $ map UnusedVar $ S.toList $ S.difference binders used'

transform :: C.Module -> SW State Errors Module
transform (C.Module defns optExpr) = do
  initState defns
  bindings <- concat <$> mapM transformDefn defns
  optExpr' <- mapM transformEntryExpr optExpr
  let valueBinds = mapMaybe justVBind bindings
  let typeBinds = mapMaybe justTBind bindings
  tell (checkUnusedTopBinds valueBinds optExpr')
  fixities' <- gets fixities
  return (Module valueBinds typeBinds fixities' optExpr')

runTransform :: C.Module -> (Either [Error] Module, [Warning])
runTransform mdule =
  let st = State S.empty S.empty S.empty S.empty M.empty M.empty S.empty
      (mdule', _, (errors, warnings)) = runSW (transform mdule) st
   in (if null errors then Right mdule' else Left errors, warnings)
