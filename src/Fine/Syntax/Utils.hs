module Fine.Syntax.Utils where

import Data.Either (isLeft, isRight)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Fine.Error (errorUNREACHABLE)
import Fine.Syntax (
  Bind (ExprBind, TypeBind),
  Defn (DataDefn),
  Expr (App, Data, Fun, GenFun, Var),
  Id,
  Pattern (..),
  Phase (Parsed),
  Range,
  Type (DataT, Forall, FunT, TApp, TFun, TVar),
  range,
 )
import Fine.Syntax.Name (param)

patternBoundVars :: Pattern -> [Id]
patternBoundVars (LiteralP _ _) = []
patternBoundVars (DataP _ _ patts) = concatMap patternBoundVars patts
patternBoundVars (RecordP _ props) = concatMap (patternBoundVars . snd) props
patternBoundVars (TupleP _ fst' snd' rest) = concatMap patternBoundVars (fst' : snd' : rest)
patternBoundVars (ListP _ patts) = concatMap patternBoundVars patts
patternBoundVars (Capture idn) = [idn]
patternBoundVars (Discard _) = []

isFunction :: Expr p -> Bool
isFunction (Fun _ _ _) = True
isFunction (GenFun _ _ _ body) = isFunction body
isFunction _ = False

isCtor :: Expr p -> Bool
isCtor (Data _ _ _) = True
isCtor (Fun _ _ body) = isCtor body
isCtor (GenFun _ _ _ body) = isCtor body
isCtor _ = False

mkDataDefn :: Range -> Id -> Maybe (NonEmpty Id) -> NonEmpty (Id, [Type Parsed], Range) -> Defn
mkDataDefn r ctTag optTParams ctors =
  let tc :: Type Parsed
      tc = TVar (range ctTag) ctTag
      retType = case optTParams of
        Just tparams ->
          TApp r tc $ NonEmpty.map (\p -> TVar (range p) p) tparams
        _ -> tc
      ctBinds = NonEmpty.map (mkCtor optTParams retType) ctors
      tBind = TypeBind ctTag $ case optTParams of
        Just tparams ->
          (TFun r tparams)
            (DataT r ctTag $ map (\p -> TVar (range p) p) $ NonEmpty.toList tparams)
        _ -> DataT r ctTag []
   in DataDefn tBind ctBinds
 where
  mkCtor optTParams' retType (tag, typeArgs, r') =
    let (type', expr) = case typeArgs of
          [] -> (retType, Data r' tag [])
          (t : ts) ->
            let argTypes = t :| ts
                params = paramsFromTypes argTypes
             in (,)
                  (FunT r' argTypes retType)
                  (Fun r' params $ Data r' tag $ map (\p -> Var (range p) p) $ NonEmpty.toList params)
        type'' = case optTParams' of
          Just tparams -> Forall r' tparams type'
          _ -> type'
     in ExprBind tag type'' expr
  paramsFromTypes (t :| []) = param (range t) 0 :| []
  paramsFromTypes (t :| ts) =
    param (range t) 0 :| map (\(n, t') -> param (range t') n) (zip [1 .. length ts] ts)

mkAppOrFun :: Range -> Expr Parsed -> NonEmpty (Either Range (Expr Parsed)) -> Expr Parsed
mkAppOrFun r f args =
  if all isRight args
    then App r f (NonEmpty.map fromRight args)
    else
      if all isLeft args
        then f
        else
          let (params'', args'') = go (NonEmpty.toList args) 0 [] []
           in Fun r params'' (App r f args'')
 where
  go :: [Either Range (Expr Parsed)] -> Int -> [Id] -> [Expr Parsed] -> (NonEmpty Id, NonEmpty (Expr Parsed))
  go [] _ params' args' = (NonEmpty.fromList $ reverse params', NonEmpty.fromList $ reverse args')
  go (Left r' : rest) count params' args' =
    let p = param r count
     in go rest (count + 1) (p : params') (Var r' p : args')
  go (Right arg : rest) count params' args' = go rest count params' (arg : args')

  fromRight (Right x) = x
  fromRight _ = errorUNREACHABLE ""
