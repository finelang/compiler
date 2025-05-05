module Fine.Syntax.Utils where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Interpolate (i)

import Data.Either (isLeft, isRight)
import Fine.Error (errorUNREACHABLE)
import Fine.Syntax (
  Bind (ExprBind, TypeBind),
  Defn (DataDefn),
  Expr (App, Data, Fun, GenFun, Var),
  Id (Id),
  Pattern (..),
  Phase (Parsed),
  Range (NoRange),
  Type (Forall, FunT, TApp, TData, TFun, TVar),
  range,
 )

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
isFunction (GenFun _ _ body) = isFunction body
isFunction _ = False

isCtor :: Expr p -> Bool
isCtor (Data _ _ _) = True
isCtor (Fun _ _ body) = isCtor body
isCtor (GenFun _ _ body) = isCtor body
isCtor _ = False

mkDataDefn ::
  Id -> Maybe (NonEmpty Id) -> NonEmpty (Id, [Type Parsed]) -> Defn
mkDataDefn ctTag optTParams ctors =
  let tc :: Type Parsed
      tc = TVar (range ctTag) ctTag
      retType = case optTParams of
        Just tparams ->
          TApp NoRange tc $ NonEmpty.map (\param -> TVar (range param) param) tparams
        _ -> tc
      ctBinds = NonEmpty.map (mkCtor optTParams retType) ctors
      tBind = TypeBind ctTag $ case optTParams of
        Just tparams ->
          (TFun NoRange tparams)
            (TData NoRange ctTag $ map (\param -> TVar (range param) param) $ NonEmpty.toList tparams)
        _ -> TData NoRange ctTag []
   in DataDefn tBind ctBinds
 where
  mkCtor optTParams' retType (tag, typeArgs) =
    let (type', expr) = case typeArgs of
          [] -> (retType, Data NoRange tag [])
          (t : ts) ->
            let argTypes = t :| ts
                params = paramsFromTypes argTypes
             in (,)
                  (FunT NoRange argTypes retType)
                  (Fun NoRange params $ Data NoRange tag $ map (Var NoRange) $ NonEmpty.toList params)
        (type'', expr') = case optTParams' of
          Just tparams -> (Forall NoRange tparams type', GenFun NoRange tparams expr)
          _ -> (type', expr)
     in ExprBind tag type'' expr'
  paramsFromTypes (_ :| []) = Id NoRange "x0" :| []
  paramsFromTypes (_ :| ts) =
    Id NoRange "x0" :| map (\n -> Id NoRange [i|x#{n}|]) [1 .. length ts]

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
    let param = Id r' [i|x#{count}|]
     in go rest (count + 1) (param : params') (Var r' param : args')
  go (Right arg : rest) count params' args' = go rest count params' (arg : args')

  fromRight (Right x) = x
  fromRight _ = errorUNREACHABLE
