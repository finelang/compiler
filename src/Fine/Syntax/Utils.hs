module Fine.Syntax.Utils where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Interpolate (i)

import Data.Either (isLeft, isRight)
import Fine.Error (errorUNREACHABLE)
import Fine.Syntax (
  Bind (ExprBind, TypeBind),
  Defn (DataDefn),
  Expr (App, Bin, Data, Fun, GenFun, Var),
  Id (Id),
  Op,
  Pattern (..),
  Phase (Parsed),
  Range (NoRange),
  Type (Forall, FunT, TApp, TData, TFun, TVar, TupleT),
  range,
 )

patternBoundVars :: Pattern -> [Id]
patternBoundVars (LiteralP _ _) = []
patternBoundVars (DataP _ _ patts) = concatMap patternBoundVars patts
patternBoundVars (RecordP _ props) = foldMap (patternBoundVars . snd) props
patternBoundVars (TupleP _ patts) = foldMap patternBoundVars patts
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
  Id -> Maybe (NonEmpty Id) -> NonEmpty (Id, Maybe (Type Parsed)) -> Defn
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
  mkCtor optTParams' retType (tag, optArgType) =
    let (type', expr) = case optArgType of
          Just argType ->
            let params = paramsFromType argType
             in (,)
                  (FunT NoRange argType retType)
                  (Fun NoRange params $ Data NoRange tag $ map (Var NoRange) $ NonEmpty.toList params)
          _ -> (retType, Data NoRange tag [])
        (type'', expr') = case optTParams' of
          Just tparams -> (Forall NoRange tparams type', GenFun NoRange tparams expr)
          _ -> (type', expr)
     in ExprBind tag type'' expr'
  paramsFromType (TupleT _ (_ :| ts)) =
    Id NoRange "x0" :| map (\n -> Id NoRange [i|x#{n}|]) [1 .. length ts]
  paramsFromType _ = Id NoRange "x" :| []

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

mkBinOrFun :: Range -> Op -> Maybe (Either (Expr Parsed) (Expr Parsed)) -> Expr Parsed
mkBinOrFun r op Nothing =
  let x = Id NoRange "x"
      y = Id NoRange "y"
   in Fun r (x :| [y]) (Bin r op (Var NoRange x) (Var NoRange y))
mkBinOrFun r op (Just (Left left)) =
  let x = Id NoRange "x"
   in Fun r (x :| []) (Bin r op left (Var NoRange x))
mkBinOrFun r op (Just (Right right)) =
  let x = Id NoRange "x"
   in Fun r (x :| []) (Bin r op (Var NoRange x) right)

mkPipeOrFun :: Range -> Maybe (Either (Expr Parsed) (Expr Parsed)) -> Expr Parsed
mkPipeOrFun r Nothing =
  let f = Id NoRange "f"
      x = Id NoRange "x"
   in Fun r (x :| [f]) (App r (Var NoRange f) (Var NoRange x :| []))
mkPipeOrFun _ (Just (Left f)) = f
mkPipeOrFun r (Just (Right arg)) =
  let f = Id NoRange "f"
   in Fun r (f :| []) (App r (Var NoRange f) (arg :| []))
