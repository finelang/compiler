module Fine.Syntax.Utils where

import Data.Functor qualified as Functor
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Interpolate (i)

import Data.Either (isLeft, isRight)
import Fine.Error (errorUNREACHABLE)
import Fine.Syntax (
  Bind (ExprBind, TypeBind),
  Defn (DataDefn, Defn),
  Expr (App, Bin, Data, Fun, GenFun, Var),
  Id (Id),
  Op,
  Pattern (..),
  Phase (Parsed),
  Range (NoRange),
  Type (Forall, FunT, TApp, TData, TFun, TVar),
  range,
 )

patternBoundVars :: Pattern -> [Id]
patternBoundVars (LiteralP _ _) = []
patternBoundVars (DataP _ _ patts) = concatMap patternBoundVars patts
patternBoundVars (RecordP _ props) = foldMap (patternBoundVars . snd) props
patternBoundVars (TupleP _ patts) = foldMap patternBoundVars patts
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
  Id -> Maybe (NonEmpty Id) -> NonEmpty (Id, Maybe (NonEmpty (Id, Type Parsed))) -> Defn
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
  mkCtor optTParams' retType (tag, optTypedParams) =
    let (type', expr) = case optTypedParams of
          Just typedParams ->
            let (params, types) = Functor.unzip typedParams
             in (,)
                  (FunT NoRange types retType)
                  (Fun NoRange params $ Data NoRange tag $ map (Var NoRange) $ NonEmpty.toList params)
          _ -> (retType, Data NoRange tag [])
        (type'', expr') = case optTParams' of
          Just tparams -> (Forall NoRange tparams type', GenFun NoRange tparams expr)
          _ -> (type', expr)
     in ExprBind tag type'' expr'

mkExprDefn :: Id -> Maybe (NonEmpty Id) -> NonEmpty (Id, Type Parsed) -> Type Parsed -> Expr Parsed -> Defn
mkExprDefn binder optTParams typedParams retType body =
  let (params, types) = Functor.unzip typedParams
      type' = FunT NoRange types retType
      expr = Fun NoRange params body
      (type'', expr') = case optTParams of
        Just tparams -> (Forall NoRange tparams type', GenFun NoRange tparams expr)
        _ -> (type', expr)
   in Defn (ExprBind binder type'' expr')

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

mkBinOrFun :: Op -> Either Range (Expr Parsed) -> Either Range (Expr Parsed) -> Expr Parsed
mkBinOrFun op (Left lr) (Left rr) =
  let r = lr <> rr
      x = Id lr "x"
      y = Id rr "y"
   in Fun r (x :| [y]) (Bin r op (Var lr x) (Var rr y))
mkBinOrFun op (Right left) (Left rr) =
  let r = range left <> rr
      x = Id rr "x"
   in Fun r (x :| []) (Bin r op left (Var rr x))
mkBinOrFun op (Left lr) (Right right) =
  let r = lr <> range right
      x = Id lr "x"
   in Fun r (x :| []) (Bin r op (Var lr x) right)
mkBinOrFun op (Right left) (Right right) = Bin (range left <> range right) op left right
