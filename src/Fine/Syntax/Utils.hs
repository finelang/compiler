module Fine.Syntax.Utils where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Fine.Syntax (
  Bind (..),
  Defn (DataDefn),
  Expr (Data, Fun, GenFun, Var),
  Id,
  Name (Name),
  Pattern (..),
  Phase (Parsed),
  Range (NoRange),
  Type (DataT, Forall, FunT, TApp, TFun, TVar),
  range,
 )
import Fine.Syntax.Name (param)

unqualified :: Id -> Name
unqualified = Name Nothing Nothing

typeQualified :: Id -> Id -> Name
typeQualified tQ name = Name Nothing (Just tQ) name

binder :: Bind t p -> Name
binder (ExprBind name _ _) = name
binder (TypeBind idn _) = unqualified idn
binder (ForeignBind idn _ _) = unqualified idn

patternBoundVars :: Pattern -> [Id]
patternBoundVars (LiteralP _ _) = []
patternBoundVars (DataP _ _ patts) = concatMap patternBoundVars patts
patternBoundVars (RecordP _ props) = concatMap (patternBoundVars . snd) props
patternBoundVars (TupleP _ fst' snd' rest) = concatMap patternBoundVars (fst' : snd' : rest)
patternBoundVars (Capture idn) = [idn]
patternBoundVars (Discard _) = []

isFunction :: Expr p -> Bool
isFunction (Fun _ _ _) = True
isFunction (GenFun _ _ _ _ body) = isFunction body
isFunction _ = False

isTFunction :: Type p -> Bool
isTFunction (TFun _ _ _) = True
isTFunction _ = False

isCtor :: Expr p -> Bool
isCtor (Data _ _ _) = True
isCtor (Fun _ _ body) = isCtor body
isCtor (GenFun _ _ _ _ body) = isCtor body
isCtor _ = False

mkDataDefn :: Id -> Maybe (NonEmpty Id) -> NonEmpty (Id, [Type Parsed]) -> Defn
mkDataDefn ctTag optTParams ctors =
  let optTParams' = NonEmpty.nub <$> optTParams
      tc :: Type Parsed
      tc = TVar () (unqualified ctTag)
      retType = case optTParams' of
        Just tparams ->
          foldl (\tf tp -> TApp () tf (TVar () $ unqualified tp)) tc tparams
        _ -> tc
      ctBinds = NonEmpty.map (mkCtor optTParams' retType) ctors
      tBind = TypeBind ctTag $ case optTParams' of
        Just tparams ->
          foldr (TFun ()) (DataT () ctTag $ map (TVar () . unqualified) $ NonEmpty.toList tparams) tparams
        _ -> DataT () ctTag []
   in DataDefn tBind ctBinds
 where
  mkCtor optTParams' retType (tag, typeArgs) =
    let (type', expr) = case typeArgs of
          [] -> (retType, Data () tag [])
          (t : ts) ->
            let argTypes = t :| ts
                funType = foldr (FunT ()) retType argTypes
                params = paramsFromTypes argTypes
                fun = foldr (Fun ()) (Data () tag $ map (Var () . unqualified) $ NonEmpty.toList params) params
             in (funType, fun)
        type'' = case optTParams' of
          Just tparams -> Forall () NoRange (tparams) type'
          _ -> type'
     in ExprBind (typeQualified ctTag tag) type'' expr
  paramsFromTypes (t :| []) = param (range t) 0 :| []
  paramsFromTypes (t :| ts) =
    param (range t) 0 :| map (\(n, t') -> param (range t') n) (zip [1 .. length ts] ts)
