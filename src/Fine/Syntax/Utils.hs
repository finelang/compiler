module Fine.Syntax.Utils (boundVars, isFunction, mkDataDefn) where

import Data.List.Extra (toNonEmptyPARTIAL)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String.Interpolate (i)
import Fine.Syntax (
  Bind (ExprBind, TypeBind),
  BindType (..),
  Defn (DataDefn),
  Expr (Data, Fun, GenFun, Var),
  Id (Id),
  Pass (Parsed),
  Pattern (..),
  Range (NoRange),
  Type (Forall, FunT, TApp, TData, TFun, TVar),
  range,
 )

boundVars :: Pattern -> [Id]
boundVars (LiteralP _ _) = []
boundVars (DataP _ _ patts) = concatMap boundVars patts
boundVars (RecordP _ props) = foldMap (boundVars . snd) props
boundVars (TupleP _ patts) = foldMap boundVars patts
boundVars (Capture idn) = [idn]
boundVars (Discard _) = []

isFunction :: Expr p -> Bool
isFunction (Fun _ _ _) = True
isFunction (GenFun _ _ body) = isFunction body
isFunction _ = False

mkParams :: Int -> [Id]
mkParams len = map (\n -> Id NoRange [i|param#{n}|]) [1 .. len]

mkCtor :: Maybe (NonEmpty Id) -> Type Parsed -> (Id, Maybe (NonEmpty (Type Parsed))) -> Bind OfExpr Parsed
mkCtor optTParams retType (tag, optTypes) =
  let (type', expr) = case optTypes of
        Just types ->
          let params = mkParams (NonEmpty.length types)
           in (,)
                (FunT NoRange types retType)
                (Fun NoRange (toNonEmptyPARTIAL params) $ Data NoRange tag $ map (Var NoRange) params)
        _ -> (retType, Data NoRange tag [])
      type'' = case optTParams of
        Just tparams -> Forall NoRange tparams type'
        _ -> type'
   in ExprBind tag type'' expr

mkDataDefn ::
  Id -> Maybe (NonEmpty Id) -> NonEmpty (Id, Maybe (NonEmpty (Type Parsed))) -> Defn
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
