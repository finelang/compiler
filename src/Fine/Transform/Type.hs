module Fine.Transform.Type (transformType) where

import Data.List.NonEmpty qualified as NonEmpty
import Fine.Syntax (Phase (Parsed, Transformed), Type (..))

transformType :: Type Parsed -> Type Transformed
transformType (LiteralT ext lit) = LiteralT ext lit
transformType (VoidT ext) = VoidT ext
transformType (TupleT ext types) = TupleT ext (NonEmpty.map transformType types)
transformType (RecordT ext propTypes) =
  RecordT ext $ (NonEmpty.map . fmap) transformType propTypes
transformType (FunT ext argTypes bodyType) =
  FunT ext (NonEmpty.map transformType argTypes) (transformType bodyType)
transformType (Forall ext univars type') = Forall ext univars (transformType type')
transformType (TData ext tag types) = TData ext tag (map transformType types)
transformType (TVar ext var) = TVar ext var
transformType (TApp ext typeFun typeArgs) =
  TApp ext (transformType typeFun) (NonEmpty.map transformType typeArgs)
transformType (TFun ext typeParams typeBody) = TFun ext typeParams (transformType typeBody)
