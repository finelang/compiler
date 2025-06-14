module Fine.Transform.Term (transformType, runExprTransformer) where

import Control.Monad.Collector (Collector, collect, runCollector)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Errors (ErrorsT (runErrorsT), fromEither)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Fine.Error (Error, Warning (DebugKeywordUsage))
import Fine.Syntax (
  Block (..),
  Equation (..),
  Expr (..),
  Phase (Parsed, Transformed),
  Type,
 )
import Fine.Transform.ShuntingYard (runShuntingYard)
import Unsafe.Coerce (unsafeCoerce)

transformType :: Type Parsed -> Type Transformed
transformType = unsafeCoerce

type EC e c a = ErrorsT e (Collector c) a

transformEquation :: Equation (Expr Parsed) -> EC Error Warning (Expr Transformed)
transformEquation equation' = do
  equation'' <- go equation'
  fromEither $ runShuntingYard equation''
 where
  go (Operand expr) = Operand <$> transformExpr expr
  go (Operation left op equation) =
    Operation <$> transformExpr left <*> pure op <*> go equation

transformBlock :: Block Parsed -> EC Error Warning (Block Transformed)
transformBlock (Return expr) = Return <$> transformExpr expr
transformBlock Void = pure Void
transformBlock (Do action block) =
  Do <$> transformExpr action <*> transformBlock block
transformBlock (Mut var expr block) =
  Mut var <$> transformExpr expr <*> transformBlock block
transformBlock (LetMut binder value block) =
  LetMut binder <$> transformExpr value <*> transformBlock block
transformBlock (Let _ pattern value block) =
  Let () pattern <$> transformExpr value <*> transformBlock block
transformBlock (Debug r expr block) = do
  lift $ collect $ DebugKeywordUsage r
  Debug r <$> transformExpr expr <*> transformBlock block
transformBlock (Loop cond actions block) =
  Loop <$> transformExpr cond <*> transformBlock actions <*> transformBlock block

transformExpr :: Expr Parsed -> EC Error Warning (Expr Transformed)
transformExpr (Literal ext r lit) = pure (Literal ext r lit)
transformExpr (Data ext tag exprs) = Data ext tag <$> mapM transformExpr exprs
transformExpr (Record ext r props) = Record ext r <$> (mapM . mapM) transformExpr props
transformExpr (Tuple ext r fst' snd' rest) =
  Tuple ext r <$> transformExpr fst' <*> transformExpr snd' <*> mapM transformExpr rest
transformExpr (Var ext var) = pure (Var ext var)
transformExpr (App ext f arg) = App ext <$> transformExpr f <*> transformExpr arg
transformExpr (GenApp ext _ fname types) =
  pure $ GenApp ext () fname (NonEmpty.map transformType types)
transformExpr (Access ext expr' prop) = Access ext <$> transformExpr expr' <*> pure prop
transformExpr (Index ext r expr' ix) = Index ext r <$> transformExpr expr' <*> pure ix
transformExpr (Cond ext r cond yes no) =
  Cond ext r <$> transformExpr cond <*> transformExpr yes <*> transformExpr no
transformExpr (Fun ext param' body) = Fun ext param' <$> transformExpr body
transformExpr (Block ext r block) = Block ext r <$> transformBlock block
transformExpr (PatternMatching ext r _ matched matches) =
  PatternMatching ext r () <$> transformExpr matched <*> (mapM . mapM) transformExpr matches
transformExpr (Equation _ _ equation) = transformEquation equation

runExprTransformer :: Expr Parsed -> (Either (NonEmpty Error) (Expr Transformed), [Warning])
runExprTransformer = runCollector . runErrorsT . transformExpr
