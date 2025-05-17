module Fine.Typer.Common (
  Env,
  fromEnv,
  Substts,
  SubsttState (substts),
  initSubsttState,
  newSubsttVar,
  (#),
  substtVars,
  (#.),
) where

import Control.Monad.State.Class (MonadState, gets, modify')
import Data.List.NonEmpty ((<|))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Fine.Error (errorUNREACHABLE)
import Fine.Syntax (Id (Id), Kind (..), Phase (PartiallyTyped), Range)

type Env a = Map Id a

fromEnv :: Id -> Env a -> a
fromEnv var env = case Map.lookup var env of
  Just value -> value
  Nothing -> errorUNREACHABLE [i|'#{var}' should be in environment.|]

type Substts a = Env a

data SubsttState a = SubsttState
  { count :: Int,
    substts :: Substts a
  }

initSubsttState :: SubsttState a
initSubsttState = SubsttState 1 Map.empty

newSubsttVar :: (MonadState (SubsttState a) m) => Text -> Range -> m Id
newSubsttVar prefix r = do
  n <- gets count
  modify' $ \st -> st{count = n + 1}
  return $ Id r [i|#{prefix}#{n}|]

class Typeable t where
  infix 5 #
  (#) :: Substts t -> t -> t
  substtVars :: t -> Set Id

infixr 5 #.
(#.) :: (Typeable t) => Substts t -> Substts t -> Substts t
s #. s' = Map.union s ((s #) <$> s')

instance Typeable (Kind PartiallyTyped) where
  (#) :: Substts (Kind PartiallyTyped) -> Kind PartiallyTyped -> Kind PartiallyTyped
  _ # k@(KLit _) = k
  s # (TFunK r kinds kind) = TFunK r (NonEmpty.map (s #) kinds) (s # kind)
  s # k@(SubsttKVar _ var) = Map.findWithDefault k var s

  substtVars :: Kind PartiallyTyped -> Set Id
  substtVars (KLit _) = Set.empty
  substtVars (TFunK _ kinds kind) = Set.unions $ NonEmpty.map substtVars (kind <| kinds)
  substtVars (SubsttKVar _ var) = Set.singleton var
