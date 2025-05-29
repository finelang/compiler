module Fine.Typer.Common (
  Env,
  fromEnv,
  Substts,
  SubsttState (substts),
  initSubsttState,
  newSubsttVar,
) where

import Control.Monad.State.Class (MonadState, gets, modify)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Fine.Error (errorUNREACHABLE)
import Fine.Syntax (Id (Id), Range)

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
  modify $ \st -> st{count = n + 1}
  return $ Id r [i|#{prefix}#{n}|]
