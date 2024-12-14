module ShuntingYard (runSy) where

import Control.Monad (when)
import Control.Monad.Trans.RWS (RWS, RWST, asks, get, gets, modify, runRWS, tell)
import qualified Data.Map as M
import Data.Text (Text)
import Syntax.Common (Assoc (..), Fixity (..), HasRange (getRange), OpChain (..), Operator (..))
import Syntax.Expr (Expr (..))

defaultFixity :: Fixity
defaultFixity = Fixity NonAssoc 10

findFixity :: Operator -> M.Map Text Fixity -> Fixity
findFixity (Operator name _) = M.findWithDefault defaultFixity name

data State = State [Expr] [Operator]

operatorStack :: State -> [Operator]
operatorStack (State _ ops) = ops

modifyOperands :: (Monoid w, Monad m) => ([Expr] -> [Expr]) -> RWST r w State m ()
modifyOperands f = modify (\(State opns ops) -> State (f opns) ops)

modifyOperators :: (Monoid w, Monad m) => ([Operator] -> [Operator]) -> RWST r w State m ()
modifyOperators f = modify (\(State opns ops) -> State opns (f ops))

mkTopApp :: [Expr] -> Operator -> [Expr]
mkTopApp (right : left : rest) (Operator name r) = App (Id name r) [left, right] (getRange (left, right)) : rest
mkTopApp _ _ = undefined -- unreachable

consume :: [Expr] -> [Operator] -> [Expr]
consume = foldl mkTopApp

continueWithCurr :: Operator -> OpChain Expr -> RWS (M.Map Text Fixity) [Text] State Expr
continueWithCurr curr chain = do
  top <- gets (head . operatorStack)
  modifyOperators tail -- remove top from operators
  modifyOperands (`mkTopApp` top) -- create app
  sy' curr chain

continueWithChain :: Operator -> OpChain Expr -> RWS (M.Map Text Fixity) [Text] State Expr
continueWithChain curr chain = modifyOperators (curr :) >> sy chain

-- shunting yard when the next thing to handle is the operator
sy' :: Operator -> OpChain Expr -> RWS (M.Map Text Fixity) [Text] State Expr
sy' curr chain = do
  noOperators <- gets (null . operatorStack)
  if noOperators
    then do
      modifyOperators (curr :)
      sy chain
    else do
      (Fixity _ topPrec) <- gets (head . operatorStack) >>= (asks . findFixity)
      (Fixity currAssoc currPrec) <- asks (findFixity curr)
      case compare topPrec currPrec of
        GT -> continueWithCurr curr chain
        EQ -> case currAssoc of
          LeftAssoc -> continueWithCurr curr chain
          _ -> do
            when (currAssoc == NonAssoc) (tell ["non assoc"])
            continueWithChain curr chain
        LT -> continueWithChain curr chain

-- shunting yard when the next thing to handle is the operand
sy :: OpChain Expr -> RWS (M.Map Text Fixity) [Text] State Expr
sy (Operand expr) = do
  State operands operators <- get
  return $ head $ consume (expr : operands) operators
sy (Operation expr curr chain) = modifyOperands (expr :) >> sy' curr chain

runSy :: M.Map Text Fixity -> OpChain Expr -> (Expr, [Text])
runSy ctx chain =
  let (expr, _, errors) = runRWS (sy chain) ctx (State [] [])
   in (expr, errors)
