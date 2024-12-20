{-# OPTIONS_GHC -Wno-x-partial #-}

module ShuntingYard (runSy) where

import Control.Monad (when)
import Control.Monad.Trans.RWS (RWS, asks, get, gets, modify, runRWS, tell)
import qualified Data.Map as M
import Data.Text (Text)
import Error
  ( ErrorCollection,
    SemanticError (SameInfixPrecedence),
    SemanticWarning,
    collectErrors,
    errorUNREACHABLE,
  )
import Syntax.Common (Assoc (..), Fixity (..), HasRange (getRange), OpChain (..), Operator (..))
import Syntax.Expr (Expr (..))

type Fixities = M.Map Text Fixity

type Errors = ErrorCollection SemanticError SemanticWarning

type SYStack = ([Expr], [Operator])

defaultFixity :: Fixity
defaultFixity = Fixity NonAssoc 10

findFixity :: Operator -> Fixities -> Fixity
findFixity (Operator name _) = M.findWithDefault defaultFixity name

operatorStack :: SYStack -> [Operator]
operatorStack (_, ops) = ops

modifyOperands :: (Monoid w) => ([Expr] -> [Expr]) -> RWS r w SYStack ()
modifyOperands f = modify $ \(opns, ops) -> (f opns, ops)

modifyOperators :: (Monoid w) => ([Operator] -> [Operator]) -> RWS r w SYStack ()
modifyOperators f = modify $ \(opns, ops) -> (opns, f ops)

mkTopApp :: [Expr] -> Operator -> [Expr]
mkTopApp (right : left : rest) (Operator name r) = App (Var name r) [left, right] (getRange (left, right)) : rest
mkTopApp _ _ = errorUNREACHABLE

consume :: [Expr] -> [Operator] -> [Expr]
consume = foldl mkTopApp

continueWithCurr :: Operator -> OpChain Expr -> RWS Fixities Errors SYStack Expr
continueWithCurr curr chain = do
  top <- gets (head . operatorStack)
  modifyOperators tail -- remove top from operators
  modifyOperands (`mkTopApp` top) -- create app
  sy' curr chain

continueWithChain :: Operator -> OpChain Expr -> RWS Fixities Errors SYStack Expr
continueWithChain curr chain = modifyOperators (curr :) >> sy chain

-- shunting yard when the next thing to handle is the operator
sy' :: Operator -> OpChain Expr -> RWS Fixities Errors SYStack Expr
sy' curr chain = do
  noOperators <- gets (null . operatorStack)
  if noOperators
    then continueWithChain curr chain
    else do
      top <- gets (head . operatorStack)
      topFix@(Fixity _ topPrec) <- asks (findFixity top)
      currFix@(Fixity currAssoc currPrec) <- asks (findFixity curr)
      case compare topPrec currPrec of
        GT -> continueWithCurr curr chain
        EQ -> case currAssoc of
          LeftAssoc -> continueWithCurr curr chain
          _ -> do
            when
              (currAssoc == NonAssoc)
              (tell $ collectErrors [SameInfixPrecedence (top, topFix) (curr, currFix)])
            continueWithChain curr chain
        LT -> continueWithChain curr chain

-- shunting yard when the next thing to handle is the operand
sy :: OpChain Expr -> RWS Fixities Errors SYStack Expr
sy (Operand expr) = do
  (operands, operators) <- get
  return $ head $ consume (expr : operands) operators
sy (Operation expr curr chain) = modifyOperands (expr :) >> sy' curr chain

runSy :: Fixities -> OpChain Expr -> (Expr, Errors)
runSy _ (Operand expr) = (expr, mempty)
runSy ctx (Operation left op chain) =
  let (expr, _, errors) = runRWS (sy chain) ctx ([left], [op])
   in (expr, errors)
