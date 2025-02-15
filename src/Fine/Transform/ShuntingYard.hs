{-# OPTIONS_GHC -Wno-x-partial #-}

module Fine.Transform.ShuntingYard (runSy) where

import Control.Monad (when)
import Control.Monad.Trans.RWS.Strict (RWS, asks, get, gets, modify, runRWS, tell)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as M
import Fine.Error
  ( Error (SameInfixPrecedence),
    Errors,
    Warning (MissingFixity),
    collectError,
    collectWarning,
    errorUNREACHABLE,
  )
import Fine.Syntax.Abstract (Expr (..))
import Fine.Syntax.Common (Assoc (..), Fixity (..), HasRange (getRange), Id, OpChain (..))
import Fine.Transform.Common (Fixities)

type SYStack = ([Expr], [Id])

defaultFixity :: Fixity
defaultFixity = Fixity LeftAssoc 9

findFixity :: Id -> RWS Fixities Errors s Fixity
findFixity var = do
  maybeFix <- asks (M.lookup var)
  case maybeFix of
    Just fix -> return fix
    Nothing -> do
      tell (collectWarning $ MissingFixity var defaultFixity)
      return defaultFixity

operatorStack :: SYStack -> [Id]
operatorStack (_, ops) = ops

modifyOperands :: (Monoid w) => ([Expr] -> [Expr]) -> RWS r w SYStack ()
modifyOperands f = modify $ \(opns, ops) -> (f opns, ops)

modifyOperators :: (Monoid w) => ([Id] -> [Id]) -> RWS r w SYStack ()
modifyOperators f = modify $ \(opns, ops) -> (opns, f ops)

mkTopApp :: [Expr] -> Id -> [Expr]
mkTopApp (right : left : rest) var = App (Var var) (left :| [right]) (getRange (left, right)) : rest
mkTopApp _ _ = errorUNREACHABLE

consume :: [Expr] -> [Id] -> [Expr]
consume = foldl mkTopApp

continueWithCurr :: Id -> OpChain Expr -> RWS Fixities Errors SYStack Expr
continueWithCurr curr chain = do
  top <- gets (head . operatorStack)
  modifyOperators tail -- remove top from operators
  modifyOperands (`mkTopApp` top) -- create app
  sy' curr chain

continueWithChain :: Id -> OpChain Expr -> RWS Fixities Errors SYStack Expr
continueWithChain curr chain = modifyOperators (curr :) >> sy chain

-- shunting yard when the next thing to handle is the operator
sy' :: Id -> OpChain Expr -> RWS Fixities Errors SYStack Expr
sy' curr chain = do
  noOperators <- gets (null . operatorStack)
  if noOperators
    then continueWithChain curr chain
    else do
      top <- gets (head . operatorStack)
      topFix@(Fixity _ topPrec) <- findFixity top
      currFix@(Fixity currAssoc currPrec) <- findFixity curr
      case compare topPrec currPrec of
        GT -> continueWithCurr curr chain
        EQ -> case currAssoc of
          LeftAssoc -> continueWithCurr curr chain
          _ -> do
            when
              (currAssoc == NonAssoc)
              (tell $ collectError $ SameInfixPrecedence (top, topFix) (curr, currFix))
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
