{-# OPTIONS_GHC -Wno-x-partial #-}

module Fine.Transform.ShuntingYard (runShuntingYard) where

import Control.Monad (when)
import Control.Monad.Trans.RWS.Strict (RWS, asks, evalRWS, get, gets, modify, tell)
import Data.Errors (Errors (Errors), error', warning)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Fine.Error (
  Error (SameInfixPrecedence),
  Warning (MissingFixity),
  errorUNREACHABLE,
 )
import Fine.Syntax (Assoc (..), Chain (..), Expr (..), Fixity (..), Id, Phase (Parsed), range)
import Fine.Transform.Common (Fixities)

type Errors' = Errors Error Warning

type SYStack = ([Expr Parsed], [Id])

defaultFixity :: Fixity
defaultFixity = Fixity LeftAssoc 9

findFixity :: Id -> RWS Fixities Errors' s Fixity
findFixity var = do
  maybeFix <- asks (Map.lookup var)
  case maybeFix of
    Just fix -> return fix
    Nothing -> do
      tell (warning $ MissingFixity var defaultFixity)
      return defaultFixity

operatorStack :: SYStack -> [Id]
operatorStack (_, ops) = ops

modifyOperands :: (Monoid w) => ([Expr Parsed] -> [Expr Parsed]) -> RWS r w SYStack ()
modifyOperands f = modify $ \(opns, ops) -> (f opns, ops)

modifyOperators :: (Monoid w) => ([Id] -> [Id]) -> RWS r w SYStack ()
modifyOperators f = modify $ \(opns, ops) -> (opns, f ops)

mkTopApp :: [Expr Parsed] -> Id -> [Expr Parsed]
mkTopApp (right : left : rest) var =
  let f = Var (range var) var
   in App (range f <> range right) f (left :| [right]) : rest
mkTopApp _ _ = errorUNREACHABLE

consume :: [Expr Parsed] -> [Id] -> [Expr Parsed]
consume = foldl mkTopApp

continueWithCurr :: Id -> Chain -> RWS Fixities Errors' SYStack (Expr Parsed)
continueWithCurr curr chain = do
  top <- gets (head . operatorStack)
  modifyOperators tail -- remove top from operators
  modifyOperands (`mkTopApp` top) -- create app
  sy' curr chain

continueWithChain :: Id -> Chain -> RWS Fixities Errors' SYStack (Expr Parsed)
continueWithChain curr chain = modifyOperators (curr :) >> sy chain

-- shunting yard when the next thing to handle is the operator
sy' :: Id -> Chain -> RWS Fixities Errors' SYStack (Expr Parsed)
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
              (tell $ error' $ SameInfixPrecedence (top, topFix) (curr, currFix))
            continueWithChain curr chain
        LT -> continueWithChain curr chain

-- shunting yard when the next thing to handle is the operand
sy :: Chain -> RWS Fixities Errors' SYStack (Expr Parsed)
sy (Operand expr) = do
  (operands, operators) <- get
  return $ head $ consume (expr : operands) operators
sy (Operation expr curr chain) = modifyOperands (expr :) >> sy' curr chain

runShuntingYard :: Fixities -> Chain -> (Expr Parsed, [Error], [Warning])
runShuntingYard _ (Operand expr) = (expr, [], [])
runShuntingYard ctx (Operation left op chain) =
  let (expr, Errors errs wrns) = evalRWS (sy chain) ctx ([left], [op])
   in (expr, errs, wrns)
