{-# OPTIONS_GHC -Wno-x-partial #-}

module Fine.Transform.ShuntingYard (runShuntingYard) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Errors (Errors, failure, runErrors)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, gets, modify)
import Data.List.NonEmpty (NonEmpty)
import Fine.Error (Error (SameInfixPrecedence))
import Fine.Syntax (
  Equation (..),
  Expr (App, Bin, Fun, Var),
  Id (Id),
  Op (..),
  Phase (Parsed, Transformed),
  Range (NoRange),
 )
import GHC.Err.Extra (errorUNREACHABLE)
import Unsafe.Coerce (unsafeCoerce)

type SE s e a = StateT s (Errors e) a

fail_ :: e -> SE s e ()
fail_ = lift . failure

data Assoc = NonAssoc | LeftAssoc | RightAssoc
  deriving (Eq)

fixity :: Op p -> (Assoc, Int)
fixity Pipe = (LeftAssoc, 0)
fixity RPipe = (RightAssoc, 0)
fixity Or = (RightAssoc, 2)
fixity And = (RightAssoc, 3)
fixity Eq = (NonAssoc, 4)
fixity Neq = (NonAssoc, 4)
fixity Le = (NonAssoc, 4)
fixity Ge = (NonAssoc, 4)
fixity Lt = (NonAssoc, 4)
fixity Gt = (NonAssoc, 4)
fixity Concat = (RightAssoc, 5)
fixity Add = (LeftAssoc, 6)
fixity Sub = (LeftAssoc, 6)
fixity Mult = (LeftAssoc, 7)
fixity Div = (LeftAssoc, 7)
fixity Rest = (LeftAssoc, 7)
fixity Comp = (RightAssoc, 9)
fixity RComp = (LeftAssoc, 9)

type Expr' = Expr Transformed

type Equation' = Equation Expr'

type Op' = Op Parsed

type SYStack = ([Expr'], [Op'])

operatorStack :: SYStack -> [Op']
operatorStack (_, ops) = ops

modifyOperands :: ([Expr'] -> [Expr']) -> SE SYStack c ()
modifyOperands f = modify $ \(opns, ops) -> (f opns, ops)

modifyOperators :: ([Op'] -> [Op']) -> SE SYStack c ()
modifyOperators f = modify $ \(opns, ops) -> (opns, f ops)

mkBinExpr :: [Expr'] -> Op' -> [Expr']
mkBinExpr (right : left : rest) op = go op : rest
 where
  go Pipe = App () right left
  go RPipe = App () left right
  go Comp =
    let param = Id NoRange "x"
     in Fun () param (App () left (App () right (Var () param)))
  go RComp =
    let param = Id NoRange "x"
     in Fun () param (App () right (App () left (Var () param)))
  go op' = Bin () () (unsafeCoerce op') left right
mkBinExpr _ _ = errorUNREACHABLE "Operand stack does not contains two operands."

consume :: [Expr'] -> [Op'] -> [Expr']
consume = foldl mkBinExpr

continueWithOp :: Op' -> Equation' -> SE SYStack Error Expr'
continueWithOp curr chain = do
  top <- gets (head . operatorStack)
  modifyOperators tail -- remove top from operators
  modifyOperands (`mkBinExpr` top) -- create app
  sy' curr chain

continueWithEquation :: Op' -> Equation' -> SE SYStack Error Expr'
continueWithEquation curr chain = modifyOperators (curr :) >> sy chain

-- shunting yard when the next thing to handle is the operator
sy' :: Op' -> Equation' -> SE SYStack Error Expr'
sy' curr chain = do
  noOperators <- gets (null . operatorStack)
  if noOperators
    then continueWithEquation curr chain
    else do
      top <- gets (head . operatorStack)
      let topPrec = snd $ fixity top
      let (currAssoc, currPrec) = fixity curr
      case compare topPrec currPrec of
        GT -> continueWithOp curr chain
        EQ -> case currAssoc of
          LeftAssoc -> continueWithOp curr chain
          RightAssoc -> continueWithEquation curr chain
          NonAssoc -> do
            fail_ (SameInfixPrecedence top curr)
            continueWithEquation curr chain
        LT -> continueWithEquation curr chain

-- shunting yard when the next thing to handle is the operand
sy :: Equation' -> SE SYStack Error Expr'
sy (Operand expr) = do
  (operands, operators) <- get
  pure $ head $ consume (expr : operands) operators
sy (Operation expr curr chain) = modifyOperands (expr :) >> sy' curr chain

runShuntingYard :: Equation (Expr Transformed) -> Either (NonEmpty Error) (Expr Transformed)
runShuntingYard (Operand expr) = pure expr
runShuntingYard (Operation left op chain) = runErrors $ evalStateT (sy chain) ([left], [op])
