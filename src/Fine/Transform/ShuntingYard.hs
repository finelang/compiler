{-# OPTIONS_GHC -Wno-x-partial #-}

module Fine.Transform.ShuntingYard (runShuntingYard) where

import Control.Monad.SW (SW, runSW, tell)
import Control.Monad.Trans.State.Strict (get, gets, modify)
import Fine.Error (Error (SameInfixPrecedence), errorUNREACHABLE)
import Fine.Syntax (
  Equation (..),
  Expr (Bin),
  Op (..),
  Phase (Transformed),
  range,
 )

data Assoc = NonAssoc | LeftAssoc | RightAssoc
  deriving (Eq)

fixity :: Op -> (Assoc, Int)
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

type Expr' = Expr Transformed

type Equation' = Equation Expr'

type SYStack = ([Expr'], [Op])

operatorStack :: SYStack -> [Op]
operatorStack (_, ops) = ops

modifyOperands :: (Monoid w) => ([Expr'] -> [Expr']) -> SW SYStack w ()
modifyOperands f = modify $ \(opns, ops) -> (f opns, ops)

modifyOperators :: (Monoid w) => ([Op] -> [Op]) -> SW SYStack w ()
modifyOperators f = modify $ \(opns, ops) -> (opns, f ops)

mkBinOp :: [Expr'] -> Op -> [Expr']
mkBinOp (right : left : rest) op =
  Bin (range left <> range right) () op left right : rest
mkBinOp _ _ = errorUNREACHABLE "Operand stack does not contains two operands."

consume :: [Expr'] -> [Op] -> [Expr']
consume = foldl mkBinOp

continueWithOp :: Op -> Equation' -> SW SYStack [Error] Expr'
continueWithOp curr chain = do
  top <- gets (head . operatorStack)
  modifyOperators tail -- remove top from operators
  modifyOperands (`mkBinOp` top) -- create app
  sy' curr chain

continueWithEquation :: Op -> Equation' -> SW SYStack [Error] Expr'
continueWithEquation curr chain = modifyOperators (curr :) >> sy chain

-- shunting yard when the next thing to handle is the operator
sy' :: Op -> Equation' -> SW SYStack [Error] Expr'
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
            tell [SameInfixPrecedence top curr]
            continueWithEquation curr chain
        LT -> continueWithEquation curr chain

-- shunting yard when the next thing to handle is the operand
sy :: Equation' -> SW SYStack [Error] Expr'
sy (Operand expr) = do
  (operands, operators) <- get
  return $ head $ consume (expr : operands) operators
sy (Operation expr curr chain) = modifyOperands (expr :) >> sy' curr chain

runShuntingYard :: Equation (Expr Transformed) -> (Expr Transformed, [Error])
runShuntingYard (Operand expr) = (expr, [])
runShuntingYard (Operation left op chain) = runSW (sy chain) ([left], [op])
