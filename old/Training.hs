module Training where

import Core

data Problem
  = Problem {
      id        :: String,
      size      :: Integer,
      op1s      :: [Op1],
      op2s      :: [Op2],
      hasFold   :: Bool,
      hasTFold  :: Bool
  }


terminal x = Terminal (Complete x)
op1 x = Op1 (Complete x)
op2 o = Op2 (Complete o)

testProgram :: Program Complete
testProgram = Program (Fold (op2 Xor (op1 Shl1 (terminal (Var 0))) (op1 Shr1 (op2 Or (op2 Xor (terminal One) (op1 Shr16 (op1 Shl1 (op1 Shr1 (op2 And (If0 (op1 Shr1 (op1 Shr1 (op1 Not (op1 Shr1 (terminal (Var 0)))))) (terminal (Var 0)) (terminal One)) (terminal Zero)))))) (terminal (Var 0))))) (terminal (Var 0)) (op2 Plus (op1 Shl1 (terminal (Var 2))) (terminal (Var 1))))
