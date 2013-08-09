module Syntax where

import Data.Word
import Data.Int
import Data.Bits

type Var = Int
type Value  = Word64

data Expression = Zero
                | One
                | Var Var
                | If0 Expression Expression Expression
                | Fold Expression Expression Expression
                | Op1 Op1 Expression
                | Op2 Op2 Expression Expression
   deriving Show

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
  deriving Show
data Op2 = And | Or | Xor | Plus
  deriving Show

evalExpression :: Expression -> [Value] -> Word64
evalExpression Zero _                  = 0
evalExpression One  _                  = 1
evalExpression (Var v) vals            = vals !! v
evalExpression (If0 e0 e1 e2) vals
  | evalExpression e0 vals == 0        = evalExpression e1 vals
  | otherwise                          = evalExpression e2 vals
evalExpression (Fold base init f) vals = error "TODO: evalExpression: fold"
evalExpression (Op1 op e)         vals = applyOp1 op (evalExpression e vals)
evalExpression (Op2 op e1 e2)     vals = applyOp2 op (evalExpression e1 vals)
                                                     (evalExpression e2 vals)

applyOp1 :: Op1 -> Word64 -> Word64
applyOp1 Not   = complement
applyOp1 Shl1  = (`shiftL` 1)
applyOp1 Shr1  = (`shiftR` 1)
applyOp1 Shr4  = (`shiftR` 4)
applyOp1 Shr16 = (`shiftR` 16)

applyOp2 And  = (.&.)
applyOp2 Or   = (.|.)
applyOp2 Xor  = xor
applyOp2 Plus = (+)
