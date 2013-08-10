{-# LANGUAGE NamedFieldPuns #-}

module Syntax where

import Data.Word
import Data.Int
import Data.List
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
  deriving (Eq, Show)
data Op2 = And | Or | Xor | Plus
  deriving (Eq, Show)

data Constraints
  = Constraints
  { allowedOp1s      :: [Op1]
  , allowedOp2s      :: [Op2]
  , op1sLeftToUse    :: [Op1]
  , op2sLeftToUse    :: [Op2]
  , sizeAvailable    :: Int
  , unforcedElements :: Int
  , foldAvailable    :: Bool
  , tfoldAvailable   :: Bool
  }
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

enumerateConcrete :: [Value] -> Int -> Constraints -> [(Constraints, Expression)]
enumerateConcrete vals sizeTarget constraints @ Constraints { sizeAvailable }
  = filter (constraintsSatisfiable . fst) refine
  where
    refine = terminals ++ ifs ++ folds ++ op1s ++ op2s
    terminals = [ (constraints { sizeAvailable = sizeAvailable - 1 }, t)
                | sizeTarget == 1
                , sizeAvailable >= 1
                , t <- (Zero:One:(map Var [1..length Value]))
                ]

    ifs = []

    folds = [] {- TODO: folds -}

    op1s = []

    op2s = []

constraintsSatisfiable :: Constraints -> Bool
constraintsSatisfiable Constraints { op1sLeftToUse, op2sLeftToUse, sizeAvailable, unforcedElements } = (length op1sLeftToUse + length op2sLeftToUse) <= (sizeAvailable + unforcedElements)

enumerateOp1 :: Constraints -> [(Constraints, Op1)]
enumerateOp1 constraints@Constraints { allowedOp1s, op1sLeftToUse }
  = filter (constraintsSatisfiable . fst)
           [ (constraints { op1sLeftToUse = op1sLeftToUse \\ [op] }, op) | op <- allowedOp1s ]

enumerateOp2 :: Constraints -> [(Constraints, Op2)]
enumerateOp2 constraints@Constraints { allowedOp2s, op2sLeftToUse }
  = filter (constraintsSatisfiable . fst)
           [ (constraints { op2sLeftToUse = op2sLeftToUse \\ [op] }, op) | op <- allowedOp2s ]
