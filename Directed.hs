module Directed where

import Data.Word
import Data.Int

data PartialProgram = Program PartialExpression SizeLeft
  deriving Show

data PartialExpression = Unforced
                       | Terminal [Terminal]
                       | If0 PartialExpression PartialExpression PartialExpression
                       | Fold PartialExpression PartialExpression PartialExpression
                       | Op1 [Op1] PartialExpression
                       | Op2 [Op2] PartialExpression PartialExpression
   deriving Show

data Terminal = Zero | One | Var Var
  deriving Show

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
  deriving Show
data Op2 = And | Or | Xor | Plus
  deriving Show

type Var = Int8

type SizeLeft = Int8

type Input  = Word64
type Output = Word64
type Value  = Word64
type Target = Word64

data Constraints
  = Constraints
  { allowedOp1s :: [Op1]
  , allowedOp2s :: [Op2]
  , size :: Int8
  , hasFold :: Bool
  , hasTFold :: Bool
  }

search :: PartialProgram -> Input -> Output -> Constraints -> [PartialProgram]
search = error "TODO"

searchExpression :: PartialExpression -> [Value] -> Target
                 -> Constraints -> SizeLeft -> [PartialExpression]
searchExpression Unforced vals target constraints sizeleft                  = [] {- TODO -}
searchExpression (Terminal terminals) vals target constraints sizeleft      = []
searchExpression (If0 c ift iff) vals target constraints sizeleft           = []
searchExpression (Fold over init function) vals target constraints sizeleft = []
searchExpression (Op1 ops exp) vals target constraints sizeleft             = []
searchExpression (Op2 ops lhs rhs) vals target constraints sizeleft         = []
