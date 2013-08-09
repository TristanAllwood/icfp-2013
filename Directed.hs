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



search :: PartialProgram -> Input -> Output -> Constraints -> [PartialProgram]
search = error "TODO"
