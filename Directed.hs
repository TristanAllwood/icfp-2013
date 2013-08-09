{-# LANGUAGE NamedFieldPuns #-}
module Directed where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Maybe
import Data.Word
import Syntax (Var, Op1(..), Op2(..), Value)
import qualified Syntax as S

data PartialProgram = Program PartialExpression SizeLeft
  deriving Show

data PartialExpression = Unforced
                       | Terminal Terminal
                       | If0 PartialExpression PartialExpression PartialExpression
                       | Fold PartialExpression PartialExpression PartialExpression
                       | Op1 Op1 PartialExpression
                       | Op2 Op2 S.Expression PartialExpression
                       | Concrete S.Expression
   deriving Show

data Terminal = Zero | One | Var Var
  deriving Show

type SizeLeft = Int8

type Input  = Word64
type Output = Word64

data Target
  = Target
  { targetBits    :: Word64
  , importantMask :: Word64
  }

satisfies :: Word64 -> Target -> Bool
satisfies value (Target { targetBits, importantMask })
  = (value .&. importantMask) == (targetBits .&. importantMask)

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
                 -> Constraints -> SizeLeft -> [(SizeLeft, PartialExpression)]
searchExpression Unforced vals target constraints sizeleft                  = [] {- TODO -}
searchExpression (Terminal terminals) vals target constraints sizeleft      = [] {- TODO -}
searchExpression (If0 c ift iff) vals target constraints sizeleft           = [] {- TODO -}
searchExpression (Fold over init function) vals target constraints sizeleft = [] {- TODO -}

searchExpression (Op1 op exp) vals target constraints sizeleft = do
  let target' = invertOp1 target op
  (sizeleft', exp') <- searchExpression exp vals target' constraints sizeleft
  let rv = castConcrete1 exp' (Op1 op) (Concrete . S.Op1 op)
  return (sizeleft', rv)

searchExpression (Op2 op lhs rhs) vals target constraints sizeleft = do
  let lval = S.evalExpression lhs vals
  target' <- maybeToList (invertOp2 target lval op)
  (sizeleft', rhs') <- searchExpression rhs vals target' constraints sizeleft
  let rv = castConcrete1 rhs' (Op2 op lhs) (Concrete . S.Op2 op lhs)
  error "TODO"

searchExpression c@(Concrete exp) vals target constraints sizeleft = do
  guard $ (S.evalExpression exp vals) `satisfies` target
  return (sizeleft, c)

castConcrete1 :: PartialExpression -> (PartialExpression -> a)
              -> (S.Expression -> a) -> a
castConcrete1 (Concrete s) _ f = f s
castConcrete1 e f _ = f e

invertOp1 :: Target -> Op1 -> Target
invertOp1 t@Target { targetBits, importantMask } op
  | Not   <- op = t { targetBits = complement targetBits }
  | Shl1  <- op = t { targetBits = shiftR targetBits 1,
                      importantMask = shiftR importantMask 1 }
  | Shr1  <- op = t { targetBits = shiftL targetBits 1,
                      importantMask = shiftL importantMask 1 }
  | Shr4  <- op = t { targetBits = shiftL targetBits 4,
                      importantMask = shiftL importantMask 4 }
  | Shr16 <- op = t { targetBits = shiftL targetBits 16,
                      importantMask = shiftL importantMask 16 }


invertOp2 :: Target -> Word64 -> Op2 -> Maybe Target
invertOp2 t@Target { targetBits, importantMask } lhs op
  | And  <- op = do let rt    = targetBits
                    let rm    = importantMask .&. lhs
                    let prune = targetBits .&. importantMask .&. (complement lhs)
                    guard (prune == 0)
                    return Target { targetBits = rt, importantMask = rm }
  | Or   <- op = error "TODO"
  | Xor  <- op = error "TODO"
  | Plus <- op = error "TODO"
