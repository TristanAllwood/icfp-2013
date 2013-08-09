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
                       | If0 S.Expression PartialExpression PartialExpression
                       | Fold PartialExpression PartialExpression PartialExpression
                       | Op1 Op1 PartialExpression
                       | Op2 Op2 S.Expression PartialExpression
                       | Concrete S.Expression
   deriving Show

type SizeLeft = Int8

type Input  = Word64
type Output = Word64

data Target
  = Target
  { targetBits    :: Word64
  , importantMask :: Word64
  }

c_PLUS_SPLIT_POINT :: Int
c_PLUS_SPLIT_POINT = 10

satisfies :: Word64 -> Target -> Bool
satisfies value (Target { targetBits, importantMask })
  = (value .&. importantMask) == (targetBits .&. importantMask)

data Constraints
  = Constraints
  { allowedOp1s :: [Op1]
  , allowedOp2s :: [Op2]
  , size :: Int8
  , foldAvailable :: Bool
  , tfoldAvailable :: Bool
  }
{- TODO: push this around searchExpression instead of just sizeLeft.
 -       take into accounts op1s and op2s that need to be used within
 -       size limit remaining.
 -       capture the size limit remaining in here.
 -}

search :: PartialProgram -> Input -> Output -> Constraints -> [PartialProgram]
search = error "TODO"

searchExpression :: PartialExpression -> [Value] -> Target
                 -> Constraints -> SizeLeft -> [(SizeLeft, PartialExpression)]
searchExpression Unforced vals target constraints sizeleft                  = [] {- TODO -}

searchExpression (If0 c ift iff) vals target constraints sizeleft           = do
  let lval = S.evalExpression c vals
  if lval == 0
    then searchExpression ift vals target constraints sizeleft
    else searchExpression iff vals target constraints sizeleft

searchExpression (Fold over init function) vals target constraints sizeleft = [] {- TODO -}

searchExpression (Op1 op exp) vals target constraints sizeleft = do
  target' <- maybeToList (invertOp1 target op)
  (sizeleft', exp') <- searchExpression exp vals target' constraints sizeleft
  let rv = castConcrete1 exp' (Op1 op) (Concrete . S.Op1 op)
  return (sizeleft', rv)

searchExpression (Op2 op lhs rhs) vals target constraints sizeleft = do
  let lval = S.evalExpression lhs vals
  target' <- invertOp2 target lval op
  (sizeleft', rhs') <- searchExpression rhs vals target' constraints sizeleft
  let rv = castConcrete1 rhs' (Op2 op lhs) (Concrete . S.Op2 op lhs)
  return (sizeleft', rv)

searchExpression c@(Concrete exp) vals target constraints sizeleft = do
  guard $ (S.evalExpression exp vals) `satisfies` target
  return (sizeleft, c)

castConcrete1 :: PartialExpression -> (PartialExpression -> a)
              -> (S.Expression -> a) -> a
castConcrete1 (Concrete s) _ f = f s
castConcrete1 e f _ = f e

invertOp1 :: Target -> Op1 -> Maybe Target
invertOp1 t@Target { targetBits, importantMask } op
  | Not   <- op = Just t { targetBits = complement targetBits }
  | Shl1  <- op = (error "TODO: pruning on lost high bits!") t { targetBits = shiftR targetBits 1,
                      importantMask = shiftR importantMask 1 }
  | Shr1  <- op = (error "TODO: pruning!") t { targetBits = shiftL targetBits 1,
                      importantMask = shiftL importantMask 1 }
  | Shr4  <- op = (error "TODO: pruning!") t { targetBits = shiftL targetBits 4,
                      importantMask = shiftL importantMask 4 }
  | Shr16 <- op = (error "TODO: pruning!") t { targetBits = shiftL targetBits 16,
                      importantMask = shiftL importantMask 16 }


invertOp2 :: Target -> Word64 -> Op2 -> [Target]
invertOp2 t@Target { targetBits, importantMask } lhs op
  | And  <- op = do let rb    = targetBits
                    let ri    = importantMask .&. lhs
                    let prune = targetBits .&. importantMask .&. (complement lhs)
                    guard (prune == 0)
                    return Target { targetBits = rb, importantMask = ri }

  | Or   <- op = do let rb    = targetBits
                    let ri    = importantMask .&. (complement (targetBits .&. lhs))
                    let prune = complement targetBits .&. importantMask .&. lhs
                    guard (prune == 0)
                    return Target { targetBits = rb, importantMask = ri }

  | Xor  <- op = do let rb = targetBits `xor` lhs
                    let ri = importantMask
                    return Target { targetBits = rb, importantMask = ri }

    {- rats, so it turns out the Plus case is .h.a.r.d. -}
    {- super naieve - enumerate all options for unset targetBits -}
    {- TODO: it's probably better here to split, and more often than not signal
     -        that the rhs' should be generated instead of enumerating all 2^64
     -        target values. -}
  | Plus <- op = do value <- enumerateTargets t
                    let rb = value - lhs
                    let ri = 0xFFFFFFFFFFFFFFFF
                    return Target { targetBits = rb, importantMask = ri }

enumerateTargets :: Target -> [Word64]
enumerateTargets Target { targetBits, importantMask }
  = let base = targetBits .&. importantMask
     in foldM build base [0..64]
  where
    build :: Word64 -> Int -> [Word64]
    build a idx
      | testBit importantMask idx = [a]
      | otherwise                 = [setBit a idx, clearBit a idx]
