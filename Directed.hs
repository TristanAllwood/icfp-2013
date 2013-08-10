{-# LANGUAGE NamedFieldPuns #-}
module Directed where

import Control.Monad
import Data.Bits
import Data.Int
import Data.Maybe
import Data.Word
import Syntax (Var, Op1(..), Op2(..), Value, enumerateConcrete,
               constraintsSatisfiable, Constraints(..), enumerateOp1,
               enumerateOp2)
import qualified Syntax as S
import Debug.Trace

data PartialProgram = PartialProgram PartialExpression Constraints
  deriving Show

data PartialExpression = Unforced
                       | If0 S.Expression PartialExpression PartialExpression
                       | Fold PartialExpression PartialExpression PartialExpression
                       | Op1 Op1 PartialExpression
                       | Op2 Op2 S.Expression PartialExpression
                       | Concrete !S.Expression
   deriving Show

type SizeLeft = Int8

type Input  = Word64
type Output = Word64

data Target
  = Target
  { targetBits    :: !Word64
  , importantMask :: !Word64
  }

concretize :: PartialExpression -> S.Expression
concretize Unforced         = S.Zero
concretize (If0 c l r)      = S.If0 c (concretize l) (concretize r)
concretize (Fold e0 e1 e2)  = S.Fold (concretize e0) (concretize e1) (concretize e2)
concretize (Op1 o e)        = S.Op1 o (concretize e)
concretize (Op2 o e0 e1)    = S.Op2 o e0 (concretize e1)
concretize (Concrete e)     = e

concretizeProgram :: PartialProgram -> S.Program
concretizeProgram (PartialProgram exp _) = S.Program (concretize exp)

satisfies :: Word64 -> Target -> Bool
satisfies value (Target { targetBits, importantMask })
  = (value .&. importantMask) == (targetBits .&. importantMask)

initProgram :: Int -> [Op1] -> [Op2] -> Bool -> Bool -> Bool -> PartialProgram
initProgram size op1s op2s if0Allowed foldAvailable tfoldAvailable
  = PartialProgram Unforced Constraints { allowedOp1s       = op1s
                                        , allowedOp2s       = op2s
                                        , op1sLeftToUse     = op1s
                                        , op2sLeftToUse     = op2s
                                        , if0Allowed        = if0Allowed
                                        , sizeAvailable     = size - 1
                                        , unforcedElements  = 1
                                        , foldAvailable     = foldAvailable
                                        , tfoldAvailable    = tfoldAvailable
                                        }

search :: PartialProgram -> Input -> Output -> [PartialProgram]
search (PartialProgram exp constraints) input output
  = map (uncurry (flip PartialProgram)) (searchExpression exp [input] (Target output (complement 0)) constraints)


searchExpression :: PartialExpression -> [Value] -> Target
                 -> Constraints -> [(Constraints, PartialExpression)]

searchExpression Unforced vals target constraints @ Constraints { sizeAvailable, unforcedElements } = do
  (constraints', e) <- refine
  searchExpression e vals target constraints'
  where
    refine = filter (constraintsSatisfiable . fst) (primitives ++ ifs ++ folds ++ op1s ++ op2s)

    primitives = [ (constraints', Concrete x)
                 | (constraints',x) <- enumerateConcrete vals 1 constraints { sizeAvailable = sizeAvailable + 1, unforcedElements = unforcedElements - 1 }
                 , let v = S.evalExpression x vals
                 , v `satisfies` target
                 ]

    ifs =        [ (constraints', If0 e Unforced Unforced)
                 | if0Allowed constraints
                 , sizeAvailable + 1 >= 4
                 , s0 <- [1 .. sizeAvailable + 1 - 3]
                 , (constraints', e) <- enumerateConcrete vals s0 constraints { sizeAvailable = sizeAvailable + 1 - 3, unforcedElements = unforcedElements - 1 + 2 }
                 ]

    folds =      [
                 ]

    op1s =       [ (constraints', Op1 op Unforced)
                 | sizeAvailable + 1 >= 2
                 , (constraints', op) <- enumerateOp1 constraints { sizeAvailable = sizeAvailable + 1 - 2, unforcedElements = unforcedElements - 1 + 1 }
                 ]

    op2s =       [ (constraints'', Op2 op exp Unforced)
                 | sizeAvailable + 1 >= 3
                 , (constraints', op) <- enumerateOp2 constraints { sizeAvailable = sizeAvailable + 1 - 2, unforcedElements = unforcedElements - 1 + 1 }
                 , s0 <- [1 .. S.sizeAvailable constraints' ]
                 , (constraints'', exp) <- enumerateConcrete vals s0 constraints'
                 ]

searchExpression (If0 c ift iff) vals target constraints = do
  let lval = S.evalExpression c vals
  if lval == 0
  then do
    (constraints', ift') <- searchExpression ift vals target constraints
    let rv = castConcrete2 ift' iff (If0 c) ((Concrete .) . S.If0 c)
    return (constraints', rv)
  else do
    (constraints', iff') <- searchExpression iff vals target constraints
    let rv = castConcrete2 ift iff' (If0 c) ((Concrete .) . S.If0 c)
    return (constraints', rv)

searchExpression (Fold over init function) vals target constraints = [] {- TODO -}

searchExpression (Op1 op exp) vals target constraints = do
  target' <- maybeToList (invertOp1 target op)
  (constraints', exp') <- searchExpression exp vals target' constraints
  let rv = castConcrete1 exp' (Op1 op) (Concrete . S.Op1 op)
  return (constraints', rv)

searchExpression (Op2 op lhs rhs) vals target constraints = do
  let lval = S.evalExpression lhs vals
  target' <- invertOp2 target lval op
  (constraints', rhs') <- searchExpression rhs vals target' constraints
  let rv = castConcrete1 rhs' (Op2 op lhs) (Concrete . S.Op2 op lhs)
  return (constraints', rv)

searchExpression c@(Concrete exp) vals target constraints = do
  guard $ (S.evalExpression exp vals) `satisfies` target
  return (constraints, c)

castConcrete1 :: PartialExpression -> (PartialExpression -> a)
              -> (S.Expression -> a) -> a
castConcrete1 (Concrete s) _ f = f s
castConcrete1 e f _ = f e

castConcrete2 :: PartialExpression -> PartialExpression -> (PartialExpression -> PartialExpression -> a)
              -> (S.Expression -> S.Expression -> a) -> a
castConcrete2 (Concrete l) (Concrete r) _ f = f l r
castConcrete2 l r f _ = f l r

invertOp1 :: Target -> Op1 -> Maybe Target
invertOp1 t@Target { targetBits, importantMask } op
  | Not   <- op = Just t { targetBits = complement targetBits }

  | Shl1  <- op = do guard ((importantMask .&. bit 0 .&. targetBits) == 0)
                     return t { targetBits = shiftR targetBits 1,
                                importantMask = shiftR importantMask 1 }
  | Shr1  <- op = do guard ((importantMask .&. bit 63 .&. targetBits) == 0)
                     return t { targetBits = shiftL targetBits 1,
                                importantMask = shiftL importantMask 1 }
  | Shr4  <- op = do guard ((importantMask .&. 0xF000000000000000 .&. targetBits) == 0)
                     return t { targetBits = shiftL targetBits 4,
                                importantMask = shiftL importantMask 4 }
  | Shr16 <- op = do guard ((importantMask .&. 0xFFFF000000000000 .&. targetBits) == 0)
                     return t { targetBits = shiftL targetBits 16,
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
  | (64 - popCount importantMask) < maxTargetSplits = let base = targetBits .&. importantMask
                                                       in (foldM build base [0..63])
  | otherwise = [ targetBits .&. importantMask
                , (targetBits .&. importantMask) .|. (complement 0 .&. complement importantMask)
                , targetBits
                ]
  where
    maxTargetSplits = 20 {- really not sure what's better here!? -}

    build :: Word64 -> Int -> [Word64]
    build a idx
      | testBit importantMask idx = [a]
      | otherwise                 = [setBit a idx, clearBit a idx]


