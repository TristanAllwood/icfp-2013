{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Syntax where

import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Data.Word

type Var = Int
type Value  = Word64

data Program = Program Expression
  deriving Show

data Expression = Zero
                | One
                | Var !Var
                | If0 !Expression !Expression !Expression
                | Fold !Expression !Expression !Expression
                | Op1 !Op1 !Expression
                | Op2 !Op2 !Expression !Expression
   deriving Show

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
  deriving (Eq, Show)
data Op2 = And | Or | Xor | Plus
  deriving (Eq, Show)

data Constraints
  = Constraints
  { allowedOp1s      :: ![Op1]
  , allowedOp2s      :: ![Op2]
  , op1sLeftToUse    :: ![Op1]
  , op2sLeftToUse    :: ![Op2]
  , sizeAvailable    :: !Int
  , unforcedElements :: !Int
  , if0Allowed       :: !Bool
  , foldAvailable    :: !Bool
  , tfoldAvailable   :: !Bool
  }
  deriving Show

formatProgram :: Program -> String
formatProgram (Program e) = "(lambda (x0) " ++ formatExpression e ++ ")"

formatExpression :: Expression -> String
formatExpression Zero            = "0"
formatExpression One             = "1"
formatExpression (Var v)         = "x" ++ show v
formatExpression (If0 c l r)
  = "(if0 " ++ formatExpression c ++ " " ++ formatExpression l ++ " " ++
                                            formatExpression r ++ ")"
formatExpression (Fold e0 e1 e2)
  = "(fold " ++ formatExpression e0 ++ " " ++ formatExpression e1 ++
    " (lambda (x1 x2) " ++ formatExpression e2 ++ "))"

formatExpression (Op1 op ex)
  = "(" ++ (map toLower (show op)) ++ " " ++ formatExpression ex ++ ")"

formatExpression (Op2 op e0 e1)
  = "(" ++ (map toLower (show op)) ++ " " ++ formatExpression e0 ++ " "
                                           ++ formatExpression e1 ++ ")"

evalExpression :: Expression -> [Value] -> Word64
evalExpression Zero _                  = 0
evalExpression One  _                  = 1
evalExpression (Var v) vals            = vals !! v
evalExpression (If0 e0 e1 e2) vals
  | evalExpression e0 vals == 0        = evalExpression e1 vals
  | otherwise                          = evalExpression e2 vals
evalExpression (Fold base init f) vals
  = let bv = evalExpression base vals
        iv = evalExpression init vals
        bytes = map (extractByte bv) [0..7]
     in foldr combine iv bytes

  where
    extractByte :: Word64 -> Int -> Word64
    extractByte b i = shiftR ((shiftL 0xF (i*8)) .&. b) (i*8)

    combine :: Word64 -> Word64 -> Word64
    combine byte acc = evalExpression f (vals ++ [byte, acc])

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
enumerateConcrete vals sizeTarget constraints @ Constraints { sizeAvailable, foldAvailable }
  = filter (constraintsSatisfiable . fst) refine
  where
    refine = terminals ++ ifs ++ folds ++ op2s ++ op1s
    terminals = [ (constraints { sizeAvailable = sizeAvailable - 1 }, t)
                | sizeTarget == 1
                , t <- (Zero:One:(map Var [0..length vals - 1]))
                ]

    ifs = [ (constraints', If0 e0 e1 e2)
          | if0Allowed constraints
          , sizeTarget >= 4
          , (s0, s1, s2)       <- genSizes3 (sizeTarget - 1)
          , (c0 , e0)          <- enumerateConcrete vals s0 constraints { sizeAvailable = sizeAvailable - 1 }
          , not (isConstant e0)
          , (c1 , e1)          <- enumerateConcrete vals s1 c0
          , (constraints', e2) <- enumerateConcrete vals s2 c1
          ]

    folds = [ (constraints', Fold e0 e1 e2)
            | foldAvailable
            , sizeTarget >= 4
            , (s0, s1, s2) <- genSizes3 (sizeTarget - 1)
            , (c0, e0) <- enumerateConcrete vals s0 constraints { sizeAvailable = sizeAvailable - 1, foldAvailable = False }
            , (c1, e1) <- enumerateConcrete vals s1 c0
            , (constraints', e2) <- enumerateConcrete (vals ++ [0,0]) s2 c1
            ]

    op1s = [ (constraints', Op1 op e0)
           | sizeTarget >= 2
           , (c0, op) <- enumerateOp1  constraints { sizeAvailable = sizeAvailable - 1 }
           , (constraints', e0) <- enumerateConcrete vals (sizeTarget - 1) c0
           ]

    op2s = [ (constraints', Op2 op e0 e1)
           | sizeTarget >= 3
           , (c0, op) <- enumerateOp2 constraints { sizeAvailable = sizeAvailable - 1 }
           , (s0, s1)           <- genSizes2Triangle (sizeTarget - 1)
           , (c1, e0)           <- enumerateConcrete vals s0 c0
           , (constraints', e1) <- enumerateConcrete vals s1 c1
           ]

constraintsSatisfiable :: Constraints -> Bool
constraintsSatisfiable Constraints { op1sLeftToUse, op2sLeftToUse, sizeAvailable, unforcedElements } = (length op1sLeftToUse + length op2sLeftToUse) <= (sizeAvailable + unforcedElements)
--constraintsSatisfiable = const True

enumerateOp1 :: Constraints -> [(Constraints, Op1)]
enumerateOp1 constraints@Constraints { allowedOp1s, op1sLeftToUse }
  = filter (constraintsSatisfiable . fst)
           [ (constraints { op1sLeftToUse = op1sLeftToUse \\ [op] }, op) | op <- allowedOp1s ]

enumerateOp2 :: Constraints -> [(Constraints, Op2)]
enumerateOp2 constraints@Constraints { allowedOp2s, op2sLeftToUse }
  = filter (constraintsSatisfiable . fst)
           [ (constraints { op2sLeftToUse = op2sLeftToUse \\ [op] }, op) | op <- allowedOp2s ]

genSizes3 :: Int -> [(Int, Int, Int)]
genSizes3 supply = [ (v0, v1, v2) | v0 <- [1..(supply - 2)],
                                    v1 <- [1..(supply - v0)],
                                    v2 <- [1..(supply - (v0 + v1))] ]

genSizes2Triangle :: Int -> [(Int, Int)]
genSizes2Triangle supply = [ (v0, v1) | v0 <- [1..(supply - 1)],
                                        v1 <- [1..v0] ]

isConstant :: Expression -> Bool
isConstant Zero         = True
isConstant One          = True
isConstant (Var _)      = False
isConstant (If0 _ l r)  = isConstant l && isConstant r
isConstant (Fold _ _ _) = False
isConstant (Op1 _ e)    = isConstant e
isConstant (Op2 _ l r)  = isConstant l && isConstant r
