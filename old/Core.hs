{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Core where

data Program a = Program !(Expression a)

deriving instance (Show (a Op2), Show (a Op1), Show (a Terminal)) => Show (Program a)

data Expression a = Terminal !(a Terminal)
                  | If0 (Expression a) (Expression a) (Expression a)
                  | Fold (Expression a) (Expression a) (Expression a)
                  | Op1 (a Op1) (Expression a)
                  | Op2 (a Op2) (Expression a) (Expression a)

deriving instance (Show (a Op2), Show (a Op1), Show (a Terminal)) => Show (Expression a)

data Terminal = Zero | One | Var Var
  deriving Show
data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
  deriving Show
data Op2 = And | Or | Xor | Plus
  deriving Show

newtype Var = Variable Integer
  deriving (Show, Num)

data Skeleton a = Skeleton
  deriving Show

newtype Complete a = Complete a
  deriving Show

class Enumerate a where
  enumerate :: Integer -> [a]

instance (EnumerateSimple (a Terminal),
          EnumerateSimple (a Op1),
          EnumerateSimple (a Op2),
          Enumerate (Expression a)) => Enumerate (Program a) where

  enumerate limit = [ Program p | p <- (enumerate (limit - 1)), limit >= 1 ]

instance (EnumerateSimple (a Terminal),
          EnumerateSimple (a Op1),
          EnumerateSimple (a Op2)) => Enumerate (Expression a) where

  enumerate size
    = [ Terminal x    | size == 1, x <- enumerateSimple ] ++
      [ If0 e0 e1 e2  | size >= 4,
                        (s0, s1, s2) <- genSizes3 (size - 1),
                        e0 <- enumerate s0,
                        e1 <- enumerate s1,
                        e2 <- enumerate s2
                        ] ++
      [ Fold e0 e1 e2 | size >= 5,
                        (s0, s1, s2) <- genSizes3 (size - 1),
                        e0 <- enumerate s0,
                        e1 <- enumerate s1,
                        e2 <- enumerate s2
                        ] ++
      [ Op1 op e0     | size >= 2,
                        op <- enumerateSimple,
                        e0 <- enumerate (size - 1)
                        ] ++
      [ Op2 op e0 e1  | size >= 3,
                        op <- enumerateSimple,
                        (s0, s1) <- genSizes2Triangle (size - 1),
                        e0 <- enumerate s0,
                        e1 <- enumerate s1
                        ]


class EnumerateSimple a where
  enumerateSimple :: [a]

instance EnumerateSimple (Skeleton a) where
  enumerateSimple = [Skeleton]

genSizes3 :: Integer -> [(Integer, Integer, Integer)]
genSizes3 supply = [ (v0, v1, v2) | v0 <- [1..(supply - 2)],
                                    v1 <- [1..(supply - v0)],
                                    v2 <- [1..(supply - (v0 + v1))] ]

genSizes2Triangle :: Integer -> [(Integer, Integer)]
genSizes2Triangle supply = [ (v0, v1) | v0 <- [1..(supply - 1)],
                                        v1 <- [1..v0] ]


eval :: Program Complete -> Word64 -> Word64
eval (Program exp) var = evalExp [(0, var)] exp

evalExp :: Expression Complete -> [(Integer, Word64)] -> Word64
evalExp (Terminal (Complete Zero))      = return 0
evalExp (Terminal (Complete One))       = return 1
evalExp (Terminal (Complete (Var v)))   = asks (lookup v)
evalExp (If0 c ifT ifF)                 = do r <- evalExp c
                                             if r == 0 then evalExp ifT
                                                       else evalExp ifF
evalExp (Fold e0 e1 e2)                 = do


evalExp (Op1 (Complete op) e)
evalExp (Op2 (Complete op) e1 e2)
