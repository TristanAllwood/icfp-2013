{-# LANGUAGE RecordWildCards #-}
module PlusInts where

import Control.Monad
import Text.Printf
import Data.Word

main :: IO ()
main = do
  forM_ allComputations (putStrLn . prettyPrint)


prettyPrint :: Computation -> String
prettyPrint Computation { .. }
  = printf "tI: %02x\ntB: %02x\nlB: %02x\nrB: %02x\nrI: %02x\n" tI tB lB rB rI


data Computation
  = Computation
  { tI  :: Word8
  , tB  :: Word8
  , lB  :: Word8
  , rB  :: Word8
  , rI  :: Word8
  , bad :: Bool
  }

allWord8s :: [Word8]
allWord8s = [minBound .. maxBound]

allComputations :: [Computation]
allComputations = [ Computation { .. }
                  | tI <- allWord8s
                  , tB <- allWord8s
                  , lB <- allWord8s
                  , let rB = tI - lB
                  , let rI = 0
                  , let bad = False
                  ]

