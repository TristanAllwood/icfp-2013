{-# LANGUAGE ViewPatterns #-}
module Train where

import System.Environment
import Net
import Solver

main :: IO ()
main = do
  net <- newNet

  [read -> size] <- getArgs

  trainingResponse <- trainingRequest net (TrainingRequest { tr_size      = Just size
                                                           , tr_operators = Just Fold })

  runLoop net (tp_id trainingResponse)
              (tp_size trainingResponse)
              (tp_operators trainingResponse)

