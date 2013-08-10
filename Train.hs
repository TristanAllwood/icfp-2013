{-# LANGUAGE ViewPatterns #-}
module Train where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.List
import Data.Vector (toList)
import Data.Word
import System.Environment
import System.Random.MWC

import Net
import qualified Directed as D

main :: IO ()
main = do
  net <- newNet

  [read -> size] <- getArgs

  trainingResponse <- trainingRequest net (TrainingRequest { tr_size      = Just size
                                                           , tr_operators = Just None })

  case trainingResponse of
    Left  msg             -> error msg
    Right trainingProblem -> runLoop net
                                     (tp_id trainingProblem)
                                     (tp_size trainingProblem)
                                     (tp_operators trainingProblem)

runLoop :: Net -> String -> Int -> Operators -> IO ()
runLoop net problemId size operators = do
  withSystemRandom $ \gen -> do
    let fixedInputs = [0, 1, complement 0, complement 1]
    randomInputs <- toList <$> uniformVector gen (256 - length fixedInputs)
    let inputs = fixedInputs ++ randomInputs

    evalResponse <- evalRequest net EvalRequest { er_id = problemId, er_arguments = inputs }

    case evalResponse of
      Left msg -> error msg
      Right output -> do
        case (er_outputs output) of
          Just outputs -> do
            let programs = solve (inputs `zip` outputs) size operators

            forM_ programs print

          Nothing -> error "No outputs :("

solve :: [(Word64, Word64)] -> Int -> Operators -> [D.PartialProgram]
solve pairs size operators
  = foldM (\p (i,o) -> D.search p i o)
          (D.initProgram size (op1s operators)
                                (op2s operators)
                                (fold operators)
                                (tfold operators))
           pairs
