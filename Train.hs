module Train where

import Net
import System.Environment

main :: IO ()
main = do
  net <- newNet

  [read -> size] <- getArgs

  trainingResponse <- net trainingRequest (TrainingRequest { tr_size      = Just size
                                                           , tr_operators = Just None })

  case trainingResponse of
    Left  msg             -> error msg
    Right trainingProblem -> runLoop (tp_id trainingProblem)
                                     (tp_size trainingProblem)
                                     (tp_operators trainingProblem)

runLoop :: String -> Int -> Operators -> IO ()
runLoop = error "TODO"

