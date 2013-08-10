{-# LANGUAGE ViewPatterns #-}
module Train where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.List
import Data.Maybe
import Data.Vector (toList)
import Data.Word
import System.Environment
import System.Random.MWC

import Net
import qualified Directed as D
import Syntax (formatProgram)

main :: IO ()
main = do
  net <- newNet

  [read -> size] <- getArgs

  trainingResponse <- trainingRequest net (TrainingRequest { tr_size      = Just size
                                                           , tr_operators = Just None })

  runLoop net (tp_id trainingResponse)
              (tp_size trainingResponse)
              (tp_operators trainingResponse)

runLoop :: Net -> String -> Int -> Operators -> IO ()
runLoop net problemId size operators = do
  withSystemRandom $ \gen -> do
    let fixedInputs = [0, 1, complement 0, complement 1]
    randomInputs <- toList <$> uniformVector gen (256 - length fixedInputs)
    let inputs = fixedInputs ++ randomInputs

    evalResponse <- evalRequest net EvalRequest { er_id = problemId, er_arguments = inputs }

    case (er_outputs evalResponse) of
      Nothing -> error "No outputs :("
      Just outputs -> do

        let programs = [(D.initProgram size (op1s operators)
                                            (op2s operators)
                                            (if0 operators)
                                            (fold operators)
                                            (tfold operators))]
        let pairs = inputs `zip` outputs

        let loop pairs programs = do
              let programs' = solve pairs programs
              case programs' of
                [] -> error "Sorry, I can't help you :("
                (p:ps) -> do
                  let c_program = D.concretizeProgram p
                  putStrLn $ "attempting: " ++ (formatProgram c_program)

                  guessResponse <- guessRequest net GuessRequest { gr_id = problemId,
                                                                   gr_program = c_program }

                  case (gr_status guessResponse) of
                    Win      -> do
                      putStrLn "Woohoo!"
                      {- Woot. -}

                    Mismatch -> do
                      let Just [input, output, urk] = gr_values guessResponse
                      putStrLn $ "Mismatch " ++ show [input, output, urk]
                      loop [(input,output)] ps

                    Error    -> do
                      putStrLn (fromJust $ gr_message guessResponse)
                      {- TODO: keep trying? -}

        loop pairs programs


solve :: [(Word64, Word64)] -> [D.PartialProgram] -> [D.PartialProgram]
solve pairs programs
  = concatMap (\program -> foldM (\p (i,o) -> D.search p i o) program pairs) programs
