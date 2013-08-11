{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Solver where

import Control.Applicative
import Control.Concurrent.MSampleVar
import Control.Monad
import Data.Aeson hiding (Error)
import Data.Bits
import Data.IORef
import Data.List
import Data.Maybe
import Data.Vector (Vector, toList, freeze)
import Data.Vector.Mutable (IOVector)
import Data.Word
import Syntax
import System.Random.MWC
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Net
import qualified Directed as D
import Syntax (formatProgram)

data DebugMessage
  = DebugMessage
  { seeds       :: !(Vector (Word64, Word64))
  , mismatches  :: ![(Word64, Word64)]
  , programs    :: !(Vector (Maybe Program))
  , counts      :: !(Vector Integer)
  , finished    :: !Bool
  }

instance ToJSON DebugMessage where
  toJSON DebugMessage { seeds,
                        mismatches,
                        programs,
                        counts,
                        finished
                      }
    = object [ "seeds"      .= seeds
             , "mismatches" .= mismatches
             , "programs"   .= fmap (fmap formatProgram) programs
             , "counts"     .= counts
             , "finished"   .= finished
             ]

data DebugInformation
  = DebugInformation
  { di_seeds      :: !(Vector (Word64, Word64))
  , di_mismatches :: !(IORef [(Word64, Word64)])
  , di_programs   :: !(IOVector (Maybe Program))
  , di_counts     :: !(IOVector Integer)
  }

buildDebugMessage :: DebugInformation -> Bool -> IO DebugMessage
buildDebugMessage di finished = do
  let seeds = di_seeds di
  mismatches <- readIORef $ di_mismatches di
  programs   <- freeze $ di_programs di
  counts     <- freeze $ di_counts di
  return DebugMessage { .. }

newDebugInformation :: [Word64] -> [Word64] -> IO DebugInformation
newDebugInformation inputs outputs = do
  let di_seeds = V.fromList (inputs `zip` outputs)
  di_mismatches <- newIORef []
  di_programs <- VM.replicate (V.length di_seeds) Nothing
  di_counts <- VM.replicate (V.length di_seeds) 0

  return DebugInformation { .. }

updateDebugInformation :: DebugInformation -> Int -> Program -> IO ()
updateDebugInformation di idx program = do
  VM.write (di_programs di) idx (Just program)
  VM.read (di_counts di) idx >>= VM.write (di_counts di) idx . succ

sendDebugMessage :: MSampleVar DebugMessage -> DebugInformation -> Bool -> IO ()
sendDebugMessage sampleVar di finished = do
  debugMessage <- buildDebugMessage di finished
  writeSV sampleVar debugMessage


runLoop :: Net -> String -> Int -> Operators -> IO ()
runLoop net problemId size operators
  = runDebugLoop net Nothing problemId size operators

runDebugLoop :: Net -> Maybe (MSampleVar DebugMessage) -> String -> Int -> Operators -> IO ()
runDebugLoop net msample problemId size operators = do

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

        let noProgram = error "Sorry, I can't help you :("

        let gotAProgram p ps looper hookDone = do
              let c_program = D.concretizeProgram p
              putStrLn $ "attempting: " ++ (formatProgram c_program)

              guessResponse <- guessRequest net GuessRequest { gr_id = problemId,
                                                               gr_program = c_program }

              case (gr_status guessResponse) of
                Win      -> do
                  putStrLn "Woohoo!"
                  hookDone

                Mismatch -> do
                  let Just [input, output, urk] = gr_values guessResponse
                  putStrLn $ "Mismatch " ++ show [input, output, urk]
                  {- add to mismatches -}
                  looper [(input,output)] ps

                Error    -> do
                  putStrLn (fromJust $ gr_message guessResponse)
                  hookDone

        let loop pairs programs = do
              let programs' = solve pairs programs
              case programs' of
                [] -> noProgram
                (p:ps) -> gotAProgram p ps loop (return ())

        let debugLoop samplevar di pairs programs = do
              programs' <- solveDebug samplevar di pairs programs
              case programs' of
                Nil -> noProgram
                (Cons p ps) -> gotAProgram p ps
                                           (\pairs -> (>>= debugLoop samplevar di pairs))
                                           (sendDebugMessage samplevar di True)

        case msample of
          Nothing     -> loop pairs programs
          Just sample -> do
            di <- newDebugInformation inputs outputs
            debugLoop sample di pairs (io_fromList programs)

data IOStream a
  = Nil
  | Cons !a (IO (IOStream a))

io_append :: IOStream a -> IO (IOStream a) -> IO (IOStream a)
io_append Nil cont           = cont
io_append (Cons x rest) cont = return (Cons x (rest >>= (`io_append` cont)))

io_fromList :: [a] -> IOStream a
io_fromList []     = Nil
io_fromList (x:xs) = Cons x (return $ io_fromList xs)

solveDebug :: MSampleVar DebugMessage -> DebugInformation -> [(Word64, Word64)] -> IOStream D.PartialProgram -> IO (IOStream D.PartialProgram)
solveDebug sampleVar di pairs programs = solveDebugLoop sampleVar di pairs 0 programs (return Nil)

solveDebugLoop :: MSampleVar DebugMessage -> DebugInformation -> [(Word64, Word64)] -> Int -> IOStream D.PartialProgram -> IO (IOStream D.PartialProgram) -> IO (IOStream D.PartialProgram)
solveDebugLoop _         di []              _   ps     cont = ps `io_append` cont
solveDebugLoop sampleVar di io@(pair:pairs) idx (Cons p io_ps) cont = do
  nexts <- oneInput sampleVar di idx pair p
  let io_nexts = io_fromList nexts
  solveDebugLoop sampleVar di pairs (idx+1) io_nexts $ io_ps >>= \ps -> solveDebugLoop sampleVar di io idx ps cont
solveDebugLoop _         _  _               _   Nil cont = cont

oneInput :: MSampleVar DebugMessage -> DebugInformation -> Int -> (Word64, Word64) -> D.PartialProgram -> IO [D.PartialProgram]
oneInput sampleVar di index (input, output) program = do
  updateDebugInformation di index (D.concretizeProgram program)
  sendDebugMessage sampleVar di False
  return $ D.search program input output


solve :: [(Word64, Word64)] -> [D.PartialProgram] -> [D.PartialProgram]
solve pairs programs
  = concatMap (\program -> foldM (\p (i,o) -> D.search p i o) program pairs) programs

