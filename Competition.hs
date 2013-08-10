{-# LANGUAGE OverloadedStrings #-}
module Competition where

import System.Environment
import Net
import Solver

import Data.Aeson
import Control.Applicative
import Prelude hiding (id, putStrLn, readFile)
import Data.ByteString.Lazy.Char8 hiding (filter)
import Control.Monad


main :: IO ()
main = do
  net <- newNet

  [problemId, problemsfile] <- getArgs

  Just problems <- decode <$> readFile problemsfile

  let [thisProblem] = filter ((== problemId) . id) problems

  when ((Net.fold $ operators thisProblem) || (Net.tfold $ (operators thisProblem))) $ do
    error "Don't be stupid"

  print thisProblem

  putStrLn "Are you absolutely sure you want to attempt this for real? (type 'yes')"
  input <- getLine

  when (input == "yes") $ do
    putStrLn "Starting - good luck!"

    runLoop net (id thisProblem)
                (size thisProblem)
                (operators thisProblem)


data GivenProblem
  = GivenProblem
  { id :: String
  , size :: Int
  , operators :: Operators
  , solved :: Maybe Bool
  , timeLeft :: Maybe Int
  }
  deriving Show

instance FromJSON GivenProblem where
  parseJSON (Object v) = GivenProblem       <$>
                          v .: "id"         <*>
                          v .: "size"       <*>
                          v .: "operators"  <*>
                          v .:? "solved"    <*>
                          v .:? "timeLeft"
  parseJSON _ = mzero
