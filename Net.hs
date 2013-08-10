module Net where

import System.IO
import Network.HTTP

import Data.Aeson

data Net
  = Net
  { key :: String
  , logFile :: Handle
  }

newNet :: IO Net
newNet = do
  key <- readFile "key"
  logFile <- openFile "log" AppendMode
  return Net { .. }

data TrainingRequest
  = TrainingRequest
  { tr_size :: Maybe Int
  , tr_operators :: Maybe TrainingOp
  }

data TrainingOp
  = None | TFold | Fold

instance ToJSON TrainingOp where
  toJSON None = emptyArray
  toJSON TFold = toJSON ["tfold"]
  toJSON Fold  = toJSON ["fold"]

data TrainingProblem
  = TrainingProblem
  { tp_challenge :: Program
  , tp_id :: String
  , tp_size :: Int
  , tp_operators :: Operators
  }

data Operators
  = Operators
  { op1s :: [Op1]
  , op2s :: [Op2]
  , fold :: Bool
  , tfold :: Bool
  }

mkUrl :: Net -> String -> String
mkUrl net page
  = "http://icfpc2013.cloudapp.net/" ++ page ++ "?auth=" ++ (key net) ++ "vpsH1H"


trainingRequest :: Net -> TrainingRequest -> IO (Either TrainingProblem String)
trainingRequest net req = do
  let json = object [ "size" .= tr_size req,
                      "operators" .= tr_operators res
                    ]
  let url = mkUrl net "train"

  hPutStrLn ("train: " ++ url ++ " : " ++ show json)

  resp <- simpleHTTP (postRequestWithBody "application/json" (show json)

  print resp

