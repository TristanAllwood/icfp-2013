{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Net where

import Control.Applicative
import Control.Concurrent
import Data.Aeson
import Text.Printf
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Data.Word
import Data.Char
import Data.Maybe
import Control.Monad
import Data.String
import Network.HTTP
import System.IO

import qualified Syntax as S

data Net
  = Net
  { key :: String
  , logFile :: Handle
  }

newNet :: IO Net
newNet = do
  key <- init <$> readFile "key"
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
  toJSON None  = toJSON ([] :: [String])
  toJSON TFold = toJSON (["tfold"] :: [String])
  toJSON Fold  = toJSON (["fold"] :: [String])

data TrainingProblem
  = TrainingProblem
  { {- tp_challenge :: S.Program -}
  {-,-} tp_id :: String
  , tp_size :: Int
  , tp_operators :: Operators
  }

instance FromJSON TrainingProblem where
  parseJSON (Object v) = TrainingProblem <$>
                          v .: "id"      <*>
                          v .: "size"    <*>
                          v .: "operators"
  parseJSON _ = mzero

data Operators
  = Operators
  { op1s :: [S.Op1]
  , op2s :: [S.Op2]
  , if0 :: Bool
  , fold :: Bool
  , tfold :: Bool
  , bonus :: Bool
  }
  deriving Show

instance FromJSON Operators where
  parseJSON a
    | Success strs <- fromJSON a
    = let fold = "fold" `elem` strs
          tfold = "tfold" `elem` strs
          if0 = "if0" `elem` strs
          bonus = "bonus" `elem` strs
          op1s = mapMaybe (flip lookup op1_ps) strs
          op2s = mapMaybe (flip lookup op2_ps) strs
       in return Operators { op1s, op2s, if0, fold, tfold, bonus}
    | otherwise = mzero

    where
      op1_ps = [ ((map toLower (show op)), op) | op <- [S.Not, S.Shl1, S.Shr1, S.Shr4, S.Shr16]]
      op2_ps = [ ((map toLower (show op)), op) | op <- [S.And, S.Or, S.Xor, S.Plus]]

data EvalRequest
  = EvalRequest
  { er_id :: String
  , er_arguments :: [Word64]
  }

data EvalResponse
  = EvalResponse
  { er_status :: String
  , er_outputs :: Maybe [Word64]
  , er_message :: Maybe String
  }

instance FromJSON EvalResponse where
  parseJSON (Object v)
    = EvalResponse                              <$>
        v .: "status"                           <*>
        (fmap (map read) <$> (v .:? "outputs"))  <*>
        v .:? "message"
  parseJSON _ = mzero

data GuessRequest
  = GuessRequest
  { gr_id      :: String
  , gr_program :: S.Program
  }

data GuessResponse
  = GuessResponse
  { gr_status :: GuessResponseStatus
  , gr_values :: Maybe [Word64]
  , gr_message :: Maybe String
  }

data GuessResponseStatus = Win | Mismatch | Error
  deriving Show

instance FromJSON GuessResponseStatus where
  parseJSON (String txt)
    | txt == "win"      = return Win
    | txt == "mismatch" = return Mismatch
    | txt == "error"    = return Net.Error
  parseJSON _ = mzero

instance FromJSON GuessResponse where
  parseJSON (Object v)
    = GuessResponse                             <$>
        v .: "status"                           <*>
        (fmap (map read) <$> (v .:? "values"))  <*>
        v .:? "message"

  parseJSON _ = mzero

mkUrl :: Net -> String -> String
mkUrl net page
  = "http://icfpc2013.cloudapp.net/" ++ page ++ "?auth=" ++ (key net) ++ "vpsH1H"

netRequest :: FromJSON a => Net -> Value -> String -> IO a
netRequest net json page = do
  let url = mkUrl net page
  hPutStrLn (logFile net) (page ++ ": " ++ url ++ " : " ++ (unpack $ encode json))
  resp <- simpleHTTP (postRequestWithBody url "application/json" (unpack $ encode json))

  print resp
  hPrint (logFile net) resp

  respCode <- getResponseCode resp

  case respCode of
    (4,2,9) -> do
      putStrLn "429 - waiting."
      threadDelay 20000000
      putStrLn "woken up."
      netRequest net json page

    (2,0,0) -> do
      body <- getResponseBody resp
      hPutStrLn (logFile net) body
      hFlush (logFile net)

      let jsonValue = decode (pack body)
      case jsonValue of
        Just tp -> return tp
        Nothing -> error "Json decode failed!?"
    _       -> error "Response failed unexpectedly."

trainingRequest :: Net -> TrainingRequest -> IO TrainingProblem
trainingRequest net req = do
  let json = object $ maybe [] (\s -> [ "size" .= s]) (tr_size req) ++
                      maybe [] (\s -> [ "operators" .= s ]) (tr_operators req)

  netRequest net json "train"

evalRequest :: Net -> EvalRequest -> IO EvalResponse
evalRequest net req = do
  let textified_arguments = map (printf "0x%016X" :: Word64 -> String) (er_arguments req)
  let json = object $ [ "id"        .= er_id req
                      , "arguments" .= textified_arguments
                      ]

  netRequest net json "eval"

guessRequest :: Net -> GuessRequest -> IO GuessResponse
guessRequest net req = do
  let code_program = S.formatProgram (gr_program req)
  let json = object $ [ "id"        .= gr_id req
                      , "program"   .= code_program
                      ]
  netRequest net json "guess"
