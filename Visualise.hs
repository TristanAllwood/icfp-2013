{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Visualise where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MSampleVar
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8
import Data.Maybe
import Network
import System.Environment
import System.IO (Handle, hSetBuffering, BufferMode(LineBuffering))
import qualified Data.ByteString as B

import Net
import Solver


main :: IO ()
main = withSocketsDo $ do
  [fromInteger . read -> port] <- getArgs
  socket <- listenOn (PortNumber port)

  forever $ do
    (hdl, host, port) <- accept socket
    forkIO $ runClient hdl


data RunRequest
  = RunRequest
  { rr_file :: Maybe String
  , rr_id   :: Maybe String
  , rr_size :: Maybe Int
  }

instance FromJSON RunRequest where
  parseJSON (Object v)
    = RunRequest      <$>
        v .:? "file"  <*>
        v .:? "id"    <*>
        v .:? "size"

runClient :: Handle -> IO ()
runClient handle = do
  net <- newNet

  hSetBuffering handle LineBuffering
  forever $ do
    request <- (fromChunks . return) <$> B.hGetLine handle
    let run_request = fromMaybe (error "No parse") (decode request)

    case (rr_size run_request) of
      Just size -> do
        {- Training mode -}
        trainingResponse <- trainingRequest net (TrainingRequest { tr_size      = Just size
                                                                 , tr_operators = Just TFold })


        sampleVar <- newEmptySV

        forkIO $ do
          runDebugLoop net (Just sampleVar) (tp_id trainingResponse)
                                            (tp_size trainingResponse)
                                            (tp_operators trainingResponse)

        let loop = do debugMessage <- readSV sampleVar
                      hPutStrLn handle (encode debugMessage)
                      unless (finished debugMessage) loop
        loop

      Nothing   -> do
        let (Just file, Just id) = (rr_file run_request, rr_size run_request)
        {- real mode -}
        return ()
