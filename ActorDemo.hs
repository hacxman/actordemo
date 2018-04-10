module Main where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import System.IO
import System.Exit

data Msg = Inc | Dec | Exit

actor ch num = do
  print num
  mesg <- atomically $ readTChan ch
  case mesg of
    Exit -> return ()
    Inc -> actor ch (num+1)
    Dec -> actor ch (num-1)


fireKey chan 'i' = atomically $ writeTChan chan Inc
fireKey chan 'I' = atomically $ writeTChan chan Inc
fireKey chan 'd' = atomically $ writeTChan chan Dec
fireKey chan 'D' = atomically $ writeTChan chan Dec
fireKey chan 'q' = (atomically $ writeTChan chan Exit) >> exitSuccess
fireKey chan _  = return ()

main = do
    putStrLn "i/I to increase; d/D do decrease; q to quit"
    chan <- atomically $ newTChan
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    
    thid <- forkIO $ actor chan 0

    forever (getChar >>= (fireKey chan))
