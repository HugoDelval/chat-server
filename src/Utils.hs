module Utils where

import Network.BSD
import System.IO
import System.Directory
import Control.Exception
import Control.Monad.Fix (fix)
import Control.Concurrent
import Data.IP
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (mallocBytes, free)

getWaitBySocket :: Int
getWaitBySocket = 80000 -- 80ms

nonBlockingRead :: Handle -> String -> IO String
nonBlockingRead hdl currentS = do
  threadDelay getWaitBySocket
  buf <- mallocBytes 4096 :: IO (Ptr CChar)
  nbRead <- hGetBufNonBlocking hdl buf 4096 
  request <- peekCStringLen (buf, nbRead)
  free buf
  if nbRead == 0
    then return currentS
    else do
      next <- nonBlockingRead hdl (currentS ++ request)
      return next

waitForInput :: Handle -> Chan Bool -> Int -> IO (Bool, Bool, String)
waitForInput hdl killedChan waitingTime = do
  let socketTimedOut = waitingTime > getWaitBySocket*100 -- 100*80ms = 8000ms = 8s
  if socketTimedOut 
    then (return (False, True, ""))
    else do
      stillAlive <- isEmptyChan killedChan
      if stillAlive
        then do
          request <- handle (\(SomeException _) -> return "") $ fix $ (return $ nonBlockingRead hdl "")
          if null request
            then do
              res <- waitForInput hdl killedChan (waitingTime + getWaitBySocket)
              return res
            else do
              return (False, False, request)
        else return (True, False, [])

sendResponse :: Handle -> String -> IO ()
sendResponse hdl resp = do
    hSetBuffering hdl $ BlockBuffering $ Just (length resp)
    hPutStr hdl resp

sendError :: Handle -> Int -> String -> IO ()
sendError hdl errorCode errorString = sendResponse hdl $ "ERROR_CODE: " ++ (show errorCode) ++ "\nERROR_DESCRIPTION: " ++ errorString

getHostNameIfDockerOrNot :: IO String
getHostNameIfDockerOrNot = do
    weAreInDocker <- doesFileExist "/.dockerenv"
    host <- if weAreInDocker 
        then getHostByName "dockerhost" 
        else (getHostName >>= getHostByName)
    return $ show $ fromHostAddress $ head $ hostAddresses host
