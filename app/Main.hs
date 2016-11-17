module Main where

import Network.Socket
import Network.BSD
import System.IO
import System.Environment
import System.Directory
import Control.Exception
import Control.Monad.Fix (fix)
import Control.Concurrent.ParallelIO.Local
import Control.Concurrent
import Control.Concurrent.Chan
import Data.List
import Data.IP
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (mallocBytes, free)
import Data.List.Split
import Control.Monad (when)

main :: IO ()
main = do
    [port] <- getArgs
    sock <- socket AF_INET Stream 0                            -- create socket
    setSocketOption sock ReuseAddr 1                           -- make socket immediately reusable.
    bind sock (SockAddrInet (toEnum $ read port) iNADDR_ANY)   -- listen on TCP port given by user.
    let nbThreads = 5
    listen sock (nbThreads*2)                                  -- queue of 10 connections max
    chan <- newChan
    withPool nbThreads $ 
        \pool -> parallel_ pool (replicate nbThreads (mainLoop sock port chan))
    putStrLn "Server killed. See you !"
    
mainLoop :: Socket -> String -> Chan Bool -> IO ()
mainLoop sock port chan = do
    putStrLn "Waiting for incoming connection..."
    conn <- try (accept sock) :: IO (Either SomeException (Socket, SockAddr))  -- try to accept a connection and handle it
    case conn of
        Left  _    -> putStrLn "Socket is now closed. Exiting."
        Right conn -> do
            putStrLn "Got a client !"
            runConn conn sock port chan  -- run our server's logic, then
            mainLoop sock port chan      -- repeat

runConn :: (Socket, SockAddr) -> Socket -> String -> Chan Bool -> IO ()
runConn (sock, addr) originalSocket port chan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl LineBuffering
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        (kill, line) <- waitForInput hdl chan
        when kill (return ())
        let commandAndArgs = splitOn " " (init line)
        let command = head commandAndArgs
        let args = intercalate " " $ tail commandAndArgs 
        case command of
            "KILL_SERVICE" -> do
                writeChan chan True
                threadDelay 500000 -- 500ms
                killService originalSocket
            "HELO"         -> helo hdl addr args port >> loop
            _              -> otherCommand hdl line
    shutdown sock ShutdownBoth 
    hClose hdl

waitForInput :: Handle -> Chan Bool -> IO (Bool, String)
waitForInput hdl chan = do
  stillAlive <- isEmptyChan chan
  if stillAlive
    then do
      request <- handle (\(SomeException _) -> return "") $ fix $ \getReq -> do
        buf <- mallocBytes 4096 :: IO (Ptr CChar)
        nbRead <- hGetBufNonBlocking hdl buf 4096 
        request <- peekCStringLen (buf, nbRead)
        free buf
        return request
      if null request
        then do
          res <- waitForInput hdl chan
          return res
        else do
          return (False, request)
    else return (True, [])

sendResponse :: Handle -> String -> IO ()
sendResponse hdl resp = do
    hSetBuffering hdl $ BlockBuffering $ Just (length resp)
    hPutStr hdl resp

getHostNameIfDockerOrNot :: IO String
getHostNameIfDockerOrNot = do
    weAreInDocker <- doesFileExist "/.dockerenv"
    host <- if weAreInDocker 
        then getHostByName "dockerhost" 
        else (getHostName >>= getHostByName)
    return $ show $ fromHostAddress $ head $ hostAddresses host

killService :: Socket -> IO ()
killService originalSocket = do
    putStrLn "Killing Service..."
    shutdown originalSocket ShutdownBoth
    close originalSocket

helo :: Handle -> SockAddr -> String -> String -> IO ()
helo hdl addr text port = do
    putStrLn $ "Responding to HELO command with params : " ++ text
    hostname <- getHostNameIfDockerOrNot
    sendResponse hdl $ "HELO " ++ text ++ "\nIP:" ++ hostname ++ "\nPort:" ++ port ++ "\nStudentID:16336620"

otherCommand :: Handle -> String -> IO ()
otherCommand hdl param = do
    putStrLn $ "Received unknown query : " ++ param
    -- sendResponse hdl $ "Command not implemented yet : " ++ param ++ "\nStay tuned !"
