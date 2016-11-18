module Main where

import Network.Socket
import System.IO
import System.Environment
import Control.Exception
import Control.Monad.Fix (fix)
import Control.Concurrent.ParallelIO.Local
import Control.Concurrent
import Data.List
import Data.List.Split
import Control.Monad (when, unless)

import Client
import Utils

main :: IO ()
main = do
    [port] <- getArgs
    sock <- socket AF_INET Stream 0                            -- create socket
    setSocketOption sock ReuseAddr 1                           -- make socket immediately reusable.
    bind sock (SockAddrInet (toEnum $ read port) iNADDR_ANY)   -- listen on TCP port given by user.
    let nbThreads = 5
    listen sock (nbThreads*2)                                  -- queue of 10 connections max
    killedChan <- newChan
    withPool nbThreads $ 
        \pool -> parallel_ pool (replicate nbThreads (server sock port killedChan))
    putStrLn "Server killed. See you !"

server :: Socket -> String -> Chan Bool -> IO ()
server sock port killedChan = do
    putStrLn "Waiting for incoming connection..."
    conn <- try (accept sock) :: IO (Either SomeException (Socket, SockAddr))  -- try to accept a connection and handle it
    case conn of
        Left  _    -> putStrLn "Socket is now closed. Exiting."
        Right conn -> do
            putStrLn "Got a client !"
            runClient conn sock port killedChan                -- run our client's logic, then
            server sock port killedChan                        -- repeat

runClient :: (Socket, SockAddr) -> Socket -> String -> Chan Bool -> IO ()
runClient (sock, addr) originalSocket port killedChan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl LineBuffering
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        (kill, timedOut, line) <- waitForInput hdl killedChan 0
        when (timedOut) (putStrLn "Client timed out")
        when (kill || timedOut) (return ())
        let commandAndArgs = splitOn " " (init line)
        let command = head commandAndArgs
        let args = intercalate " " $ tail commandAndArgs 
        case command of
            "KILL_SERVICE"    -> do
                writeChan killedChan True
                threadDelay 200000 -- 200ms
                killService originalSocket
            "HELO"            -> do
                helo hdl args port
                loop
            "JOIN_CHATROOM:"  -> do
                error <- join hdl args port
                unless error loop
            "LEAVE_CHATROOM:" -> do
                error <- leave hdl args
                unless error loop
            "DISCONNECT:"     -> do
                error <- disconnect hdl args
                unless error loop
            "CHAT:"           -> do
                error <- chat hdl args
                unless error loop
            _                 -> otherCommand hdl line
    -- shutdown sock ShutdownBoth 
    hClose hdl
    putStrLn "Client disconnected"

