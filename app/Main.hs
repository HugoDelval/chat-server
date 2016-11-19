module Main where

import Network.Socket
import System.IO
import System.Environment
import Control.Exception
import Control.Monad.Fix (fix)
import Control.Concurrent.ParallelIO.Local
import Control.Concurrent.MVar
import Control.Concurrent
import Data.List
import Data.List.Split
import qualified Data.HashTable.IO as H
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
    htSI           <- H.new :: IO (HashTable String Int)
    htIC           <- H.new :: IO (HashTable Int ChatRoom)
    htClients      <- H.new :: IO (HashTable Int Client)
    htClientsNames <- H.new :: IO (HashTable String Int)
    let nbCR = 0
    chatRooms <- newMVar (ChatRooms {chatRoomFromId=htIC, chatRoomIdFromName=htSI, numberOfChatRooms=nbCR})
    clients <- newMVar (Clients 0 htClients htClientsNames)
    withPool nbThreads $ 
        \pool -> parallel_ pool (replicate nbThreads (server sock port killedChan clients chatRooms))
    clog "Server killed. See you !"

server :: Socket -> String -> Chan Bool -> MVar Clients -> MVar ChatRooms -> IO ()
server sock port killedChan clients chatrooms = do
    clog "Waiting for incoming connection..."
    conn <- try (accept sock) :: IO (Either SomeException (Socket, SockAddr))  -- try to accept a connection and handle it
    case conn of
        Left  _    -> clog "Socket is now closed. Exiting."
        Right conn -> do
            clog "Got a client !"
            runClient conn sock port killedChan clients chatrooms       -- run our client's logic, then
            server sock port killedChan clients chatrooms               -- repeat

runClient :: (Socket, SockAddr) -> Socket -> String -> Chan Bool -> MVar Clients -> MVar ChatRooms -> IO ()
runClient (sock, addr) originalSocket port killedChan clients chatrooms = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl LineBuffering
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        (kill, timedOut, input) <- waitForInput hdl killedChan 0
        when (timedOut) (clog "Client timed out")
        when (kill || timedOut) (return ())
        let commandAndArgs = splitOn " " input
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
                error <- join hdl args port clients chatrooms
                unless error loop
            "LEAVE_CHATROOM:" -> do
                error <- leave hdl args clients
                unless error loop
            "DISCONNECT:"     -> do
                error <- disconnect hdl args
                unless error loop
            "CHAT:"           -> do
                error <- chat hdl args
                unless error loop
            _                 -> otherCommand hdl input
    -- shutdown sock ShutdownBoth 
    hClose hdl
    clog "Client disconnected"

