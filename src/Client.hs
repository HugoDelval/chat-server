module Client where

import Utils

import Network.Socket
import System.IO
import Data.List
import Data.List.Split

killService :: Socket -> IO ()
killService originalSocket = do
    putStrLn "Killing Service..."
    shutdown originalSocket ShutdownBoth
    close originalSocket

helo :: Handle -> String -> String -> IO ()
helo hdl text port = do
    putStrLn $ "Responding to HELO command with params : " ++ text
    hostname <- getHostNameIfDockerOrNot
    sendResponse hdl $ "HELO " ++ text ++ "\nIP:" ++ hostname ++ "\nPort:" ++ port ++ "\nStudentID:16336620"

otherCommand :: Handle -> String -> IO ()
otherCommand hdl param = do
    putStrLn $ "Received unknown query : " ++ param
    -- sendResponse hdl $ "Command not implemented yet : " ++ param ++ "\nStay tuned !"

join :: Handle -> String -> IO Bool
join hdl args = do
    let error = True
    let lines = splitOn "\n" args
    if not $ (length lines) == 4
        then do 
            sendError hdl 1 $ "Bad arguments for JOIN_CHATROOM." ++ "*********************\n" ++ args ++ "\n********************"
            return error
        else do
            let chatRoomName   = lines !! 0
            let clientIP       = lines !! 1
            let clientPort     = lines !! 2
            let clientNameLine = lines !! 3
            let clientNameLineParsed = splitOn " " clientNameLine
            let clientNameHeader = head clientNameLineParsed
            let clientName = intercalate " " $ tail clientNameLineParsed
            if not ((clientIP == "CLIENT_IP: 0") && (clientPort == "PORT: 0") && (clientNameHeader == "CLIENT_NAME:"))
                then do 
                    sendError hdl 1 "Bad arguments for JOIN_CHATROOM." 
                    return error
                else do
                    return False
