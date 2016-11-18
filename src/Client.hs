module Client where

import Utils

import Network.Socket
import System.IO
import Data.List
import Data.List.Split

-- Default method (the "otherwise" case)
otherCommand :: Handle -> String -> IO ()
otherCommand hdl param = do
    putStrLn $ "Received unknown query : " ++ param
    -- sendResponse hdl $ "Command not implemented yet : " ++ param ++ "\nStay tuned !"

-- Kill server and all clients
killService :: Socket -> IO ()
killService originalSocket = do
    putStrLn "Killing Service..."
    shutdown originalSocket ShutdownBoth
    close originalSocket

-- Basic "Helo" response
helo :: Handle -> String -> String -> IO ()
helo hdl text port = do
    putStrLn $ "Responding to HELO command with params : " ++ text
    hostname <- getHostNameIfDockerOrNot
    sendResponse hdl $ "HELO " ++ text ++ "\nIP:" ++ hostname ++ "\nPort:" ++ port ++ "\nStudentID:16336620"

-- The client asks to join a chatroom
join :: Handle -> String -> String -> IO Bool
join hdl args port = do
    let error = True
    let lines = splitOn "\n" args
    if not $ (length lines) == 4
        then do 
            sendError hdl 1 $ "Bad arguments for JOIN_CHATROOM."
            return error
        else do
            let chatRoomName         = lines !! 0
            let clientIP             = lines !! 1
            let clientPort           = lines !! 2
            let clientNameLine       = lines !! 3
            let clientNameLineParsed = splitOn " " clientNameLine
            let clientNameHeader     = head clientNameLineParsed
            let clientName           = intercalate " " $ tail clientNameLineParsed
            if not ((clientIP == "CLIENT_IP: 0") && (clientPort == "PORT: 0") && (clientNameHeader == "CLIENT_NAME:"))
                then do
                    sendError hdl 2 "Bad arguments for JOIN_CHATROOM." 
                    return error
                else do
                    let joinId = 1
                    let roomRef = 2
                    serverIP <- getHostNameIfDockerOrNot
                    let resp = "JOINED_CHATROOM: " ++ chatRoomName ++ "\nSERVER_IP: " ++ serverIP ++ "\nPORT: " ++ port ++ "\nROOM_REF: " ++ (show roomRef) ++ "\nJOIN_ID: " ++ (show joinId)
                    sendResponse hdl resp
                    return False


-- The client asks to leave a chatroom
leave :: Handle -> String -> IO Bool
leave hdl args = do
    let error = True
    let lines = splitOn "\n" args
    if not $ (length lines) == 3
        then do 
            sendError hdl 3 $ "Bad arguments for LEAVE_CHATROOM."
            return error
        else do
            let chatRoomRef           = lines !! 0
            let joinIdLine            = lines !! 1
            let clientNameLine        = lines !! 2
            let clientNameLineParsed  = splitOn " " clientNameLine
            let clientNameHeader      = head clientNameLineParsed
            let clientName            = intercalate " " $ tail clientNameLineParsed
            let joinIdLineParsed      = splitOn " " joinIdLine
            let joinIdHeader          = head joinIdLineParsed
            let joinIdStr             = intercalate " " $ tail joinIdLineParsed
            let joinIdsCasted         = reads joinIdStr   :: [(Int, String)]
            let chatRoomRefsCasted    = reads chatRoomRef :: [(Int, String)]
            if not $ ((length joinIdsCasted) == 1 && (length chatRoomRefsCasted) == 1)
                then do
                    sendError hdl 4 $ "Bad arguments for LEAVE_CHATROOM."
                    return error
                else do
                    let (joinId, restJ)      = head joinIdsCasted
                    let (chatRoomRef, restR) = head chatRoomRefsCasted
                    if not ((null restJ) && (null restR) && (clientNameHeader == "CLIENT_NAME:") && (joinIdHeader == "JOIN_ID:"))
                        then do 
                            sendError hdl 5 "Bad arguments for LEAVE_CHATROOM." 
                            return error
                        else do
                            let resp = "LEFT_CHATROOM: " ++ (show chatRoomRef) ++ "\nJOIN_ID: " ++ (show joinId)
                            sendResponse hdl resp
                            return False

-- The client disconnects
disconnect :: Handle -> String -> IO Bool
disconnect hdl args = do
    let error = True
    let lines = splitOn "\n" args
    if not $ (length lines) == 3
        then do 
            sendError hdl 6 $ "Bad arguments for DISCONNECT."
            return error
        else do
            let disconnect            = lines !! 0
            let portLine              = lines !! 1
            let clientNameLine        = lines !! 2
            let clientNameLineParsed  = splitOn " " clientNameLine
            let clientNameHeader      = head clientNameLineParsed
            let clientName            = intercalate " " $ tail clientNameLineParsed
            if not ((disconnect == "0") && (portLine == "PORT: 0") && (clientNameHeader == "CLIENT_NAME:"))
                then do 
                    sendError hdl 7 "Bad arguments for DISCONNECT." 
                    return error
                else do
                    -- remove client "clientName" from database
                    return True -- fake error, we just want to disconnect the client at this point


-- The client sends a message
chat :: Handle -> String -> IO Bool
chat hdl args = do
    let error = True
    let request = splitOn "\nMESSAGE: " args
    if not $ (length request) >= 2
        then do 
            sendError hdl 8 $ "Bad arguments for CHAT."
            return error
        else do
            let lines   = splitOn "\n" (request !! 0)
            let message = intercalate "\nMESSAGE: " $ tail request
            if not $ (length lines) == 3
                then do 
                    sendError hdl 9 $ "Bad arguments for CHAT."
                    return error
                else do
                    let chatRoomRef           = lines !! 0
                    let joinIdLine            = lines !! 1
                    let clientNameLine        = lines !! 2
                    let clientNameLineParsed  = splitOn " " clientNameLine
                    let clientNameHeader      = head clientNameLineParsed
                    let clientName            = intercalate " " $ tail clientNameLineParsed
                    let joinIdLineParsed      = splitOn " " joinIdLine
                    let joinIdHeader          = head joinIdLineParsed
                    let joinIdStr             = intercalate " " $ tail joinIdLineParsed
                    let joinIdsCasted         = reads joinIdStr   :: [(Int, String)]
                    let chatRoomRefsCasted    = reads chatRoomRef :: [(Int, String)]
                    if not $ ((length joinIdsCasted) == 1 && (length chatRoomRefsCasted) == 1)
                        then do
                            sendError hdl 10 $ "Bad arguments for CHAT."
                            return error
                        else do
                            let (joinId, restJ)      = head joinIdsCasted
                            let (chatRoomRef, restR) = head chatRoomRefsCasted
                            if not ((null restJ) && (null restR) && (clientNameHeader == "CLIENT_NAME:") && (joinIdHeader == "JOIN_ID:"))
                                then do 
                                    sendError hdl 11 "Bad arguments for CHAT."
                                    return error
                                else do
                                    -- send to every clients on this chat room
                                    let resp = "CHAT: " ++ (show chatRoomRef) ++ "\nCLIENT_NAME: " ++ clientName ++ "\nMESSAGE: " ++ message
                                    sendResponse hdl resp
                                    return False
