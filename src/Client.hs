module Client where

import Utils

import Network.Socket
import System.IO
import Data.List
import Data.List.Split
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Data.HashTable.IO as H


-- Default method (the "otherwise" case)
otherCommand :: Handle -> String -> IO ()
otherCommand hdl param = do
    clog $ "Received unknown query : " ++ param
    -- sendResponse hdl $ "Command not implemented yet : " ++ param ++ "\nStay tuned !"

-- Kill server and all clients
killService :: Socket -> IO ()
killService originalSocket = do
    clog "Killing Service..."
    shutdown originalSocket ShutdownBoth
    close originalSocket

-- Basic "Helo" response
helo :: Handle -> String -> String -> IO ()
helo hdl text port = do
    clog $ "Responding to HELO command with params : " ++ text
    hostname <- getHostNameIfDockerOrNot
    sendResponse hdl $ "HELO " ++ text ++ "\nIP:" ++ hostname ++ "\nPort:" ++ port ++ "\nStudentID:16336620"

-- The client asks to join a chatroom
join :: Handle -> String -> String -> MVar Clients -> MVar ChatRooms -> IO (Bool, Int)
join hdl args port clients chatrooms = do
    clog $ "Receiving JOIN command with arguments : " ++ args
    let error = True
    let lines = splitOn "\n" args
    if not $ (length lines) == 4 then do 
        sendError hdl 1 $ "Bad arguments for JOIN_CHATROOM."
        return (error, -1)
    else do
        let chatRoomName         = lines !! 0
        let clientIP             = lines !! 1
        let clientPort           = lines !! 2
        let clientNameLine       = lines !! 3
        let clientNameLineParsed = splitOn " " clientNameLine
        let clientNameHeader     = head clientNameLineParsed
        let clientName           = intercalate " " $ tail clientNameLineParsed
        if not ((clientIP == "CLIENT_IP: 0") && (clientPort == "PORT: 0") && (clientNameHeader == "CLIENT_NAME:")) then do
            sendError hdl 2 "Bad arguments for JOIN_CHATROOM." 
            return (error, -1)
        else do
            (ChatRooms theChatrooms theChatroomsNames nbCR) <- takeMVar chatrooms
            wantedChatRoomID                        <- H.lookup theChatroomsNames chatRoomName
            (clientChanChat, roomRef, newNbCR)      <- case wantedChatRoomID of
                Just chatRoomRef -> do
                    wantedChatRoom <- H.lookup theChatrooms chatRoomRef
                    clientChanChat <- case wantedChatRoom of
                        Just (ChatRoom nbSubscribers chatChan) -> do
                            clientChanChat <- dupChan chatChan
                            H.insert theChatrooms chatRoomRef (ChatRoom (nbSubscribers+1) chatChan)
                            return clientChanChat
                        Nothing -> do
                            clog "/!\\ /!\\ /!\\ ------ This should not happen! ------ /!\\ /!\\ /!\\"
                            fakeChan <- newChan
                            return fakeChan
                    return (clientChanChat, chatRoomRef, nbCR)
                Nothing -> do
                    clientChanChat <- newChan
                    let newCRRef = nbCR + 1
                    H.insert theChatroomsNames chatRoomName newCRRef
                    H.insert theChatrooms newCRRef (ChatRoom 1 clientChanChat)
                    return (clientChanChat, newCRRef, newCRRef)
            putMVar chatrooms (ChatRooms theChatrooms theChatroomsNames newNbCR)
            (Clients lastClientId theClients clientsNames) <- takeMVar clients
            maybeClientId                        <- H.lookup clientsNames clientName
            (Client clientName channels joinId)  <- case maybeClientId of
                Just clientId -> do
                    maybeClient <- H.lookup theClients clientId
                    client      <- case maybeClient of
                        Just client -> return client
                        Nothing     -> do
                            clog "/!\\ /!\\ /!\\ ------ This should not happen! ------ /!\\ /!\\ /!\\"
                            htCTRefToChan <- H.new :: IO (HashTable Int (Chan String))
                            return (Client clientName htCTRefToChan (lastClientId+1))
                    return client
                Nothing       -> do
                    let joinId = lastClientId+1
                    H.insert clientsNames clientName joinId
                    htCTRefToChan <- H.new :: IO (HashTable Int (Chan String))
                    return (Client clientName htCTRefToChan joinId)
            H.insert channels roomRef clientChanChat
            H.insert theClients joinId (Client clientName channels joinId)
            putMVar clients (Clients joinId theClients clientsNames)
            writeChan clientChanChat $ "CHAT: " ++ (show roomRef) ++ "\nCLIENT_NAME: " ++ clientName ++ "\nMESSAGE: " ++ clientName ++ " has joined this chatroom.\n\n"
            serverIP <- getHostNameIfDockerOrNot
            let resp = "JOINED_CHATROOM: " ++ chatRoomName ++ "\nSERVER_IP: " ++ serverIP ++ "\nPORT: " ++ port ++ "\nROOM_REF: " ++ (show roomRef) ++ "\nJOIN_ID: " ++ (show joinId) ++ "\n"
            sendResponse hdl resp
            return (False, joinId)


-- The client asks to leave a chatroom
leave :: Handle -> String -> MVar Clients -> IO (Bool, Int)
leave hdl args clients = do
    clog $ "Receiving LEAVE message with args : " ++ args
    let error = True
    let lines = splitOn "\n" args
    if not $ (length lines) == 3 then do 
        sendError hdl 3 $ "Bad arguments for LEAVE_CHATROOM."
        return (error, -1)
    else do
        let chatRoomRefStr        = lines !! 0
        let joinIdLine            = lines !! 1
        let clientNameLine        = lines !! 2
        let clientNameLineParsed  = splitOn " " clientNameLine
        let clientNameHeader      = head clientNameLineParsed
        let clientNameGiven       = intercalate " " $ tail clientNameLineParsed
        let joinIdLineParsed      = splitOn " " joinIdLine
        let joinIdHeader          = head joinIdLineParsed
        let joinIdStr             = intercalate " " $ tail joinIdLineParsed
        let joinIdsCasted         = reads joinIdStr      :: [(Int, String)]
        let chatRoomRefsCasted    = reads chatRoomRefStr :: [(Int, String)]
        if not $ ((length joinIdsCasted) == 1 && (length chatRoomRefsCasted) == 1) then do
            sendError hdl 4 $ "Bad arguments for LEAVE_CHATROOM."
            return (error, -1)
        else do
            let (joinIdGivenByUser, restJ) = head joinIdsCasted
            let (chatRoomRef, restR)       = head chatRoomRefsCasted
            if not ((null restJ) && (null restR) && (clientNameHeader == "CLIENT_NAME:") && (joinIdHeader == "JOIN_ID:")) then do 
                sendError hdl 5 "Bad arguments for LEAVE_CHATROOM." 
                return (error, -1)
            else do
                (Clients lastClientId theClients clientsNames) <- takeMVar clients
                maybeClient                                    <- H.lookup theClients joinIdGivenByUser
                (Client clientName channels joinId, notFound)  <- case maybeClient of
                    Just client -> return (client, False)
                    Nothing     -> do
                        htCTRefToChan <- H.new :: IO (HashTable Int (Chan String))
                        return (Client "" htCTRefToChan (-1), True)
                if notFound then do 
                    sendError hdl 12 "Unknown JOIN_ID for LEAVE_CHATROOM."
                    return (error, -1)
                else do
                    maybeChannel <- H.lookup channels chatRoomRef
                    let message = "CHAT: " ++ (show chatRoomRef) ++ "\nCLIENT_NAME: " ++ clientName ++ "\nMESSAGE: " ++ clientName ++ " has left this chatroom.\n\n"
                    case maybeChannel of 
                        Just channel -> do
                            H.delete channels chatRoomRef
                            writeChan channel message
                        Nothing      -> return ()
                    H.insert theClients joinId (Client clientName channels joinId)
                    putMVar clients (Clients lastClientId theClients clientsNames)
                    let resp = "LEFT_CHATROOM: " ++ (show chatRoomRef) ++ "\nJOIN_ID: " ++ (show joinId) ++ "\n"
                    sendResponse hdl resp
                    sendResponse hdl message
                    return (False, joinId)

-- The client disconnects
disconnect :: Handle -> String -> MVar Clients -> IO Bool
disconnect hdl args clients = do
    clog $ "Receiving DISCONNECT message with args : " ++ args
    let error = True
    let lines = splitOn "\n" args
    if not $ (length lines) == 3 then do 
        sendError hdl 6 $ "Bad arguments for DISCONNECT."
        return error
    else do
        let disconnect            = lines !! 0
        let portLine              = lines !! 1
        let clientNameLine        = lines !! 2
        let clientNameLineParsed  = splitOn " " clientNameLine
        let clientNameHeader      = head clientNameLineParsed
        let clientNameGivenByUser = intercalate " " $ tail clientNameLineParsed
        if not ((disconnect == "0") && (portLine == "PORT: 0") && (clientNameHeader == "CLIENT_NAME:")) then do 
            sendError hdl 7 "Bad arguments for DISCONNECT." 
            return error
        else do
            (Clients lastClientId theClients clientsNames) <- takeMVar clients
            maybeClientId                                <- H.lookup clientsNames clientNameGivenByUser
            (Client clientName channels joinId, success) <- case maybeClientId of
                Just clientId -> do
                    maybeClient       <- H.lookup theClients clientId
                    (client, success) <- case maybeClient of
                        Just client -> return (client, True)
                        Nothing     -> do
                            htCTRefToChan <- H.new :: IO (HashTable Int (Chan String))
                            return (Client "" htCTRefToChan (-1), False)
                    return (client, success)
                Nothing       -> do
                    htCTRefToChan <- H.new :: IO (HashTable Int (Chan String))
                    return (Client "" htCTRefToChan (-1), False)
            if not success then do
                putMVar clients (Clients lastClientId theClients clientsNames)
                sendError hdl 14 "Client name does not exist."
                return error
            else do
                messages <- H.foldM (sendLeaveMessages clientNameGivenByUser) [] channels
                H.delete theClients joinId
                H.delete clientsNames clientName
                let messagesSortedAll = sortOn (\(m,chan,id) -> id) messages
                let messagesSorted = map (\(m,chan,id) -> m) $ messagesSortedAll 
                let chans = map (\(m,chan,id) -> chan) $ messagesSortedAll 
                clog $ show messagesSorted
                mapM_ (sendResponse hdl) messagesSorted
                sequence (zipWith ($) (map writeChan chans) messagesSorted)
                putMVar clients (Clients lastClientId theClients clientsNames)
                return True -- fake error, we just want to disconnect the client at this point


-- The client sends a message
chat :: Handle -> String -> MVar Clients -> IO Bool
chat hdl args clients = do
    let error = True
    let request = splitOn "\nMESSAGE: " args
    if not $ (length request) >= 2 then do 
        sendError hdl 8 $ "Bad arguments for CHAT."
        return error
    else do
        let lines   = splitOn "\n" (request !! 0)
        let message = intercalate "\nMESSAGE: " $ tail request
        if not $ (length lines) == 3 then do 
            sendError hdl 9 $ "Bad arguments for CHAT."
            return error
        else do
            let chatRoomRefStr        = lines !! 0
            let joinIdLine            = lines !! 1
            let clientNameLine        = lines !! 2
            let clientNameLineParsed  = splitOn " " clientNameLine
            let clientNameHeader      = head clientNameLineParsed
            let clientNameGiven       = intercalate " " $ tail clientNameLineParsed
            let joinIdLineParsed      = splitOn " " joinIdLine
            let joinIdHeader          = head joinIdLineParsed
            let joinIdStr             = intercalate " " $ tail joinIdLineParsed
            let joinIdsCasted         = reads joinIdStr      :: [(Int, String)]
            let chatRoomRefsCasted    = reads chatRoomRefStr :: [(Int, String)]
            if not $ ((length joinIdsCasted) == 1 && (length chatRoomRefsCasted) == 1) then do
                sendError hdl 10 $ "Bad arguments for CHAT."
                return error
            else do
                let (joinIdGivenByUser, restJ) = head joinIdsCasted
                let (chatRoomRef, restR)       = head chatRoomRefsCasted
                if not ((null restJ) && (null restR) && (clientNameHeader == "CLIENT_NAME:") && (joinIdHeader == "JOIN_ID:")) then do
                    sendError hdl 11 "Bad arguments for CHAT."
                    return error
                else do
                    (Clients lastClientId theClients clientsNames) <- takeMVar clients
                    maybeClient                                    <- H.lookup theClients joinIdGivenByUser
                    (Client clientName channels joinId, notFound)  <- case maybeClient of
                        Just client -> return (client, False)
                        Nothing     -> do
                            htCTRefToChan <- H.new :: IO (HashTable Int (Chan String))
                            return (Client "" htCTRefToChan (-1), True)
                    if notFound then do
                        putMVar clients (Clients lastClientId theClients clientsNames)
                        sendError hdl 13 "Unknown JOIN_ID for CHAT."
                        return error
                    else do
                        maybeChannel <- H.lookup channels chatRoomRef
                        let resp = "CHAT: " ++ (show chatRoomRef) ++ "\nCLIENT_NAME: " ++ clientNameGiven ++ "\nMESSAGE: " ++ message ++ "\n"
                        clog $ "Chat sending : " ++ resp
                        error <- case maybeChannel of 
                            Just channel -> do
                                writeChan channel resp
                                return False
                            Nothing      -> return True
                        putMVar clients (Clients lastClientId theClients clientsNames)
                        return error
