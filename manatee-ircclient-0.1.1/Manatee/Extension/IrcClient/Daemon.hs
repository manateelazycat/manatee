-- Author:     Andy Stewart <lazycat.manatee@gmail.com>
-- Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
-- 
-- Copyright (C) 2010 ~ 2011 Andy Stewart, all rights reserved.
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Manatee.Extension.IrcClient.Daemon where

import Control.Applicative hiding (empty)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import DBus.Client hiding (Signal)
import Data.List (delete)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Manatee.Extension.IrcClient.DBus
import Manatee.Extension.IrcClient.Types
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Network
import Network.FastIRC.IO
import Network.FastIRC.Messages
import Network.FastIRC.Users
import System.IO
import System.Posix.Types (ProcessID)
import Text.Groom

import qualified Control.Exception as Exc
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S

type HasConnected = Bool

type IrcConnect = 
    -- "MVar Handle" as lock for avoid multithreading request to connect same server.
    Map Server
    -- Use processId list can broadcast irc message to multiple clients.
        (MVar Handle, Map Channel [ProcessID])

data IrcStatus =
    IrcStatus {ircConnect       :: TVar IrcConnect
              ,ircClient        :: Client
              ,ircExitSignal    :: MVar String}

-- | Initial status.
ircInitStatus :: IO IrcStatus
ircInitStatus =
  IrcStatus <$> newTVarIO M.empty
            <*> mkSessionClient
            <*> newEmptyMVar

-- | Send irc message.
ircSendMessage :: IrcStatus -> IrcDaemonSignalArgs -> IO ()
ircSendMessage IrcStatus {ircConnect    = connect} 
               (SendMessageArgs server message) = do
  connectStatus <- readTVarIO connect
  case findMinMatch connectStatus (\ serverName _ -> serverName == server) of
    Just (_, (handle, _)) -> do
      isEmpty <- isEmptyMVar handle
      if isEmpty
         -- Socket is not activated.
         then putStrLn $ "Server " ++ server ++ "'s handle is not activated."
         -- Send message.
         else do
           h <- readMVar handle
           write h message
    -- Server is not connected.
    Nothing -> putStrLn $ "Server " ++ server ++ " is not connected."

-- | Part channel.
ircPart :: IrcStatus -> IrcDaemonSignalArgs -> IO ()    
ircPart (IrcStatus {ircConnect   = connect
                   ,ircExitSignal= exitSignal})
        (PartArgs server channel processId) = do
  -- Debug.
  -- putStrLn $ "ircPart : part command " ++ show (server, channel, processId)
  
  -- Part channel.
  connectStatus <- readTVarIO connect
  findMinMatch connectStatus (\ serverName _ -> serverName == server) 
    ?>= \ (_, (handle, channelMap)) -> do
      let newChannelMap = 
              case findMinMatch channelMap (\ channelName _ -> channelName == channel) of
                Just (_, processIdList) -> 
                    -- Delete process id from list.
                    case delete processId processIdList of
                      -- Delete channel from map if no client process exit. 
                      [] -> M.delete channel channelMap
                      -- Otherwise, just delete process from process list.
                      x  -> M.insert channel x channelMap
                Nothing -> channelMap

      if M.null newChannelMap
         -- Close socket when no channel exit in with server.
         then do
           -- Close socket.
           tryTakeMVar handle >?>= \h -> 
                -- putStrLn $ "ircPart : Close irc server " ++ server ++ " connect."
                hClose h                                      

           -- Whether quit daemon process.
           let newConnect = M.delete server connectStatus
           if M.null newConnect
              -- Quit daemon process when no server exit.
              then putMVar exitSignal "Exit"
              -- Otherwise, delete server from map.
              else 
                -- putStrLn $ "ircPart : Delete irc server " ++ server
                writeTVarIO connect newConnect
         -- Otherwise, just update channel's process list.
         else 
           -- putStrLn $ "ircPart : Part channel " ++ channel
           modifyTVarIO connect $ \c ->
             M.insert server (handle, newChannelMap) c

-- | Join channel.
ircJoin :: IrcStatus -> IrcDaemonSignalArgs -> IO ()
ircJoin status@(IrcStatus {ircConnect   = connect})
        (JoinArgs server port channel nick processId) = do
  -- Debug.
  -- putStrLn $ "Join command : " ++ show (server, port, channel, nick, processId)

  connectStatus <- readTVarIO connect
  let connectMatch = findMinMatch connectStatus (\ serverName _ -> serverName == server)
  (connectHandle, firstJoin) <- 
    case connectMatch of
      -- Add client information if server has connected.
      Just (serverName, (handle, channelMap)) -> do
        let newConnectStatus = 
              M.insert serverName 
                   (handle,
                    case findMinMatch channelMap (\ channelName _ -> channelName == channel) of
                      Just (channelName, processList) -> 
                          if processId `elem` processList
                             -- Keep no touch if channel and processId has exist
                             then channelMap
                             -- Otherwise add current processId in map.
                             else M.insert channelName (processList ++ [processId]) channelMap
                      Nothing -> 
                          -- Or create new processid channel.
                          M.insert channel [processId] channelMap) 
                   connectStatus
        writeTVarIO connect newConnectStatus
        return (handle, False)
      -- Otherwise connect server.
      Nothing -> do
        -- Create socket lock avoid multiple connect *race condition*.
        handle <- newEmptyMVar
        let newConnectStatus = 
              M.insert server 
                       (handle, M.fromList [(channel, [processId])])
                       connectStatus
        writeTVarIO connect newConnectStatus

        -- Connect server.
        h <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering h NoBuffering
        isSucess <- tryPutMVar handle h
        if isSucess 
           then putStrLn $ "ircJoin : connect server " ++ server ++ " successful."
           else putStrLn "### ircJoin : Impossible! Multiple threads trying to connect to same server."
      
        -- Listen server.
        forkIO $ ircListenServerMessage status (server, h)
      
        return (handle, True)

  -- All threads for same socket will block here until first thread connect socket successful.
  h <- readMVar connectHandle

  -- Feed Nick and User message when first connect to server.
  when firstJoin $ do
    hPutCommand h (NickCmd (B.pack nick) Nothing)
    hPutCommand h (UserCmd tempUser "0" tempRealname "0")

  -- Feed join message.
  hPutCommand h (JoinCmd $ M.fromList [(B.pack channel, Nothing)])
  
-- | Listen server message.
ircListenServerMessage :: IrcStatus -> (Server, Handle) -> IO ()
ircListenServerMessage status@(IrcStatus {ircConnect = connect
                                         ,ircClient  = client})
                       (server, handle) = 
  Exc.catch 
     (do 
       -- Get irc message.
       Message {msgOrigin  = origin
               ,msgCommand = command} <- hGetMessage handle

       -- Get nick.
       let (nick, user, host) = 
               case origin of
                 Just spec -> 
                     case spec of
                       Nick n -> (n, "", "")
                       User n u h -> (n, u, h)
                 Nothing -> ("", "", "")
       
       -- Parse command.
       connectStatus <- readTVarIO connect
       let sendToChannel command channelMap channel action = 
               case findMinMatch channelMap (\ channelName _ -> B.pack channelName == channel) of
                 Just (_, processIdList) -> 
                     -- Broadcast message to all irc client.
                     forM_ processIdList action
                 Nothing -> putStrLn $ "ircListenServerMessage : " ++ groom command
       case findMinMatch connectStatus (\ serverName _ -> serverName == server) of
         Just (_, (_, channelMap)) -> 
           case command of
             PrivMsgCmd targetNameSet commandArg -> 
                 mapM_ (\ target -> 
                            sendToChannel command channelMap target $ \processId -> 
                                mkIrcClientSignal client processId ReceivePrivate (ReceivePrivateArgs nick commandArg)
                       ) $ S.toList targetNameSet
             PingCmd server str -> 
                          -- putStrLn $ "ircListenServerMessage : Pong server " ++ show (showCommand command)
                          hPutCommand handle $ PongCmd server str
             JoinCmd channels -> 
                    mapM_ (\ (channel, _) -> 
                               sendToChannel command channelMap channel $ \processId -> 
                                   mkIrcClientSignal client processId ReceiveJoin (ReceiveJoinArgs nick user host)
                          ) $ M.toList channels
             QuitCmd reason -> 
                     -- Broadcast quit message to all channel process.
                     mapM_ (\ (_, processIdList) -> 
                                forM_ processIdList $ \processId -> 
                                    mkIrcClientSignal client processId 
                                                      ReceiveQuit 
                                                      (ReceiveQuitArgs nick user host (fromMaybe "" reason))
                           ) $ M.toList channelMap
             PartCmd targetNameSet commandArg -> 
                 mapM_ (\ target -> 
                            sendToChannel command channelMap target $ \processId -> 
                                mkIrcClientSignal client processId 
                                                  ReceivePart 
                                                  (ReceivePartArgs nick user host (fromMaybe "" commandArg))
                       ) $ S.toList targetNameSet
             NumericCmd number messages -> 
                 case number of
                   332 -> 
                     when (length messages >= 3) $ do
                       let channel = messages !! 1
                           msg     = B.concat $ drop 2 messages
                       sendToChannel command channelMap channel $ \processId -> 
                           mkIrcClientSignal 
                               client processId 
                               ReceiveTopicReply 
                               (ReceiveTopicReplyArgs msg)
                   333 -> 
                       when (length messages >= 4) $ do
                         let channel = messages !! 1
                             nick    = messages !! 2
                             seconds = read (B.unpack (messages !! 3)) :: Integer
                         sendToChannel command channelMap channel $ \processId -> 
                             mkIrcClientSignal 
                                 client processId 
                                 ReceiveTopicWhoTime 
                                 (ReceiveTopicWhoTimeArgs nick seconds)
                   328 -> 
                       when (length messages >= 3) $ do
                         let channel = messages !! 1
                             url     = messages !! 2
                         sendToChannel command channelMap channel $ \processId -> 
                             mkIrcClientSignal 
                                 client processId 
                                 ReceiveChannelUrl
                                 (ReceiveChannelUrlArgs url)
                   353 -> 
                       when (length messages >= 4) $ do
                         let channel = messages !! 2
                             nicks   = messages !! 3
                         sendToChannel command channelMap channel $ \processId ->
                             mkIrcClientSignal 
                                 client processId
                                 ReceiveNames
                                 (ReceiveNamesArgs nicks)
                   _ -> putStrLn $ "Numeric <" ++ show number ++ "> " ++ show messages
             _ -> putStrLn $ groom $ showCommand command
         Nothing -> putStrLn $ "### Impossible! ircListenServerMessage : Unknown server " ++ server
       
       -- Loop.
       ircListenServerMessage status (server, handle))
     -- Print debug information when catch server socket exception.
     (\ (_ :: Exc.IOException) -> putStrLn $ "ircListenServerMessage : Catch server " ++ server ++ " socket exception\n.")

-- | Flush message to socket.
write :: Handle -> B.ByteString -> IO ()
write handle msg = do
  B.hPutStr handle (msg `B.append` "\r\n")
  B.putStrLn msg

