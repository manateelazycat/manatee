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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Manatee.Extension.Mplayer.Daemon where

import Control.Applicative hiding (empty)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import DBus.Client hiding (Signal)
import Data.Maybe (isNothing)
import Manatee.Extension.Mplayer.DBus
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import System.IO
import System.Posix.Types (ProcessID)
import System.Process

type MplayerHandle = (Handle, Handle, Handle, ProcessHandle)

data MplayerStatus =
    MplayerStatus {mplayerHandle           :: TVar (Maybe MplayerHandle) -- handle of mplayer process
                  ,mplayerPlayStatus       :: TVar PlayStatus            -- play status
                  ,mplayerStopByCommand    :: MVar String                -- whether process stop by user's command?
                  ,mplayerProcessId        :: TVar (Maybe ProcessID)     -- the process id of mplayer client
                  ,mplayerClient           :: Client                     -- the dbus client
                  ,mplayerSentinelThreadId :: TVar (Maybe ThreadId)      -- the id of sentinel thread
                  }

data PlayStatus = PlayStatus
                | PauseStatus
                | StopStatus
                  deriving (Show, Eq, Ord)

-- | Init mplayer status.
mplayerInitStatus :: IO MplayerStatus
mplayerInitStatus = 
  MplayerStatus <$> newTVarIO Nothing
                <*> newTVarIO PauseStatus
                <*> newEmptyMVar
                <*> newTVarIO Nothing
                <*> mkSessionClient
                <*> newTVarIO Nothing

-- | Play current select candidate.
mplayerPlay :: MplayerStatus -> (String, ProcessID) -> IO ()
mplayerPlay status@(MplayerStatus {mplayerHandle           = handle
                                  ,mplayerPlayStatus       = playStatus
                                  ,mplayerStopByCommand    = byCommand
                                  ,mplayerProcessId        = processId}) 
            (filepath, pid) = do
  -- Stop play first.
  mplayerStopInternal status

  -- No buffer with handle, transform immediately.
  newHandle@(inp, oup, err, ph) <- runInteractiveCommand $ "mplayer -slave \"" ++ filepath ++ "\""
  hSetBuffering inp NoBuffering
  hSetBuffering oup NoBuffering
  hSetBuffering err NoBuffering
                
  -- Update status.
  writeTVarIO handle (Just newHandle)
  writeTVarIO playStatus PlayStatus
  writeTVarIO processId (Just pid)

  -- When MVar is empty and mplayer process exit, 
  -- will send DBus signal to mplayer client for play next file.
  -- Otherwise, will consider mplayer stop by user's stop command.
  tryTakeMVar byCommand

  -- Fork thread to sentinel mplayer process.
  -- Will send DBus signal to mplayer client after current file play finished.
  forkIO $ mplayerSentinelProcess status ph

  return ()

-- | Sentinel mplayer process.
-- If MVar 'mplayerStopByCommand' is empty,   
-- will send DBus signal to mplayer client after current file play finished.
-- Otherwise, consider mplayer process stop by user's command.
mplayerSentinelProcess :: MplayerStatus -> ProcessHandle -> IO ()
mplayerSentinelProcess MplayerStatus {mplayerStopByCommand    = byCommand
                                     ,mplayerProcessId        = processId
                                     ,mplayerClient           = client
                                     ,mplayerSentinelThreadId = threadId} 
                       processHandle = do
    -- Use this thread id tvar avoid multiple threads race condition
    -- with `mplayerStopByCommand' lock variable.
    myThreadId >>= \ tId -> writeTVarIO threadId (Just tId)

    -- Wait mplayer process exit.
    _ <- waitForProcess processHandle

    -- When MVar `mplayerStopByCommand' is empty,
    -- try to send signal to mplayer client for play next file.
    mvar <- tryTakeMVar byCommand
    when (isNothing mvar) $ 
         readTVarIO processId >?>= \pid -> 
             readTVarIO threadId >?>= \tId -> do
               currentThreadId <- myThreadId
               -- Just send DBus signal when activate thread same as current thread.
               -- this condition avoid multiple threads race condition with 
               -- `mplayerStopByCommand' lock variable.
               when (currentThreadId == tId) $
                    mkMplayerClientSignal client pid PlayFinished PlayFinishedArgs

-- | Pause or continue play.
mplayerPause :: MplayerStatus -> IO ()
mplayerPause status@(MplayerStatus {mplayerPlayStatus    = playStatus})  = do
  -- Send pause command to mplayer process.
  mplayerSendCommand status "pause" False

  -- Switch play status.
  modifyTVarIO playStatus 
    (\ stat -> case stat of
                PlayStatus -> PauseStatus
                _ -> PlayStatus)

-- | Stop play.
mplayerStop :: MplayerStatus -> IO ()
mplayerStop status@(MplayerStatus {mplayerHandle        = handle 
                                  ,mplayerPlayStatus    = playStatus}) = do
  -- Stop mplayer process.
  mplayerStopInternal status

  -- Update status.
  writeTVarIO handle Nothing
  writeTVarIO playStatus StopStatus

-- | Internal function for mplayerStop.
mplayerStopInternal :: MplayerStatus -> IO ()
mplayerStopInternal MplayerStatus {mplayerHandle           = handle
                                  ,mplayerStopByCommand    = byCommand
                                  ,mplayerSentinelThreadId = threadId} = 
  readTVarIO handle >?>= \ (inp, _, _, _) -> do
    -- Fill `mplayerStopByCommand` lock variable, then sentinel thread will consider mplayer process
    -- cause by user's stop command.
    tryTakeMVar byCommand >> putMVar byCommand "Stop by command : mplayerStop"

    -- Update thread id.
    writeTVarIO threadId Nothing

    -- Just send quit command when process is running.
    whenM (mplayerProcessIsActivated handle) $ hPutStrLn inp "quit"

-- | Forward current track.
mplayerForward :: MplayerStatus -> Int -> IO ()
mplayerForward status step = 
  mplayerSendCommand status ("seek " ++ show step) True

-- | Backward current track.
mplayerBackward :: MplayerStatus -> Int -> IO ()
mplayerBackward status step = 
  mplayerSendCommand status ("seek -" ++ show step) True

-- | Increase volume.
mplayerVolumeInc :: MplayerStatus -> Int -> IO ()
mplayerVolumeInc status step = 
  mplayerSendCommand status ("volume " ++ show step) True

-- | Decrease volume.
mplayerVolumeDec :: MplayerStatus -> Int -> IO ()
mplayerVolumeDec status step = 
  mplayerSendCommand status ("volume -" ++ show step) True

-- | Check whether mplayer process is activated.
mplayerProcessIsActivated :: TVar (Maybe MplayerHandle) -> IO Bool
mplayerProcessIsActivated handle = do
  h <- readTVarIO handle
  case h of
    Just (_, _, _, ph) -> liftM isNothing $ getProcessExitCode ph
    Nothing -> return False 

-- | Send command to mplayer process.
mplayerSendCommand :: MplayerStatus -> String -> Bool -> IO ()
mplayerSendCommand MplayerStatus {mplayerHandle      = handle
                                 ,mplayerPlayStatus  = playStatus}
                   command 
                   needPlaying = do
  stat <- readTVarIO playStatus
  when (not needPlaying || stat == PlayStatus) $
       readTVarIO handle >?>= \ (inp, _, _, _) -> 
         whenM (mplayerProcessIsActivated handle) $ 
           hPutStrLn inp command
    
  
