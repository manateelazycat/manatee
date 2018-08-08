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

{-# LANGUAGE ScopedTypeVariables #-}
module Manatee.Toolkit.Gtk.Concurrent where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM 
import Graphics.UI.Gtk
import Manatee.Toolkit.General.STM

import qualified Control.Exception as Exc

data ViewChannel a = 
    ViewChannel {viewChannel          :: TChan a
                ,viewChannelLock      :: TVar Bool}

-- | Fork GUI IO.
forkGuiIO :: IO a -> (a -> IO ()) -> IO (MVar a, ThreadId, ThreadId)
forkGuiIO calcAction guiAction = do
  -- Create signal MVar variable.
  signal <- newEmptyMVar
  -- Build new thread for long-time calculation.
  calcThreadId <- forkIO $ calcAction >>= putMVar signal    -- fill signal when calculation finish 
  -- Build new thread for listen signal.
  guiThreadId <- onGuiSignal signal guiAction -- post GUI action to Gtk+ thread when catch finish signal
  return (signal, calcThreadId, guiThreadId)

-- | Similar `forkGuiIO`, except return ()
forkGuiIO_ :: IO a -> (a -> IO ()) -> IO ()
forkGuiIO_ calcAction guiAction =
  forkGuiIO calcAction guiAction
  >> return ()

-- | Post GUI Action to Gtk+ thread when catch signal.
onGuiSignal :: MVar a -> (a -> IO ()) -> IO ThreadId
onGuiSignal signal guiAction =
  forkIO $ takeMVar signal >>= postGUIAsync . guiAction

-- | Create view channel.
-- If widget destroy, stop read broadcast channel.
createViewChannel :: WidgetClass widget 
                         => TChan a 
                         -> widget
                         -> IO (ViewChannel a)
createViewChannel channel widget = do
  -- Create channel and lock.
  chan <- dupTChanIO channel
  lock <- newTVarIO True

  -- After render view widget destroy lock channel stop read thread.
  widget `onDestroy` writeTVarIO lock False

  -- Return ViewChannel.
  return $ ViewChannel chan lock
                    
-- | Listen view channel.
listenViewChannel :: ViewChannel a -> (a -> IO ()) -> IO ()
listenViewChannel vChannel@(ViewChannel {viewChannel      = channel
                                        ,viewChannelLock  = channelLock})
                  action = do
  forkIO $
    readTChanIO channel
    >>= \x -> do
      isLive <- readTVarIO channelLock
      when isLive $
           postGUIAsync $               
             Exc.catch 
               (do
                 action x
                 listenViewChannel vChannel action)
               (\ (_ :: Exc.SomeException) ->
                 putStrLn "listenViewChannel : Catch exception, stop read broadcast channel.")
  return ()

-- | Listen buffer channel.
listenBufferChannel :: TChan a -> (a -> IO ()) -> IO ()
listenBufferChannel channel action = do
  forkIO $
    readTChanIO channel
    >>= \x -> 
        postGUIAsync $
        Exc.catch 
          (do
            action x
            listenBufferChannel channel action)
           (\ (_ :: Exc.SomeException) ->
                putStrLn "listenBufferChannel : Catch exception, stop read broadcast channel.")
  return ()
