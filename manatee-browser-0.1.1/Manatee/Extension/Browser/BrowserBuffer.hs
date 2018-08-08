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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Extension.Browser.BrowserBuffer where

import Control.Applicative
import Control.Concurrent.STM 
import DBus.Client hiding (Signal)
import Data.Binary
import Data.DeriveTH
import Data.Typeable
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Extension.Browser.PageMode
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Concurrent

data BrowserBuffer =
    BrowserBuffer {browserBufferUri              :: TVar String
                  ,browserBufferClient           :: Client
                  ,browserBufferPageId           :: PageId
                  ,browserBufferMode             :: PageMode
                  ,browserBufferBroadcastChannel :: TChan BrowserBufferSignal
                  ,browserBufferState            :: TVar BrowserState
                  } 
    deriving Typeable

data BrowserState =
    BrowserState {browserStateScrolledPosition  :: (Double, Double)}

data BrowserBufferSignal = SyncUri String
                         | SyncTitle String
                           deriving (Show, Eq, Ord)

-- | Init state.
browserInitState :: BrowserState
browserInitState =
    BrowserState (0, 0)

-- | New browser buffer.
browserBufferNew :: String -> [String] -> Client -> PageId -> CustomizeWrap -> IO BrowserBuffer
browserBufferNew uri _ client pageId _ = do
  -- New buffer.
  buffer <- 
    BrowserBuffer <$> newTVarIO uri
                  <*> pure client
                  <*> pure pageId
                  <*> pure browserMode
                  <*> (newTChanIO :: IO (TChan BrowserBufferSignal))
                  <*> newTVarIO browserInitState

  -- Listen broadcast channel.
  browserBufferListenChannel buffer

  return buffer

-- | Listen broadcast channel.
browserBufferListenChannel :: BrowserBuffer -> IO ()
browserBufferListenChannel BrowserBuffer {browserBufferClient           = client
                                         ,browserBufferPageId           = pageId
                                         ,browserBufferMode             = mode
                                         ,browserBufferBroadcastChannel = channel
                                         } = 
  listenBufferChannel channel $ \ signal -> 
    case signal of
      SyncUri uri -> 
          -- Synchronization url.
          mkDaemonSignal client SynchronizationPathName (SynchronizationPathNameArgs (pageModeName mode) pageId uri)
      SyncTitle title -> 
          -- Synchronization tab title.
          mkDaemonSignal client ChangeTabName (ChangeTabNameArgs (pageModeName mode) pageId title)

-- | Write state.
browserBufferWriteState :: BrowserBuffer -> FilePath -> IO ()
browserBufferWriteState buffer path = do
  state <- readTVarIO $ browserBufferState buffer
  writeConfigPath path state

-- | Read state.
browserBufferReadState :: BrowserBuffer -> FilePath -> IO ()  
browserBufferReadState buffer path = do
  state <- readConfigPath path browserInitState
  writeTVarIO (browserBufferState buffer) state
  
$(derive makeBinary ''BrowserState)
