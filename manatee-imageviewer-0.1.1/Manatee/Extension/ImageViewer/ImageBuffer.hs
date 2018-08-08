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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Extension.ImageViewer.ImageBuffer where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM 
import DBus.Client hiding (Signal)
import Data.Binary
import Data.ByteString.UTF8
import Data.DeriveTH
import Data.List
import Data.Typeable
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Extension.ImageViewer.PageMode
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Concurrent
import System.FilePath
import System.GIO
import System.Glib.MainLoop (HandlerId)
import Text.Regex.TDFA

data ImageBuffer =
    ImageBuffer {imageBufferPath                :: TVar String
                ,imageBufferClient              :: Client
                ,imageBufferPageId              :: PageId
                ,imageBufferMode                :: PageMode
                ,imageBufferFiles               :: TVar [FilePath]
                ,imageBufferBroadcastChannel    :: TChan String
                ,imageBufferSlideShowHanlderId  :: TVar (Maybe HandlerId)
                ,imageBufferState               :: TVar ImageState
                } 
    deriving Typeable

data ImageState =
    ImageState {imageStateDirection             :: ImageDirection
               ,imageStateZoom                  :: Maybe Double
               ,imageStateScrolledPosition      :: (Double, Double)}

data ImageDirection = DirectionUp
                    | DirectionDown
                    | DirectionLeft
                    | DirectionRight

data RotateAction = RotateCounterclockwise
                  | RotateClockwise
                  | RotateMirror

-- | Init state.
imageInitState :: ImageState
imageInitState =
    ImageState DirectionUp Nothing (0, 0)

-- | New image buffer.
imageBufferNew :: String -> [String] -> Client -> PageId -> CustomizeWrap -> IO ImageBuffer
imageBufferNew path _ client pageId _ = do
  -- Create new buffer.
  buffer <- ImageBuffer <$> newTVarIO path
                        <*> pure client
                        <*> pure pageId
                        <*> pure imageMode
                        <*> newTVarIO []
                        <*> (newTChanIO :: IO (TChan String))
                        <*> newTVarIO Nothing
                        <*> newTVarIO imageInitState

  -- Scan image files under current directory.
  forkIO $ imageBufferScanFiles buffer

  -- Listen broadcast channel.
  imageBufferListenChannel buffer

  return buffer

-- | Listen broadcast channel.
imageBufferListenChannel :: ImageBuffer -> IO ()
imageBufferListenChannel ImageBuffer {imageBufferClient                 = client
                                     ,imageBufferPageId                 = pageId
                                     ,imageBufferMode                   = mode
                                     ,imageBufferBroadcastChannel       = channel} = 
  listenBufferChannel channel $ \path ->   
    -- Send SynchronizationPathName signal to daemon process.
    mkDaemonSignal client SynchronizationPathName (SynchronizationPathNameArgs (pageModeName mode) pageId path)
                      
-- | Scan image files under current directory.                        
-- Run this function in back-end thread, then user can open image immediately
-- and don't need wait scan finish.
imageBufferScanFiles :: ImageBuffer -> IO ()
imageBufferScanFiles ImageBuffer {imageBufferPath  = path
                                 ,imageBufferFiles = files} = do
  filePath <- readTVarIO path
  let currentDir = takeDirectory filePath
      dir = fileFromPath (fromString currentDir)
      -- Scan images under current directory.
      -- It will update imageBufferFiles once find next image, 
      -- and not update imageBufferFiles after find all images under current directory.
      -- This behaviour make command imageViewBrowseNext/Prev can browse other image immediately
      -- and don't care the number images under current directory.
      scan enum = do
        fileInfo <- fileEnumeratorNextFile enum Nothing
        case fileInfo of
          Just info -> 
              case fileInfoGetContentType info of
                Just typ -> 
                  if typ =~ ("^image/*" :: String)
                     then do
                       case fileInfoGetName info of
                         -- Update imageBufferFiles if current file is image.
                         Just name -> modifyTVarIO files (\list -> (currentDir </> (toString name)) : list)
                         Nothing -> return ()
                       -- Scan next image.
                       scan enum
                     -- Scan next file if not image file.
                     else 
                         scan enum
                -- Scan next file if can't get content type of file.
                Nothing -> 
                  scan enum
          -- Sort all images after scan last image of current directory.
          Nothing -> 
            modifyTVarIO files sort

  -- Get file enumerate to scan images under current directory.
  enum <- fileEnumerateChildren dir "*" [] Nothing

  -- Scan...
  scan enum
            
-- | Write state.
imageBufferWriteState :: ImageBuffer -> FilePath -> IO ()
imageBufferWriteState buffer path = do
  state <- readTVarIO $ imageBufferState buffer
  writeConfigPath path state

-- | Read state.
imageBufferReadState :: ImageBuffer -> FilePath -> IO ()  
imageBufferReadState buffer path = do
  state <- readConfigPath path imageInitState
  writeTVarIO (imageBufferState buffer) state
  
$(derive makeBinary ''ImageDirection)
$(derive makeBinary ''RotateAction)
$(derive makeBinary ''ImageState)

