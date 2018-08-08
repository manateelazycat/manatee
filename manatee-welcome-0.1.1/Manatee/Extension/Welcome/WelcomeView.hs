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
{-# LANGUAGE OverloadedStrings #-}
module Manatee.Extension.Welcome.WelcomeView where

import Control.Applicative
import Control.Concurrent.STM 
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.PageFrame
import Manatee.Core.Types
import Manatee.Extension.Welcome.WelcomeBuffer
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.ModelView
import Manatee.Toolkit.Gtk.ScrolledWindow
import Paths_manatee_welcome
import System.FilePath

import qualified Data.Map as M

data WelcomeView =
    WelcomeView {welcomeViewPlugId      :: TVar PagePlugId
                ,welcomeViewFrame       :: PageFrame
                ,welcomeViewIconView    :: IconView
                ,welcomeViewBuffer      :: WelcomeBuffer
                }
    deriving Typeable

instance PageBuffer WelcomeBuffer where
    pageBufferGetName           = readTVarIO . welcomeBufferFilePath
    pageBufferSetName a         = writeTVarIO (welcomeBufferFilePath a)
    pageBufferClient            = welcomeBufferClient
    pageBufferCreateView a pId  = PageViewWrap <$> welcomeViewNew a pId
    pageBufferMode              = welcomeBufferMode
    pageBufferPackageName _     = fmap takeFileName getDataDir

instance PageView WelcomeView where
    pageViewBuffer               = PageBufferWrap . welcomeViewBuffer
    pageViewPlugId               = welcomeViewPlugId
    pageViewFrame                = welcomeViewFrame
    pageViewLocalKeymap _        = welcomeViewLocalKeymap
    pageViewLocalCommandMap _    = welcomeViewLocalCommandMap
    pageViewFocus                = widgetGrabFocus . welcomeViewIconView
    pageViewPropagateWidget      = castToWidget . welcomeViewIconView
    pageViewSaveState view       = welcomeViewSaveState view Nothing
    pageViewRestoreState view    = welcomeViewRestoreState view Nothing
    pageViewWriteState view path = welcomeViewSaveState view (Just path)
    pageViewReadState view path  = welcomeViewRestoreState view (Just path)

-- | Internal function for create string buffer.
welcomeViewNew :: WelcomeBuffer -> PagePlugId -> IO WelcomeView
welcomeViewNew buffer plugId = do
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ welcomeBufferMode buffer)

  -- Create Icon view.
  iconView <- iconViewNew
  pageFrameAddChild pFrame iconView

  -- Set list store.
  let store = welcomeBufferStore buffer
  iconViewSetModel iconView (Just store)

  -- Set Icon view callback.
  iconViewSetTextColumn iconView applicationNameColumnId
  iconViewSetPixbufColumn iconView applicationIconColumnId

  -- Set icon view.
  iconViewSetSpacing iconView applicationSpace
  iconViewSetColumns iconView applicationColumns

  -- Start application when item activated.
  iconView `on` itemActivated $ \ [index] -> do
      (_, (pType, pPath, pOption)) <- listStoreGetValue store index
      mkDaemonSignal (pageBufferClient buffer) NewTab (NewTabArgs pType pPath pOption)

  return $ WelcomeView pId pFrame iconView buffer

-- | String buffer keymap.
welcomeViewLocalKeymap :: Map Text Text
welcomeViewLocalKeymap = 
    M.fromList 
         [("h", "Select left")
         ,("l", "Select right")
         ,("j", "Select below")
         ,("k", "Select above")
         ,("K", "Select first")
         ,("J", "Select last")
         ,("m", "Open")]

-- | String buffer keymap.
welcomeViewLocalCommandMap :: Map Text (WelcomeView -> IO ())
welcomeViewLocalCommandMap = 
    M.fromList 
         [("Select left",       iconViewSelectPrevColumnNode . welcomeViewIconView)
         ,("Select right",      iconViewSelectNextColumnNode . welcomeViewIconView)
         ,("Select below",      iconViewSelectNextRowNode . welcomeViewIconView)
         ,("Select above",      iconViewSelectPrevRowNode . welcomeViewIconView)
         ,("Select first",      iconViewSelectFirstNode . welcomeViewIconView)
         ,("Select last",       iconViewSelectLastNode . welcomeViewIconView)
         ,("Open",              iconViewActivatedCursor . welcomeViewIconView)]

-- | Scrolled window.
welcomeViewScrolledWindow :: WelcomeView -> ScrolledWindow
welcomeViewScrolledWindow =
  pageFrameScrolledWindow . welcomeViewFrame

-- | Save state.
welcomeViewSaveState :: WelcomeView -> Maybe FilePath -> IO ()
welcomeViewSaveState view@(WelcomeView {welcomeViewBuffer     = buffer})  
                     statePath = do
  -- Get scroll position.
  scrolledWindowPosition <- scrolledWindowGetValue (welcomeViewScrolledWindow view)

  -- Save state.
  let state = WelcomeState scrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (welcomeBufferState buffer) state
    Just path -> writeConfigPath path state

-- | Restore state.
welcomeViewRestoreState :: WelcomeView -> Maybe FilePath -> IO ()
welcomeViewRestoreState view@(WelcomeView {welcomeViewBuffer  = buffer})
                        statePath = do
  bufferState <- readTVarIO (welcomeBufferState buffer)
  (WelcomeState scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore scroll position.
  scrolledWindowSetValue (welcomeViewScrolledWindow view) scrolledWindowPosition
