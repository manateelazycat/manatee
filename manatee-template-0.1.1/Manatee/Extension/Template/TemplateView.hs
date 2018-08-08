-- Author:     Your Name <YourMail>
-- Maintainer: Your Name <YourMail>
-- 
-- Copyright (C) 2010 Your Name, all rights reserved.
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

{-# LANGUAGE ExistentialQuantification, RankNTypes, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Manatee.Extension.Template.TemplateView where

import Control.Applicative
import Control.Concurrent.STM 
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Manatee.Core.Config
import Manatee.Core.PageFrame
import Manatee.Core.Types
import Manatee.Extension.Template.TemplateBuffer
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Multiline
import Paths_manatee_template
import System.FilePath

import qualified Data.Map as M
import qualified Graphics.UI.Gtk.SourceView.SourceView as SV

data TemplateView =
    TemplateView {templateViewPlugId          :: TVar PagePlugId
                 ,templateViewFrame           :: PageFrame
                 ,templateViewView            :: SV.SourceView
                 ,templateViewBuffer          :: TemplateBuffer
                 }
    deriving Typeable

instance PageBuffer TemplateBuffer where
    -- Get buffer name from Template buffer.
    pageBufferGetName           = readTVarIO . templateBufferFilePath

    -- Set buffer name.
    pageBufferSetName a         = writeTVarIO (templateBufferFilePath a)

    -- DBus client.
    pageBufferClient            = templateBufferClient

    -- How to create TemplateView from TemplateBuffer for multiple view design.
    pageBufferCreateView a pId  = PageViewWrap <$> templateViewNew a pId

    -- Page mode.
    pageBufferMode              = templateBufferMode

    -- Get package name to update user's configure.
    pageBufferPackageName _     = fmap takeFileName getDataDir

    -- Save buffer state.
    pageBufferWriteState        = templateBufferWriteState
    
    -- Restore buffer state.
    pageBufferReadState         = templateBufferReadState

instance PageView TemplateView where
    -- Page buffer.
    pageViewBuffer                = PageBufferWrap . templateViewBuffer
                                  
    -- GtkPlug id.                
    pageViewPlugId                = templateViewPlugId
                                  
    -- Page Frame.                
    pageViewFrame                 = templateViewFrame
                                  
    -- Local keymap.              
    pageViewLocalKeymap _         = templateViewLocalKeymap

    -- Command map.
    pageViewLocalCommandMap _     = templateViewLocalCommandMap

    -- How to focus when manatee focus current application.
    pageViewFocus                 = widgetGrabFocus . templateViewView

    -- Which widget to propagate event.
    pageViewPropagateWidget       = castToWidget . templateViewView
                                 
    -- Save view state.          
    pageViewSaveState view        = templateViewSaveState view Nothing

    -- Restore view state.
    pageViewRestoreState view     = templateViewRestoreState view Nothing

    -- Save view state to file.
    pageViewWriteState view path  = templateViewSaveState view (Just path)

    -- Restore view state from file.
    pageViewReadState view path   = templateViewRestoreState view (Just path)

    -- How to handle cut action, can ignore.
    -- pageViewCut                 = templateViewCut

    -- How to handle copy action, can ignore.
    -- pageViewCopy                = templateViewCopy

    -- How to handle paste action, can ignore.
    -- pageViewPaste               = templateViewPaste

    -- How to scroll top postion, can ignore.
    -- pageViewScrollToTop         = templateViewScrollToTop

    -- How to scroll bottom postion, can ignore.
    -- pageViewScrollToBottom      = templateViewScrollToBottom

    -- How to scroll vertical page, can ignore.
    -- pageViewScrollVerticalPage  = templateViewScrollVerticalPage

    -- How to scroll vertical step, can ignore.
    -- pageViewScrollVerticalStep  = templateViewScrollVerticalStep

    -- How to scroll to left side, can ignore.
    -- pageViewScrollToLeft        = templateViewScrollToLeft

    -- How to scroll to right side, can ignore.
    -- pageViewScrollToRight       = templateViewScrollToRight

    -- How to scroll horizontal page, can ignore.
    -- pageViewScrollHorizontalPage= templateViewScrollHorizontalPage

    -- How to scroll horizontal step, can ignore.
    -- pageViewScrollHorizontalStep= templateViewScrollHorizontalStep


-- | Internal function for create string buffer.
templateViewNew :: TemplateBuffer -> PagePlugId -> IO TemplateView
templateViewNew sb plugId = do
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ templateBufferMode sb)

  templateView <- SV.sourceViewNewWithBuffer (templateBufferBuffer sb)
  pageFrameAddChild pFrame templateView

  return $ TemplateView pId pFrame templateView sb

-- | String buffer keymap.
templateViewLocalKeymap :: Map Text Text
templateViewLocalKeymap = 
    M.fromList []

-- | String buffer keymap.
templateViewLocalCommandMap :: Map Text (TemplateView -> IO ())
templateViewLocalCommandMap = 
    M.fromList []

-- | Template view scrolled window.
templateViewScrolledWindow :: TemplateView -> ScrolledWindow    
templateViewScrolledWindow =
  pageFrameScrolledWindow . templateViewFrame

-- | Save state.
templateViewSaveState :: TemplateView -> Maybe FilePath -> IO ()
templateViewSaveState view@(TemplateView {templateViewBuffer  = buffer}) 
                      statePath = do
  -- Get scroll position.
  scrolledWindowPosition <- textViewGetCursorAlign (templateViewView view)

  -- Save state.
  let state = TemplateState Nothing scrolledWindowPosition
  case statePath of
       Nothing   -> writeTVarIO (templateBufferState buffer) state
       Just path -> writeConfigPath path state

-- | Restore state.
templateViewRestoreState :: TemplateView -> Maybe FilePath -> IO ()
templateViewRestoreState TemplateView {templateViewBuffer  = buffer
                                      ,templateViewView    = textView} 
                         statePath = do
  bufferState <- readTVarIO (templateBufferState buffer)
  (TemplateState cursor scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore cursor.
  cursor ?>= \ (line, column) -> do
    textViewGotoLine textView line
    textViewGotoColumn textView column

  -- Restore cursor alignment.
  textViewSetCursorAlign textView scrolledWindowPosition

