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

module Manatee.Core.PageFrame where

import Control.Concurrent.STM 
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew)
import Manatee.Core.Types
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.String
import Manatee.Toolkit.Gtk.Gtk
import Manatee.Toolkit.Gtk.ScrolledWindow
import Manatee.Toolkit.Widget.Interactivebar
import Manatee.Toolkit.Widget.Outputbar
import Manatee.Toolkit.Widget.CompletionWindow
import Manatee.Toolkit.Widget.KeymapWindow
import Manatee.Toolkit.Widget.Statusbar

-- | Auto completion enable color.
autoCompletionEnableColor :: Color
autoCompletionEnableColor = Color 61440 30464 17920

-- | Auto completion disable color.
autoCompletionDisableColor :: Color
autoCompletionDisableColor = Color 0 0 0

-- | Create 'PageFrame'.
pageFrameNew :: IO PageFrame
pageFrameNew = do
  -- Create box.
  box <- vBoxNew False 0

  -- Interactivebar
  interactivebar <- interactivebarNew

  -- Body frame.
  frame <- frameNewWithShadowType Nothing
  boxPackStart box frame PackGrow 0 

  -- Scrolled Window.
  scrolledWindow <- scrolledWindowNew_
  frame `containerAdd` scrolledWindow

  -- Outputbar.
  outputbar <- outputbarNew
  
  -- Statusbar.
  statusbar <- statusbarNew box

  -- Completion window.
  autoCompletion <- newTVarIO False
  completionWindow <- completionWindowNew

  -- Interactive args.
  args   <- newTVarIO []
  typ    <- newTVarIO IDefault
  result <- newTVarIO []
  func   <- newTVarIO (\_ -> return ())

  -- Keymap window.
  keymapWin <- keymapWindowNew

  -- Destory keymap window when page destory.
  box `onDestroy` keymapWindowExit keymapWin

  -- Hide keymap window when page frame parent changed.
  -- In most case, box's parent will changed when got
  -- `ReparentRenderPage` DBus signal, 
  -- look the source code of `Manatee.Core.Render.renderHandleReparentPage` :)
  box `on` parentSet $ \_ -> keymapWindowHide keymapWin

  return $ PageFrame box interactivebar frame 
                     scrolledWindow outputbar statusbar 
                     autoCompletion completionWindow
                     args typ result func keymapWin

-- | Page frame new with mode name.
pageFrameNewWithModeName :: String -> IO PageFrame
pageFrameNewWithModeName name = do
  frame <- pageFrameNew
  pageFrameUpdateStatusbar frame "PageMode" ("Mode (" ++ name ++ ")")
  return frame

-- | Show interactivebar.
pageFrameShowInteractivebar :: PageFrame -> IO ()  
pageFrameShowInteractivebar pageFrame =
    interactivebarShow (pageFrameBox pageFrame)
                       (pageFrameInteractivebar pageFrame)

-- | Add child.
pageFrameAddChild :: WidgetClass widget => PageFrame -> widget -> IO ()  
pageFrameAddChild pageFrame widget =
  pageFrameScrolledWindow pageFrame `containerAdd` widget

-- | Show outputbar.
pageFrameShowOutputbar :: PageFrame -> String -> Maybe Int -> IO ()
pageFrameShowOutputbar pageFrame output limit = do
  let str = case limit of
              Just l  -> stripFormat output l
              Nothing -> output
  outputbarShow (pageFrameBox pageFrame) 
                (pageFrameOutputbar pageFrame) 
                str

-- | Update statusbar.
pageFrameUpdateStatusbar :: PageFrame -> String -> String -> IO ()
pageFrameUpdateStatusbar pageFrame = 
    statusbarInfoItemUpdate (pageFrameStatusbar pageFrame) 

-- | Update progress.
pageFrameUpdateProgress :: PageFrame -> Double -> IO ()
pageFrameUpdateProgress pageFrame = 
    statusbarProgressUpdate (pageFrameStatusbar pageFrame) 

-- | Close interactivebar.
pageFrameCloseInteractivebar :: PageFrame -> IO ()
pageFrameCloseInteractivebar pageFrame = do
  -- Close interactivebar.
  interactivebarExit (pageFrameBox pageFrame) (pageFrameInteractivebar pageFrame)

  -- Clean interactive data.
  writeTVarIO (pageFrameInteractiveArgs pageFrame) []
  writeTVarIO (pageFrameInteractiveType pageFrame) IDefault
  writeTVarIO (pageFrameInteractiveResult pageFrame) []
  writeTVarIO (pageFrameInteractiveFun pageFrame) (\_ -> return ())

-- | Switch auto completion.
pageFrameSwitchAutoCompletion :: PageFrame -> IO ()
pageFrameSwitchAutoCompletion pageFrame = do
  title <- interactivebarGetTitle (pageFrameInteractivebar pageFrame)
  let label = interactivebarTitleLabel $ pageFrameInteractivebar pageFrame
      titleLen = length title
  autoCompletion <- readTVarIO $ pageFrameAutoCompletion pageFrame
  case autoCompletion of
    True -> do
      set label [labelAttributes := [AttrForeground 0 titleLen autoCompletionDisableColor]]
      writeTVarIO (pageFrameAutoCompletion pageFrame) False
    False -> do
      set label [labelAttributes := [AttrForeground 0 titleLen autoCompletionEnableColor]]
      writeTVarIO (pageFrameAutoCompletion pageFrame) True
