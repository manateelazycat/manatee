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

module Manatee.Toolkit.Widget.CompletionWindow where

import Control.Applicative
import Graphics.UI.Gtk
import Manatee.Toolkit.Widget.PopupWindow
import Manatee.Toolkit.Gtk.ScrolledWindow

data CompletionWindow = 
    CompletionWindow {cwWindow                  :: PopupWindow
                     ,cwNotebook                :: Notebook
                     ,cwCandidateScrolledWindow :: ScrolledWindow
                     ,cwCandidateTab            :: TreeView
                     ,cwCommandScrolledWindow   :: ScrolledWindow
                     ,cwCommandTab              :: TreeView
                     }

-- | Candidate tab index in notebook.
candidateTabIndex :: Int
candidateTabIndex = 0

-- | Command tab index in notebook.
commandTabIndex :: Int
commandTabIndex = 0
    
-- | Completion new.    
completionWindowNew :: IO CompletionWindow
completionWindowNew = do
  -- Create component.
  window <- popupWindowNew

  -- Notebook.
  notebook <- notebookNew
  notebookSetTabPos notebook PosBottom
  notebookSetShowTabs notebook False

  -- Candidate tab.
  candidateScrolledWindow <- scrolledWindowNew_
  candidateTab <- treeViewNew

  -- Command tab.
  commandScrolledWindow <- scrolledWindowNew_
  commandTab <- treeViewNew

  -- Connect component.
  pwFrame window `containerAdd` notebook
  candidateScrolledWindow `containerAdd` candidateTab
  commandScrolledWindow `containerAdd` commandTab
  notebookInsertPage notebook candidateScrolledWindow "Candidate Tab" candidateTabIndex
  notebookInsertPage notebook commandScrolledWindow "Command Tab" commandTabIndex
  
  -- Create completion window.
  return $ CompletionWindow window notebook candidateScrolledWindow candidateTab commandScrolledWindow commandTab

-- | Show completion window.
completionWindowShow :: WidgetClass widget => CompletionWindow -> widget -> Rectangle -> IO ()
completionWindowShow (CompletionWindow {cwWindow = win}) 
                     parent rect = do
  popupWindowStickParent win parent
  popupWindowSetAllocation win rect
  popupWindowShow win

-- | Hide completion window.
completionWindowHide :: CompletionWindow -> IO ()
completionWindowHide =
    popupWindowHide . cwWindow
    
-- | Set candidate.
completionWindowSetCandidates :: CompletionWindow -> [(String, [String])] -> IO ()
completionWindowSetCandidates window candidates = 
  return ()
  
-- | Set command.  
completionWindowSetCommands :: CompletionWindow -> [String] -> IO () 
completionWindowSetCommands window commands = 
  return ()

-- | Show candidate tab.
completionWindowShowCandidateTab :: CompletionWindow -> IO ()
completionWindowShowCandidateTab window = 
  notebookSetCurrentPage (cwNotebook window) candidateTabIndex
  
-- | Show command tab.  
completionWindowShowCommandTab :: CompletionWindow -> IO ()
completionWindowShowCommandTab window =
  notebookSetCurrentPage (cwNotebook window) commandTabIndex
  
-- | Is focus candidate tab.
completionWindowIsFocusCandidateTab :: CompletionWindow -> IO Bool
completionWindowIsFocusCandidateTab window =
    (candidateTabIndex ==) <$> notebookGetCurrentPage (cwNotebook window)

-- | Is focus command tab.
completionWindowIsFocusCommandTab :: CompletionWindow -> IO Bool
completionWindowIsFocusCommandTab window =
    (commandTabIndex ==) <$> notebookGetCurrentPage (cwNotebook window)

