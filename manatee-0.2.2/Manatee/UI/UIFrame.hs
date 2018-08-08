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

module Manatee.UI.UIFrame where

import Graphics.UI.Gtk hiding (Statusbar, statusbarNew)
import Manatee.Types
import Manatee.Toolkit.Gtk.Notebook
import Manatee.Toolkit.Widget.NotebookTab

-- | Stick ui frame to notebook.
uiFrameStick :: NotebookClass notebook => notebook -> IO UIFrame
uiFrameStick notebook = do
  -- Create or clone ui frame.
  uiFrame <- uiFrameNew

  -- Stick uiFrame in notebook.
  let notebookTab = uiFrameNotebookTab uiFrame
  notebookAppendPageTab_ notebook (uiFrameBox uiFrame) (ntBox notebookTab)
  notebookSetTabReorderable notebook (uiFrameBox uiFrame) False -- disable reorder tab use DND action
  notebookTabStart notebookTab  -- start spinner animation

  return uiFrame

-- | Create 'UIFrame'.
uiFrameNew :: IO UIFrame
uiFrameNew = do
  -- Create box.
  box <- vBoxNew False 0

  -- Notebook tab. 
  notebookTab <- notebookTabNew Nothing Nothing

  return $ UIFrame box notebookTab
