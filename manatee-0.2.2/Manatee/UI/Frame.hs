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

module Manatee.UI.Frame where

import Graphics.UI.Gtk hiding (on, get, Frame)
import Manatee.Toolkit.Gtk.Gtk

type Frame = Window

-- | Create new frame.
frameNew :: IO Frame
frameNew = do
  frame <- windowNew
  windowFullscreen frame
  return frame

-- | Toggle fullscreen.
toggleFullscreen :: Frame -> IO ()
toggleFullscreen frame = do
  -- Get fullscreen states.
  isFullscreen <- windowIsFullscreen frame

  -- Toggle fullscreen.
  if isFullscreen
    then do
      windowUnfullscreen frame
      windowMaximize frame
    else windowFullscreen frame

