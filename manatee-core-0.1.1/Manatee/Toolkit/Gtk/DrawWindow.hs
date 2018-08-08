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

module Manatee.Toolkit.Gtk.DrawWindow where

import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Gdk.DrawWindow

-- | Invalidate rectangle stroke.
drawWindowInvalidateRectStroke :: DrawWindowClass self => self -> Rectangle -> Int -> Bool -> IO () 
drawWindowInvalidateRectStroke drawWin (Rectangle x y w h) lw invalidateChildren = do
  -- Invalid left stroke.
  drawWindowInvalidateRect drawWin (Rectangle x y lw (y + h)) invalidateChildren
  -- Invalid right stroke.
  drawWindowInvalidateRect drawWin (Rectangle (x + w - lw) y lw (y + h)) invalidateChildren
  -- Invalid top stroke.
  drawWindowInvalidateRect drawWin (Rectangle x y (x + w) lw) invalidateChildren
  -- Invalid bottom stroke.
  drawWindowInvalidateRect drawWin (Rectangle x y (x + w) (y + h - lw)) invalidateChildren
