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

module Manatee.Toolkit.Widget.PopupWindow where

import Control.Applicative hiding (empty)
import Graphics.UI.Gtk
import Manatee.Toolkit.Gtk.Container

data PopupWindow =
    PopupWindow {pwWindow       :: Window
                ,pwFrame        :: Frame}

-- | The default height of popup window.
popupWindowDefaultHeight :: Int  
popupWindowDefaultHeight = 360

-- | Create popup window.
popupWindowNew :: IO PopupWindow
popupWindowNew = do
  -- Create window and frame.
  window <- windowNewPopup
  frame  <- frameNew
  window `containerAdd` frame
  
  -- Set window properties.
  set window [windowTypeHint           := WindowTypeHintDialog
             ,windowWindowPosition     := WinPosCenter
             ,windowDecorated          := False
             ,windowAcceptFocus        := False
             ,windowResizable          := True -- this make window can smaller than last size
             ]

  return $ PopupWindow window frame

-- | Stick popup window with parent window.
-- Note, the widget's toplevel widget must be Window, 
-- otherwise this function with failed.
popupWindowStickParent :: WidgetClass widget => PopupWindow -> widget -> IO ()   
popupWindowStickParent popupWindow widget =
    castToWindow <$> widgetGetToplevel widget
        >>= windowSetTransientFor (pwWindow popupWindow) 

-- | Set size and position.
popupWindowSetAllocation :: PopupWindow -> Rectangle -> IO ()
popupWindowSetAllocation popupWindow (Rectangle x y w h) = do
  let window = pwWindow popupWindow
  windowMove window x y
  widgetSetSizeRequest window w h
  windowResize window w h       -- this step is necessary to make window smaller than last size
  
-- | Show popup window.
popupWindowShow :: PopupWindow -> IO ()
popupWindowShow = widgetShowAll . pwWindow

-- | Hide popup window.
popupWindowHide :: PopupWindow -> IO ()
popupWindowHide = widgetHide . pwWindow

-- | Add child.
popupWindowAdd :: WidgetClass child => PopupWindow -> child -> IO ()
popupWindowAdd popupWindow child =
  pwFrame popupWindow `containerAdd` child

-- | Remove children.
popupWindowRemove :: PopupWindow -> IO ()
popupWindowRemove = containerRemoveAll . pwFrame

-- | Is visible.
popupWindowIsVisible :: PopupWindow -> IO Bool
popupWindowIsVisible win = get (pwWindow win) widgetVisible

-- | Exit.
popupWindowExit :: PopupWindow -> IO ()
popupWindowExit win = do
  popupWindowRemove win
  popupWindowHide win
