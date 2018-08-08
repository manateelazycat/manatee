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
module Manatee.Toolkit.Widget.KeymapWindow where

import Control.Applicative
import Control.Monad
import Data.Text.Lazy (Text, unpack)
import Graphics.UI.Gtk
import Manatee.Toolkit.Gtk.Container
import Manatee.Toolkit.Gtk.Gtk

data KeymapWindow = 
    KeymapWindow {kwWindow              :: Window
                 ,kwScrolledWindow      :: ScrolledWindow
                 ,kwBox                 :: VBox
                 }

-- | Application name column id.
columnId = makeColumnIdString 0

-- | Keymap window new.
keymapWindowNew :: IO KeymapWindow
keymapWindowNew = do
  -- Init.
  window <- windowNewPopup
  set window [windowTypeHint           := WindowTypeHintDialog
             ,windowDecorated          := False
             ,windowAcceptFocus        := False
             ,windowResizable          := True -- this make window can smaller than last size
             ,windowOpacity            := 0.9
             ]
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyNever PolicyAutomatic
  box <- vBoxNew False 0

  -- Connect widgets.
  connectWidgets [castToContainer window] scrolledWindow 
  scrolledWindowAddWithViewport scrolledWindow box

  return $ KeymapWindow window scrolledWindow box
  
-- | Show keymap window.
keymapWindowShow :: WidgetClass widget 
                   => KeymapWindow 
                   -> widget                       -- module widget
                   -> Rectangle                    -- position and size
                   -> [(String, [(Text, Text)])]   -- keymap
                   -> IO ()
keymapWindowShow (KeymapWindow {kwWindow = window
                               ,kwBox = box}) 
                 widget 
                 (Rectangle x y w h) 
                 keymap = do
  -- Set transient window.
  castToWindow <$> widgetGetToplevel widget
               >>= windowSetTransientFor window

  -- Set window position and size.
  windowMove window x y
  widgetSetSizeRequest window w h
  windowResize window w h

  -- Add keymap.
  forM_ keymap $ \ (keymapTitle, list) -> do
    -- Add keymap title.
    label <- labelNew Nothing
    labelSetMarkup label ("<span foreground='#000000'><big>" ++ keymapTitle ++ "</big></span>")
    boxPackStart box label PackNatural 0 

    -- Add keymap content.
    let items = map (\ (key, command) -> 
                         "<span foreground='#FF7E00' weight='bold'>" 
                         ++ concatMap (\c -> if c == ' ' then "Space" else [c]) (unpack key)
                         ++ "</span>" 
                         ++ " <span foreground='#FFFFFF'>" 
                         ++ unpack command 
                         ++ "</span>")
                list
    store <- listStoreNew items
    customStoreSetColumn store columnId id
    iconView <- iconViewNewWithModel store
    iconViewSetMarkupColumn iconView columnId
    widgetModifyBase iconView StateNormal (Color 15360 15104 14080) 
    boxPackStart box iconView PackGrow 0

  -- Show.
  widgetShowAll window

-- | Hide keymap window.
keymapWindowHide :: KeymapWindow -> IO ()
keymapWindowHide view = do
  widgetHide $ kwWindow view
  containerRemoveAll $ kwBox view

-- | Is visible.
keymapWindowIsVisible :: KeymapWindow -> IO Bool
keymapWindowIsVisible win = get (kwWindow win) widgetVisible

-- | Exit.
keymapWindowExit :: KeymapWindow -> IO ()
keymapWindowExit = widgetDestroy . kwWindow

