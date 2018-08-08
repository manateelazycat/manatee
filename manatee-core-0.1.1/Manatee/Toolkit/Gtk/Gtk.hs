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

module Manatee.Toolkit.Gtk.Gtk where

import Control.Applicative hiding (empty)
import Data.Maybe 
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew)
import Graphics.UI.Gtk.Gdk.SerializedEvent
import Graphics.UI.Gtk.SourceView.SourceLanguage
import Graphics.UI.Gtk.SourceView.SourceLanguageManager
import Manatee.Toolkit.General.Basic

-- | Get source language along with file name.
sourceLanguageForFilename :: SourceLanguageManager -> Maybe String -> IO (Maybe String, Maybe SourceLanguage)
sourceLanguageForFilename lm filename = do
    mbLang <- 
        case filename of
          Just f  -> sourceLanguageManagerGuessLanguage lm (Just f) Nothing
          Nothing -> sourceLanguageManagerGuessLanguage lm Nothing (Just "text/x-text")
    case mbLang of
        Nothing -> return (Nothing,Nothing)
        Just lang -> do
            name <- sourceLanguageGetName lang
            return (Just name, Just lang)

-- | Add window with special window group.
windowNewWithWindowGroup :: WindowGroup -> IO Window
windowNewWithWindowGroup windowGroup = do
  window <- windowNew
  windowGroupAddWindow windowGroup window
  return window

-- | Get x coordinate of Rectangle.
rectangleX :: Rectangle -> Int
rectangleX (Rectangle x _ _ _) = x

-- | Get y coordinate of Rectangle.
rectangleY :: Rectangle -> Int
rectangleY (Rectangle _ y _ _) = y

-- | Get width of Rectangle.
rectangleW :: Rectangle -> Int
rectangleW (Rectangle _ _ w _) = w

-- | Get height of Rectangle.
rectangleH :: Rectangle -> Int
rectangleH (Rectangle _ _ _ h) = h

-- | This function like `socketNew`, 
-- but call `widgetShow` when create.
-- Because Gtk+ said must show GtkScoekt before add to container.
socketNew_ :: IO Socket
socketNew_ = do
  socket <- socketNew
  widgetShow socket
  return socket

-- | Window is full-screen.
-- Notes, just call this function when you have realized window.
windowIsFullscreen :: Window -> IO Bool
windowIsFullscreen window = 
  widgetGetDrawWindow window 
  >>= drawWindowGetState
  >>= \states ->  
      return $ WindowStateFullscreen `elem` states

-- | Frame new with shadow type.
frameNewWithShadowType :: Maybe ShadowType -> IO Frame
frameNewWithShadowType Nothing = frameNew
frameNewWithShadowType (Just shadow) = do
  frame <- frameNew
  frameSetShadowType frame shadow
  return frame

-- | Redraw rectangle frame.
widgetRedrawRectangleFrame :: WidgetClass self => self 
                           -> Int -- rectangle x
                           -> Int -- rectangle y
                           -> Int -- rectangle width
                           -> Int -- rectangle height
                           -> Int -- rectangle line width
                           -> IO () 
widgetRedrawRectangleFrame widget x y w h lw = do
  -- Redraw left stroke.
  widgetQueueDrawArea widget x y lw (y + h) 
  -- Redraw right stroke.
  widgetQueueDrawArea widget (x + w - lw) y lw (y + h) 
  -- Redraw top stroke.
  widgetQueueDrawArea widget x y (x + w) lw 
  -- Redraw bottom stroke.
  widgetQueueDrawArea widget x (y + h - lw) (x + w) lw

-- | Whether the widget has parent.
widgetHasParent :: WidgetClass widget => widget -> IO Bool
widgetHasParent widget = 
  isJust <$> widgetGetParent widget

-- | Propagate event on specify widget.
widgetPropagateEvent :: WidgetClass widget => widget -> SerializedEvent -> IO ()
widgetPropagateEvent widget sEvent = do
  drawWindow <- widgetGetDrawWindow widget
  postGUIAsync $ deserializeEvent sEvent drawWindow (widgetEvent widget) >> return ()

-- | Get screen size.
widgetGetScreenSize :: WidgetClass widget => widget -> IO (Int, Int)
widgetGetScreenSize widget = do
  screen <- widgetGetScreen widget
  width  <- screenGetWidth screen
  height <- screenGetHeight screen
  return (width, height)

-- | Get default screen size.
getDefaultScreenSize :: IO (Int, Int)  
getDefaultScreenSize = do
  displayManager <- displayManagerGet
  display <- get displayManager displayManagerDefaultDisplay
  screen <- displayGetDefaultScreen display
  width <- screenGetWidth screen
  height <- screenGetHeight screen
  return (width, height)

-- | Create image widget with given icon name and size. 
imageNewFromIcon :: String -> Int -> IO Image
imageNewFromIcon iconName size = do
  pixbuf <- pixbufNewFromIcon iconName size
  imageNewFromPixbuf pixbuf

-- | Get pixbuf from give icon name and size.
pixbufNewFromIcon :: String -> Int -> IO Pixbuf
pixbufNewFromIcon iconName size = do
  iconTheme <- iconThemeGetDefault
  -- Function 'iconThemeLoadIcon' can scale icon with specified size.
  pixbuf <- iconThemeLoadIcon iconTheme iconName size IconLookupUseBuiltin
  case pixbuf of
    Just p  -> return p
    Nothing -> error $ "pixbufNewFromIcon : search icon " ++ iconName ++ " failed."

-- | Set position of paned.
panedAdjustSize :: PanedClass self => self -> Int -> IO ()
panedAdjustSize paned adjustSize = do
  -- Get position of paned.
  position    <- get paned panedPosition
  minPosition <- get paned panedMinPosition
  maxPosition <- get paned panedMaxPosition
  -- Calculate new position.
  let adjustPosition = position + adjustSize
      newPosition 
        -- Don't lower than min position.
        | adjustPosition < minPosition = minPosition
        -- Don't bigger than max position.
        | adjustPosition > maxPosition = maxPosition
        | otherwise = adjustPosition
  -- Set new position to parent node paned.
  panedSetPosition paned newPosition

-- | Convert Color to RGB value.
colorToRGB :: Color -> (Double, Double, Double)
colorToRGB (Color r g b) = 
  (i2d r / 65535, i2d g / 65535, i2d b / 65535)

-- | Get pixbuf snapshot of widget.
widgetGetSnapshotPixbuf :: WidgetClass widget => widget -> IO (Maybe Pixbuf)
widgetGetSnapshotPixbuf widget = do
  rect@(Rectangle x y w h) <- widgetGetAllocation widget
  pixmap <- widgetGetSnapshot widget rect
  case pixmap of
    Just d -> pixbufGetFromDrawable d (Rectangle 0 0 (w - x) (h - y))
    Nothing -> return Nothing 

-- | Connect widget.
connectWidgets :: (ContainerClass container, WidgetClass widget) => [container] -> widget -> IO ()
connectWidgets [] _ = return ()
connectWidgets [container] widget = 
    container `containerAdd` widget
connectWidgets (x:y:z) widget = do
  x `containerAdd` y 
  connectWidgets (y:z) widget
