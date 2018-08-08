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

module Manatee.Toolkit.Gtk.Container where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk
import Manatee.Toolkit.General.List

-- | Get first child.
containerGetFirstChild :: ContainerClass self => self -> IO (Maybe Widget)
containerGetFirstChild self = do
  children <- containerGetChildren self
  return $ if null children
              then Nothing
              else Just $ head children

-- | Get last child.
containerGetLastChild :: ContainerClass self => self -> IO (Maybe Widget)
containerGetLastChild self = do
  children <- containerGetChildren self
  return $ if null children
              then Nothing
              else Just $ last children

-- | Remove all child of container.
containerRemoveAll :: ContainerClass self => self -> IO ()
containerRemoveAll self = containerForeach self $ containerRemove self

-- | Try to remove child from parent.    
containerTryRemove :: (ContainerClass parent, WidgetClass child) => parent -> child -> IO ()     
containerTryRemove parent widget = do
  hasParent <- widgetGetParent widget
  unless (isNothing hasParent) $ containerRemove parent widget

-- | Apply container size and won't change widget's size.
containerApplySize :: ContainerClass self => self -> (Int -> Int -> IO ()) -> IO ()
containerApplySize container childFun = 
  widgetGetSize container 
  >>= \(width, height) -> do
    -- Run user's function with container size.
    childFun width height

    -- Why set size request with 0?
    -- Answer: because set size request can make container as small as possible,
    -- example, when you un-fullscreen and maximum window, 
    -- window will adjust it's size make it can display in screen area.
    widgetSetSizeRequest container 0 0

-- | Whether has children.
containerHasChildren :: ContainerClass self => self -> IO Bool    
containerHasChildren self = 
  has <$> containerGetChildren self
