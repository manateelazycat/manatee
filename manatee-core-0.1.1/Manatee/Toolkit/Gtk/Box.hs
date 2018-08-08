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

module Manatee.Toolkit.Gtk.Box where

import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk
import Manatee.Toolkit.General.Maybe

-- | Try to packing widget in box.
-- If @child@ have exist parent, do nothing,
-- otherwise, add @child@ to @parent@.
boxTryPack :: (BoxClass parent, WidgetClass child) => parent -> child -> Packing -> Maybe Int -> Maybe Int -> IO ()
boxTryPack box widget packing order space = do
  parent <- widgetGetParent widget
  when (isNothing parent) $ do
    boxPackStart box widget packing (fromMaybe 0 space)
    order ?>= boxReorderChild box widget

-- | Change box child Packing.
boxChangeChildPacking :: (BoxClass parent, WidgetClass child) => parent -> child -> Packing -> IO ()
boxChangeChildPacking box widget packing = do
  (_, index, packType) <- boxQueryChildPacking box widget
  boxSetChildPacking box widget packing index packType

