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

module Manatee.Toolkit.Gtk.Struct where

import Data.Function
import Data.Ord
import Graphics.UI.Gtk.Gdk.DrawWindow (NativeWindowId, fromNativeWindowId, toNativeWindowId)

newtype GWindowId = GWindowId NativeWindowId

instance Show GWindowId where
    show = show . fromNativeWindowId . getInternalWindowId 

instance Eq GWindowId where
    (==) = (==) `on` getInternalWindowId

instance Ord GWindowId where
    compare = comparing (fromIntegral . fromNativeWindowId . getInternalWindowId)

instance Read GWindowId where
    readsPrec _ str = [(GWindowId (toNativeWindowId id), idStr) 
                           | (id, idStr) <- reads str]

instance Num GWindowId where
    (+)         = (+) `on` fromIntegral . fromNativeWindowId . getInternalWindowId
    (*)         = (*) `on` fromIntegral . fromNativeWindowId . getInternalWindowId
    abs         = abs . fromIntegral . fromNativeWindowId . getInternalWindowId
    signum      = signum . fromIntegral . fromNativeWindowId . getInternalWindowId
    fromInteger = GWindowId . toNativeWindowId

-- | Get NativeWindowId.
getInternalWindowId :: GWindowId -> NativeWindowId
getInternalWindowId (GWindowId id) = id
