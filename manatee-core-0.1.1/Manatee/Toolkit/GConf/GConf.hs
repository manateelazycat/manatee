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

module Manatee.Toolkit.GConf.GConf where

import Data.Char
import System.Gnome.GConf

data BackgroundOption = None
                      | Wallpaper
                      | Centered
                      | Scaled
                      | Stretched
                      | Zoom
                      | Spanned
                        deriving (Show, Ord, Eq, Read)

-- | Set background, just for gnome.
setDesktopBackground :: FilePath -> Int -> BackgroundOption -> IO ()
setDesktopBackground imagePath opacity option = do
  conf <- gconfGetDefault
  gconfSet conf "/desktop/gnome/background/picture_filename" imagePath
  gconfSet conf "/desktop/gnome/background/picture_opacity" opacity
  gconfSet conf "/desktop/gnome/background/picture_options" (map toLower (show option))
