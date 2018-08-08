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

module Manatee.Extension.IrcClient.HighlightNick where

import Data.Digest.OpenSSL.MD5
import Data.List.Split
import Graphics.UI.Gtk
import Manatee.Toolkit.General.String

import qualified Data.ByteString.Char8 as B

type NickColor = (Int, Int , Int)

-- | Background color.
backgroundColor :: NickColor
backgroundColor = (65335, 65335, 65335)
-- backgroundColor = (0, 0, 0)

-- | Returns the luminance of color. 
-- Luminance is a value of 0.299 red + 0.587 green + 0.114 blue 
-- and is always between 0 and 255.
colorLuminance :: NickColor -> Int
colorLuminance (r, g, b) = 
    floor ((fromIntegral r * 0.299 + fromIntegral g * 0.587 + fromIntegral b * 0.114) / 256)

-- | Returns the inverted color.
invertColor :: NickColor -> NickColor
invertColor (r, g, b) =
  (65535 - r, 65535 - g, 65535 - b)

-- | Get unique color of nick. 
nickColor :: B.ByteString -> NickColor
nickColor nick
    -- Invert color if too dark or too bright.
    | abs (luminance - backgroundLuminance) < 85
        = invertColor color
    | otherwise 
        = color
  where hash      = take 12 $ md5sum nick 
        [r, g, b] = splitEvery 4 hash
        color     = (hexStringToInt r
                    ,hexStringToInt g
                    ,hexStringToInt b)
        luminance = colorLuminance color
        backgroundLuminance = colorLuminance backgroundColor

-- | Convert nick color to color.
nickColorToColor :: NickColor -> Color        
nickColorToColor (r, g, b) =
  Color (fromIntegral r)
        (fromIntegral g)
        (fromIntegral b)