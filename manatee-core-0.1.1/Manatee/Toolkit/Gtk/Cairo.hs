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

module Manatee.Toolkit.Gtk.Cairo where

import Graphics.Rendering.Cairo

-- | Like `Cairo.rectangle`, but with round corner.
roundRectangle :: Double -> Double -> Double -> Double -> Double -> Render ()
roundRectangle x y w h r = do
  -- Draw top line.
  moveTo (x + r) y
  lineTo (x + w - r) y

  -- Draw bottom line.
  moveTo (x + w - r) (y + h)
  lineTo (x + r) (y + h)

  -- Draw right line.
  moveTo (x + w) (y + r)
  lineTo (x + w) (y + h - r)

  -- Draw left line.
  moveTo x (y + h - r)
  lineTo x (y + r)

  -- Draw top-left corner.
  arcDegree (x + r) (y + r) r 180 270

  -- Draw top-right corner.
  arcDegree (x + w - r) (y + r) r 270 360

  -- Draw bottom-right corner.
  arcDegree (x + w - r) (y + h - r) r 0 90

  -- Draw bottom-left corner
  arcDegree (x + r) (y + h - r) r 90 180

-- | Convert from degree to radian.
degreeToRadian :: Double -> Double
degreeToRadian degrees = degrees * pi / 180

-- | Like `Cairo.arc` except use degree as arguments.
arcDegree :: Double -> Double -> Double -> Double -> Double -> Render ()
arcDegree xc yc radius angleStart angleEnd = 
  arc xc yc radius (degreeToRadian angleStart) (degreeToRadian angleEnd)

