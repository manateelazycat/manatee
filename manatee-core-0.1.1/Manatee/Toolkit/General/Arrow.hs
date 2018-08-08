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

module Manatee.Toolkit.General.Arrow where

import Control.Arrow
import Manatee.Toolkit.General.Basic

-- | Arrow with delicate.
dupA :: Arrow a => a b (b, b)
dupA = arr dup

-- | Arrow with curry.
curryA :: Arrow a => a ((b, c) -> d) (b -> c -> d)
curryA = arr curry

-- | Arrow with uncurry.
uncurryA :: Arrow a => a (b -> c -> d) ((b, c) -> d)
uncurryA = arr uncurry

-- | Arrow with swap.
swapA :: Arrow a => a (b, c) (c, b)
swapA = arr swap

-- | Arrow with liftM2.
liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 f g h = g &&& h >>> arr (uncurry f)

-- | Arrow with id.
idA :: Arrow a => a b b
idA = arr id

-- | Fst with arrow.
fstA :: Arrow a => a (b, c) b
fstA = arr fst

-- | Snd with arrow.
sndA :: Arrow a => a (b, c) c
sndA = arr snd

