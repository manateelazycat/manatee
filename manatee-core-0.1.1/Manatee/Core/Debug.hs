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

module Manatee.Core.Debug where

import Control.Monad

-- | Debug flag for dbus message.
debugDBusFlag :: Bool
debugDBusFlag = True
-- debugDBusFlag = False

-- | Debug flag for test event.
debugEventFlag :: Bool
debugEventFlag = True

-- | Debug message for dbus message.
debugDBusMessage :: String -> IO ()
debugDBusMessage str = 
  when debugDBusFlag $ putStrLn str

-- | Debug message for event.
debugEventMessage :: String -> IO ()
debugEventMessage str = 
  when debugEventFlag $ putStrLn str
