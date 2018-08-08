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

module Config.User where

-- | Default download thread number.
defaultThreadNumber :: Int
defaultThreadNumber = 8

-- | Default cache size (bytes).
-- Default is 512K.
-- This cache value is for thread, 
-- max memory is bigger if you use more threads.
-- Of course, memory will release after flush cache to disk.
-- NOTE, don't set value too little, 
-- it will cause too many I/O operation if cache size is less than download speed.
defaultCacheSize :: Int
defaultCacheSize = 
  512 * 2 ^ 10

-- | Whether automatic start download file when startup process?
-- Default is 'False'.
autoStart :: Bool
autoStart = False
