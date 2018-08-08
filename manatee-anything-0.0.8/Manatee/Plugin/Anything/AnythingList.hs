-- Author:     Andy Stewart <lazycat.manatee@gmail.com>
-- Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
-- 
-- Copyright (C) 2010 Andy Stewart, all rights reserved.
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

module Manatee.Plugin.Anything.AnythingList where

import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.AnythingApplication
import Manatee.Plugin.Anything.AnythingBrowser
import Manatee.Plugin.Anything.AnythingBrowseHistory
import Manatee.Plugin.Anything.AnythingBuffer
import Manatee.Plugin.Anything.AnythingBufferHistory
import Manatee.Plugin.Anything.AnythingExecuteFile
import Manatee.Plugin.Anything.AnythingGoogleSuggest
import Manatee.Plugin.Anything.AnythingInteractiveChar
import Manatee.Plugin.Anything.AnythingInteractiveDirectory
import Manatee.Plugin.Anything.AnythingInteractiveFile
import Manatee.Plugin.Anything.AnythingInteractiveNumber
import Manatee.Plugin.Anything.AnythingInteractiveString
import Manatee.Plugin.Anything.AnythingIrc
import Manatee.Plugin.Anything.AnythingLocate
import Manatee.Plugin.Anything.AnythingProcess
import Manatee.Plugin.Anything.Types

import qualified Data.Map as M

-- | Anything list.
anythingList :: AnythingList
anythingList = 
    M.fromList $ map (\x -> (anythingName x, x)) 
         [anythingLocate
         ,anythingApplication
         ,anythingProcess
         ,anythingExecuteFile
         ,anythingBrowser
         ,anythingIrc
         ,anythingGoogleSuggest
         ,anythingBuffer
         ,anythingBufferHistory
         ,anythingInteractiveFile
         ,anythingInteractiveDirectory
         ,anythingInteractiveChar
         ,anythingInteractiveString
         ,anythingInteractiveNumber
         ,anythingBrowseHistory
         ]
