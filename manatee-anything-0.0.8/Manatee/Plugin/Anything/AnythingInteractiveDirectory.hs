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

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, DeriveDataTypeable, RankNTypes #-}
module Manatee.Plugin.Anything.AnythingInteractiveDirectory where

import Data.ByteString.UTF8 hiding (drop, length)
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.AnythingInteractive
import Manatee.Plugin.Anything.AnythingLocate
import Manatee.Plugin.Anything.Types
import System.FilePath

-- | Search local files.
anythingInteractiveDirectory :: Anything
anythingInteractiveDirectory = 
  Anything {anythingColumnTitle         = ["InteractiveDirectory", "Size"]
           ,anythingColumnFun           = anythingLocateColumnFun
           ,anythingSearch              = AnythingSearch (anythingLocateSearch True)
           ,anythingFilterRule          = anythingLocateFilterRule
           ,anythingCompletionRule      = takeFileName
           ,anythingInputDepend         = True
           ,anythingCommandFun          = anythingInteractiveDirectoryCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Function to generate command list.
anythingInteractiveDirectoryCommandFun :: AnythingCommandFun
anythingInteractiveDirectoryCommandFun _ wrap iType
    = return [("Return directory", anythingInteractiveActionReturn iType filepath)]
      where path     = fileInfoPath $ anythingCandidateUnpack wrap
            filepath = toString path

