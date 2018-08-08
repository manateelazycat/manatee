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
module Manatee.Plugin.Anything.AnythingInteractiveString where

import DBus.Client hiding (Signal)
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.AnythingInteractive
import Manatee.Plugin.Anything.Types

-- | Search interactive string. 
anythingInteractiveString :: Anything
anythingInteractiveString = 
  Anything {anythingColumnTitle         = ["InteractiveString"]
           ,anythingColumnFun           = [anythingCandidateUnpack]
           ,anythingSearch              = AnythingSearch anythingInteractiveStringSearch
           ,anythingFilterRule          = anythingFuzzyFilterRule True
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingInteractiveStringCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search function for string.
anythingInteractiveStringSearch :: AnythingInput -> Client -> IO [String]
anythingInteractiveStringSearch input _ 
    | null input
        = return []
    | otherwise
        = return [input]

-- | Function to generate command list.
anythingInteractiveStringCommandFun :: AnythingCommandFun
anythingInteractiveStringCommandFun input _ iType
    = return [("Return String", anythingInteractiveActionReturn iType input)]
