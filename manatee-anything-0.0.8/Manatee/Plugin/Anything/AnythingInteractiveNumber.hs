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
module Manatee.Plugin.Anything.AnythingInteractiveNumber where

import Data.Char
import DBus.Client hiding (Signal)
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.AnythingInteractive
import Manatee.Plugin.Anything.Types

-- | Search interactive number.
anythingInteractiveNumber :: Anything
anythingInteractiveNumber = 
  Anything {anythingColumnTitle         = ["InteractiveNumber"]
           ,anythingColumnFun           = [anythingCandidateUnpack]
           ,anythingSearch              = AnythingSearch anythingInteractiveNumberSearch
           ,anythingFilterRule          = anythingFuzzyFilterRule True
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingInteractiveNumberCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search function for number.
anythingInteractiveNumberSearch :: AnythingInput -> Client -> IO [String]
anythingInteractiveNumberSearch input _ 
    | null input
        = return []
    | all isNumber input
        = return [input]
    | otherwise
        = return []

-- | Function to generate command list.
anythingInteractiveNumberCommandFun :: AnythingCommandFun
anythingInteractiveNumberCommandFun input _ iType
    = return [("Return Number", anythingInteractiveActionReturn iType input)]
