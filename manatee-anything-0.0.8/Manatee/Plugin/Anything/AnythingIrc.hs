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

{-# LANGUAGE OverloadedStrings #-}
module Manatee.Plugin.Anything.AnythingIrc where

import DBus.Client hiding (Signal)
import Manatee.Core.DBus
import Manatee.Core.Types 
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types
-- import Network.URI
import Text.Regex.TDFA

-- | Search uri.
anythingIrc :: Anything
anythingIrc =
  Anything {anythingColumnTitle         = ["Irc"]
           ,anythingColumnFun           = [anythingCandidateUnpack]
           ,anythingSearch              = AnythingSearch anythingIrcSearch
           ,anythingFilterRule          = anythingFuzzyFilterRule True
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingIrcCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search uri.
anythingIrcSearch :: AnythingInput -> Client -> IO [String]
anythingIrcSearch input _
    | input =~ ("^irc://.*$" :: String) :: Bool
        = return [input]
    | otherwise
        = return []

-- | Irc uri.
anythingIrcCommandFun :: AnythingCommandFun
anythingIrcCommandFun _ (AnythingCandidateWrap a) _ = 
  return [("Join Channel", anythingIrcJoinChannel (candidateCommandName a))]

-- | Open uri.
anythingIrcJoinChannel :: String -> Client -> IO ()      
anythingIrcJoinChannel info client = 
  mkDaemonSignal client NewTab (NewTabArgs "PageIrc" info [])
