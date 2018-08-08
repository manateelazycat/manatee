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
module Manatee.Plugin.Anything.AnythingBrowser where

import DBus.Client hiding (Signal)
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types
import Network.URI

-- | Search uri.
anythingBrowser :: Anything
anythingBrowser =
  Anything {anythingColumnTitle         = ["Browser"]
           ,anythingColumnFun           = [anythingCandidateUnpack]
           ,anythingSearch              = AnythingSearch anythingBrowserSearch
           ,anythingFilterRule          = anythingFuzzyFilterRule True
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingBrowserCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search uri.
anythingBrowserSearch :: AnythingInput -> Client -> IO [String]
anythingBrowserSearch input _ =
  case parseURI input of
       Just uri -> if uriScheme uri /= "irc:" then return [input] else return []
       Nothing  -> 
         return $ if null input then [] else [input]

-- | Browser uri.
anythingBrowserCommandFun :: AnythingCommandFun
anythingBrowserCommandFun _ (AnythingCandidateWrap a) _
    | isURI input
      = return $ 
        anythingOpenUri input
        ++ anythingSearchCommands input
        ++ anythingDownload input
    | isURI httpInput
      = return $ 
        anythingOpenUri httpInput
        ++ anythingSearchCommands input
        ++ anythingDownload httpInput
    | otherwise
      = return $ anythingSearchCommands input
      where input = candidateCommandName a
            httpInput = "http://" ++ input
