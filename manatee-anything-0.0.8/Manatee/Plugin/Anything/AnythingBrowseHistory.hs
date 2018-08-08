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
module Manatee.Plugin.Anything.AnythingBrowseHistory where

import DBus.Client hiding (Signal)
import Data.Typeable
import Data.List.Split
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types
import Network.URI

import qualified Data.Map as M

data BrowseHistory = 
    BrowseHistory {browseHistoryUrl     :: String
                  ,browseHistoryTitle   :: String}
    deriving (Show, Read, Eq, Ord, Typeable)

instance AnythingCandidate BrowseHistory where
  candidateCommandName        = browseHistoryUrl
  candidateFilterName a       = browseHistoryUrl a ++ " " ++ browseHistoryTitle a
  candidateCompletionName a i = 
      let url = browseHistoryUrl a
      -- Smart completion rule. :)
      in if isURI i then url else last $ splitOn "://" url
  candidateExpandName a _     = return $ browseHistoryUrl a

-- | Search browse history.
anythingBrowseHistory :: Anything
anythingBrowseHistory = 
  Anything {anythingColumnTitle         = ["BrowseHistory", "Url"]
           ,anythingColumnFun           = anythingBrowseHistoryColumnFun
           ,anythingSearch              = AnythingSearch anythingBrowseHistorySearch
           ,anythingFilterRule          = anythingFuzzyFilterRule False
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingBrowseHistoryCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search browse history.
anythingBrowseHistorySearch :: AnythingInput -> Client -> IO [BrowseHistory]
anythingBrowseHistorySearch _ _ = do
  (BrowseHistoryList history) <- readConfig browseHistoryPath (BrowseHistoryList M.empty)
  return $ map (uncurry BrowseHistory) (M.toList history)

-- | Function for generate candidate column.
anythingBrowseHistoryColumnFun :: [AnythingColumnFun]
anythingBrowseHistoryColumnFun =
  [browseHistoryTitle . anythingCandidateUnpack
  ,browseHistoryUrl . anythingCandidateUnpack]

-- | Open url.
anythingBrowseHistoryCommandFun :: AnythingCommandFun
anythingBrowseHistoryCommandFun _ (AnythingCandidateWrap a) _ = do
  let uri = candidateCommandName a
  return $ anythingOpenUri uri 
           ++ [("Remove from history",       anythingBrowseHistoryRemove uri)
              ,("Clean History",             anythingBrowseHistoryClean)]

-- | Remove url from history.
anythingBrowseHistoryRemove :: String -> Client -> IO ()
anythingBrowseHistoryRemove uri _ = do
  (BrowseHistoryList history) <- readConfig browseHistoryPath (BrowseHistoryList M.empty)
  writeConfig browseHistoryPath (BrowseHistoryList (M.delete uri history))

-- | Clean history.
anythingBrowseHistoryClean :: Client -> IO ()
anythingBrowseHistoryClean _ = 
  writeConfig browseHistoryPath (BrowseHistoryList M.empty)
