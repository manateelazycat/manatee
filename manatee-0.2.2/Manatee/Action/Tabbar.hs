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

module Manatee.Action.Tabbar where

import Data.List (find)
import Data.Map (filterWithKey)
import Data.Sequence (Seq)
import Manatee.Core.Types
import Manatee.Types
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Seq

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Map as M

-- | Swap tab.
tabbarSwapTab :: PageModeName -> Int -> Int -> Tabbar -> Tabbar
tabbarSwapTab modeName currentIndex targetIndex (Tabbar tabbar) = 
  Tabbar $ M.mapWithKey
             (\ (_, pageModeName) tabs -> 
                 if pageModeName == modeName
                    then swap currentIndex targetIndex tabs
                    else tabs
             ) tabbar

-- | Remove tab.
tabbarRemoveTab :: PageModeName -> Int -> Tabbar -> Tabbar
tabbarRemoveTab modeName index (Tabbar tabbar) = 
    Tabbar $ M.mapWithKey 
               (\ (_, pageModeName) tabs -> 
                if pageModeName == modeName 
                   then deleteAt index tabs
                   else tabs
               ) tabbar

-- | Add new tab information in tabbar.
tabbarAddTab :: WindowId -> PageModeName -> Tab -> Tabbar -> Tabbar
tabbarAddTab windowId modeName newTab (Tabbar tabbar) = Tabbar newTabbar
    where matchTab = findMinMatch tabbar (\(winId, _) _ -> winId == windowId)
          sequence = case matchTab of
                        -- Add new tab if haven't same mode found.
                        Nothing -> Seq.singleton newTab
                        -- Replace or add new tab.
                        Just (_, seq) -> replaceOrAdd (\x -> tabPageId x == tabPageId newTab) newTab seq
          newTabbar = M.insert (windowId, modeName) sequence tabbar

-- | Remove tab with window id.  
tabbarRemoveTabs :: WindowId -> Tabbar -> Tabbar
tabbarRemoveTabs windowId (Tabbar tabbar) = Tabbar newTabbar
    where newTabbar = filterWithKey (\(winId, _) _ -> winId /= windowId) tabbar

-- | Get tab that match plug id.
tabbarGetTab :: PagePlugId -> Tabbar -> Maybe Tab
tabbarGetTab plugId (Tabbar tabbar) = 
    find (\ tab -> tabPlugId tab == plugId) allTabs
        where allTabList = map snd $ M.toList tabbar
              allTabs    = concatMap F.toList allTabList

-- | Get page mode with special tab.
tabbarGetTabInfo :: Tabbar -> WindowId -> Maybe (PageModeName, Seq Tab)
tabbarGetTabInfo (Tabbar tabbar) windowId = info
    where matchTab = findMinMatch tabbar (\ (wId, _) _ -> wId == windowId)
          info = fmap (\ ((_, name), tabs) -> (name, tabs)) matchTab

-- | Get page mode name with window id.
tabbarGetPageModeName :: Tabbar -> WindowId -> Maybe PageModeName
tabbarGetPageModeName tabbar windowId = 
    maybe Nothing (Just . fst) (tabbarGetTabInfo tabbar windowId)

-- | Get tabs.
tabbarGetTabSeq :: Tabbar -> WindowId -> Maybe (Seq Tab)
tabbarGetTabSeq tabbar windowId =
    maybe Nothing (Just . snd) (tabbarGetTabInfo tabbar windowId)

-- | Get tab list of match window.
tabbarGetTabList :: WindowId -> Tabbar -> [Tab]
tabbarGetTabList windowId tabbar = 
    maybe [] F.toList (tabbarGetTabSeq tabbar windowId)

