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

{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, RankNTypes, OverloadedStrings #-}
module Manatee.Core.PageMode where

import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Toolkit.General.Map
import Text.Regex.TDFA

import qualified Data.Map as M

-- | Default mode.
defaultMode :: PageMode
defaultMode = 
    PageMode {pageModeName      = "Default"
             ,pageModeRegexp    = ""
             ,pageModeKeymap    = M.empty
             ,pageModeLoad      = \_ -> return ()}

-- | Get duplicate tab list.
getDuplicateTabList :: IO [PageModeName]
getDuplicateTabList = do
  (PageModeDuplicateList list) <- readConfig pageModeDuplicateList (PageModeDuplicateList [])
  return list

-- | Get page mode name.
getPageModeName :: PageType -> String -> IO PageModeName
getPageModeName pType path = do
  (PageModeRule ruleMap) <- readConfig pageModeRulePath (PageModeRule M.empty)
  return $ 
    case findMinMatch ruleMap (\ typ _ -> pType == typ) of
      Just (_, rule) -> 
        case rule of
          Left name -> name
          Right map -> 
            case findMinMatch map (\ regexp _ -> path =~ regexp) of
              Just (_, name) -> name
              Nothing -> pageModeName defaultMode
      Nothing -> pageModeName defaultMode
