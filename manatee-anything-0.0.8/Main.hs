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

{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, TypeSynonymInstances, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.UI.Gtk
import Manatee.Core.Types
import Manatee.Plugin.Anything.Main
import System.Environment

-- | main entry.
main :: IO ()
main = do
  -- Init.
  unsafeInitGUIForThreadedRTS
  
  -- Get program arguments.
  args <- getArgs

  case args of
    [x] -> do
         case (read x :: SpawnProcessArgs) of
           SpawnAnythingProcessArgs arg -> do
               let (iType, iNames) = 
                       case arg of
                         InteractiveSearchArgs iType iNames -> (iType, iNames)
                         -- 'GlobalSearchArgs' use default candidate name list.
                         GlobalSearchArgs -> 
                             (GlobalSearch, 
                             ["Irc"
                             ,"Locate"
                             ,"Buffer"
                             ,"BrowseHistory"
                             ,"BufferHistory"
                             ,"Browser"
                             ,"GoogleSuggest"
                             ,"Application"
                             ,"ExecuteFile"
                             ,"Process"
                             ])
               anythingMain iType iNames
           _ -> return ()
    _ -> return ()
