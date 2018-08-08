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

{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, TypeSynonymInstances, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Manatee.Environment where

import Control.Applicative
import Control.Concurrent.STM 
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Toolkit.General.DBus 
import Manatee.Types
import Manatee.UI.FocusNotifier
import Manatee.UI.Frame
import Manatee.UI.Window hiding (windowNew)
import Manatee.UI.WindowNode

import qualified Data.Map as M
import qualified Data.Set as Set

-- | Build environment arguments.
-- This is strut for manage miscellaneous state in Daemon process.
mkEnvironment :: IO Environment
mkEnvironment = 
  Environment <$> frameNew  
              <*> mkSessionClientWithName daemonBusName
              <*> newTVarIO windowListNew
              <*> newTVarIO windowNodeListNew
              <*> newTVarIO (Tabbar M.empty)
              <*> newTVarIO (TabbarSelect M.empty)
              <*> newTVarIO (BufferList M.empty)
              <*> newTVarIO Set.empty
              <*> newTVarIO 0
              <*> newTVarIO 0
              <*> newTVarIO (FocusNotifierList Set.empty Nothing)
              <*> newTVarIO (TabCloseHistory [])
              <*> newTVarIO []
