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

{-# LANGUAGE DeriveDataTypeable #-}
module Manatee.Extension.Reader.Types where

import Control.Concurrent.STM 
import Data.Map (Map)
import Data.Typeable
import Manatee.Core.Types
import Manatee.Toolkit.General.STM
import Unsafe.Coerce

data ReaderCustomize = 
    ReaderCustomize {readerCustomizeFeedInfos   :: TVar FeedDB
                    }
    deriving Typeable

instance Customize ReaderCustomize where 
  customizeConfigFile _ = "Reader.hs"
  customizeLoad a       =
    [("feedInfos"
     ,\v -> writeTVarIO (readerCustomizeFeedInfos a) (unsafeCoerce v :: FeedDB))]

type FeedDB = Map FeedName FeedInfo

data Interval = Second Int
              | Minute Int
              | Hour   Int
              | Day    Int
                deriving (Show, Eq, Ord)

type FetchNumber = Int
type FeedUrl     = String
type FeedName    = String

data FeedInfo =
    FeedInfo {feedUrl         :: FeedUrl           -- feed url
             ,feedInterval    :: [Interval]        -- interval time between retrievals
             }
    deriving Show

