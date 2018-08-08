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

module Config.Default where

import Manatee.Extension.Reader.Types

import qualified Data.Map as M

-- | Temp feed information, instead with user customize option after customize system complete. 
feedInfos :: FeedDB
feedInfos = 
  M.fromList [("Planet Haskell"
              ,FeedInfo "http://planet.haskell.org/rss20.xml" 
                        [Minute 30])
             ,("HackageDB recent additions"
              ,FeedInfo "http://hackage.haskell.org/packages/archive/recent.rss"
                        [Minute 20])
             ,("Planet Emacsen"
              ,FeedInfo "http://planet.emacsen.org/atom.xml"
                        [Minute 20])
             ,("Aqee.net"
              ,FeedInfo "http://feed.feedsky.com/aqee-net"
                        [Minute 30]
              )
             ,("Yeeyan-Science"
              ,FeedInfo "http://feed.feedsky.com/yeeyan-science"
                        [Minute 20])
             ,("Yeeyan-Tech"
              ,FeedInfo "http://feed.feedsky.com/yeeyan-tech"
                        [Minute 20])
             ]
