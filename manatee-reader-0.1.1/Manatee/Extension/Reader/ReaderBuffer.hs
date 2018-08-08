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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Extension.Reader.ReaderBuffer where

import Codec.Binary.UTF8.String
import Config.Import
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM 
import Control.Monad
import DBus.Client hiding (Signal)
import Data.Binary
import Data.DeriveTH
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Typeable
import Graphics.UI.Gtk hiding (get)
import Manatee.Core.Config
import Manatee.Core.Dynload
import Manatee.Core.Types
import Manatee.Extension.Reader.PageMode
import Manatee.Extension.Reader.Types
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Network.Curl.Download
import Text.Feed.Query
import Text.Feed.Types

import qualified Data.Map as M

data ReaderBuffer =
    ReaderBuffer {readerBufferName              :: String
                 ,readerBufferClient            :: Client
                 ,readerBufferPageId            :: PageId
                 ,readerBufferMode              :: PageMode
                 ,readerBufferFeedInfos         :: Map FeedName FeedInfo
                 ,readerBufferFeedTreeItems     :: TVar [FeedTreeItem]
                 ,readerBufferFeedTreeOptions   :: [(TreeItemOption, SortColumnId)]
                 ,readerBufferFeedNavItems      :: TVar (Map FeedName [FeedNavItem])
                 ,readerBufferFeedNavOptions    :: [(NavItemOption, SortColumnId)]
                 ,readerBufferBroadcastChannel  :: TChan ReaderTChanSignal
                 ,readerBufferCustomize         :: ReaderCustomize
                 ,readerBufferState             :: TVar ReaderState
                 }
    deriving Typeable

data ReaderState =
    ReaderState {readerStateFeedSelectedPath    :: (Maybe TreePath) 
                ,readerStateItemSelectedPath    :: (Maybe TreePath)
                ,readerStateItemScrolledPosition:: (Double, Double)
                ,readerStateWebScrolledPosition :: (Double, Double)
                }

data ReaderTChanSignal = FeedUpdated FeedName
                         deriving (Show, Eq, Ord)

data FeedTreeItem = 
    FeedTreeItem {ftiName         :: String
                 ,ftiUnreadNumber :: Int
                 ,ftiUpdateTime   :: Int
                 }
    deriving Show

data TreeItemOption = TName
                      deriving (Eq, Show, Read)

class TreeItemClass a where
  tiGetColumnTitle        :: a -> String
  tiGetCellText           :: a -> FeedTreeItem -> String
  tiGetCellXAlign         :: a -> Float
  tiCompareRow            :: a -> FeedTreeItem -> FeedTreeItem -> IO Ordering

instance TreeItemClass TreeItemOption where
  tiGetColumnTitle TName         = "Feed" 
  tiGetCellText TName item       = ftiName item ++ " (" ++ show (ftiUnreadNumber item) ++ ")"
  tiGetCellXAlign TName          = 0.0
  tiCompareRow TName item1 item2 = return $ comparing ftiName item1 item2

data FeedNavItem = 
    FeedNavItem {fniFeedName    :: String
                ,fniTitle       :: String
                ,fniTime        :: String
                ,fniUrl         :: String
                ,fniHasRead     :: Bool
                }
    deriving Show

data NavItemOption = NTitle | NTime 
                   deriving (Eq, Show, Read)

class NavItemClass a where
  niGetColumnTitle        :: a -> String
  niGetColumnMaxWidth     :: a -> Maybe Int
  niGetCellText           :: a -> FeedNavItem -> String
  niGetCellXAlign         :: a -> Float
  niCompareRow            :: a -> FeedNavItem -> FeedNavItem -> IO Ordering

instance NavItemClass NavItemOption where
  niGetColumnTitle NTitle       = "Title" 
  niGetColumnTitle NTime        = "Time"

  niGetColumnMaxWidth NTitle = Just 500
  niGetColumnMaxWidth NTime  = Just 50

  niGetCellText NTitle  item    = decodeString $ fniTitle item
  niGetCellText NTime   item    = fniTime item

  niGetCellXAlign NTitle        = 0.0
  niGetCellXAlign NTime         = 0.0
  
  niCompareRow NTitle   item1 item2 = return $ comparing fniTitle item1 item2
  niCompareRow NTime    item1 item2 = return $ comparing fniTime item1 item2 

-- | Init state.
readerInitState :: ReaderState      
readerInitState = 
  ReaderState Nothing Nothing (0, 0) (0, 0)

-- | Create reader buffer.
readerBufferNew :: String -> [String] -> Client -> PageId -> CustomizeWrap -> IO ReaderBuffer
readerBufferNew path _ client pageId c = do
  let customize = castCustomize c
  feedInfos <- readTVarIO (readerCustomizeFeedInfos customize)

  buffer <- ReaderBuffer <$> pure path
                         <*> pure client
                         <*> pure pageId
                         <*> pure readerMode
                         <*> pure feedInfos
                         <*> newTVarIO (map (\ (name, _) -> FeedTreeItem name 0 0) $ M.toList feedInfos)
                         <*> pure (pairPred [TName])
                         <*> newTVarIO M.empty
                         <*> pure (pairPred [NTitle, NTime])
                         <*> (newTChanIO :: IO (TChan ReaderTChanSignal))
                         <*> pure customize
                         <*> newTVarIO readerInitState

  -- Fetch feed.
  readerBufferFetchFeed buffer

  return buffer

-- | Fetch feed.
readerBufferFetchFeed :: ReaderBuffer -> IO ()
readerBufferFetchFeed buffer@(ReaderBuffer {readerBufferFeedInfos = infos}) = 
  forM_ (M.toList infos) $ \ (feedName, _) -> 
    readerBufferUpdateFeed buffer feedName

-- | Update feed.
readerBufferUpdateFeed :: ReaderBuffer -> FeedName -> IO ()
readerBufferUpdateFeed buffer@(ReaderBuffer {readerBufferFeedInfos = infos})
                       feedName = do
  forkIO $ 
    findMinMatch infos (\ name _ -> name == feedName) 
      ?>= \ (_, FeedInfo {feedUrl          = url
                         ,feedInterval     = interval}) -> do
        -- Download feed.
        feed <- openAsFeed url

        case feed of
          -- Pick feed information after download feed.
          Right f -> do
              putStrLn $ "Feed " ++ feedName ++ " is updated."
              pickFeedInfo buffer feedName f
              writeTChanIO (readerBufferBroadcastChannel buffer) (FeedUpdated feedName)
          -- Or print error information.
          Left err -> putStrLn err

        -- Start next update cycle.
        timeoutAddFull (readerBufferUpdateFeed buffer feedName >> return False) 
                       priorityDefault 
                       (calculateIntervalTime interval)
        return ()

  return ()

-- | Pick information from feed.
pickFeedInfo :: ReaderBuffer -> FeedName -> Feed -> IO () 
pickFeedInfo (ReaderBuffer {readerBufferFeedNavItems = navItems}) 
          name feed = 
  modifyTVarIO navItems $ \items -> 
      let newNavItems =
              map (\item -> 
                       FeedNavItem name
                                   (fromMaybe "" $ getItemTitle item)
                                   (fromMaybe "" $ getItemDate item)
                                   (fromMaybe "" $ getItemLink item)
                                   False
                  ) $ getFeedItems feed
      in M.insert name newNavItems items

-- | Calculate interval time.
calculateIntervalTime :: [Interval] -> Int
calculateIntervalTime [] = 0
calculateIntervalTime (x:xs) = time + calculateIntervalTime xs
    where 
      time   = second * 1000    -- convert to millisecond
      second = case x of        -- convert to second
                 Second t -> t
                 Minute t -> t * 60
                 Hour   t -> t * 60 * 60
                 Day    t -> t * 60 * 60 * 24

-- | Reader customize new.
readerCustomizeNew :: IO CustomizeWrap
readerCustomizeNew =
  fmap CustomizeWrap
       ReaderCustomize <$> newTVarIO feedInfos

-- | Write state.
readerBufferWriteState :: ReaderBuffer -> FilePath -> IO ()
readerBufferWriteState buffer path = do
  state <- readTVarIO $ readerBufferState buffer
  writeConfigPath path state

-- | Read state.
readerBufferReadState :: ReaderBuffer -> FilePath -> IO ()  
readerBufferReadState buffer path = do
  state <- readConfigPath path readerInitState
  writeTVarIO (readerBufferState buffer) state
  
$(derive makeBinary ''ReaderState)

