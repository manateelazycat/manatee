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

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Extension.Curl.Types where

import Control.Concurrent
import Control.Concurrent.STM 
import Data.Binary
import Data.DeriveTH
import Data.Function
import Data.Map (Map)
import Data.Ord
import Data.Typeable
import Foreign
import Graphics.UI.Gtk.Gdk.Pixbuf
import Manatee.Core.Types
import Manatee.Toolkit.General.STM
import Unsafe.Coerce

data CurlCustomize =
    CurlCustomize {curlCustomizeDefaultThreadNumber     :: TVar Int
                  ,curlCustomizeDefaultCacheSize        :: TVar Int
                  ,curlCustomizeAutoStart        :: TVar Bool
                  }
    deriving Typeable

instance Customize CurlCustomize where
  customizeConfigFile _ = "Curl.hs"
  customizeLoad a       =
    [("defaultThreadNumber"
     ,\v -> writeTVarIO (curlCustomizeDefaultThreadNumber a) (unsafeCoerce v :: Int))
    ,("defaultCacheSize"
     ,\v -> writeTVarIO (curlCustomizeDefaultCacheSize a) (unsafeCoerce v :: Int))
    ,("autoStart"
     ,\v -> writeTVarIO (curlCustomizeAutoStart a) (unsafeCoerce v :: Bool))]

data DownloadFile =
    DownloadFile {dfURL                 :: String
                 ,dfName                :: TVar String
                 ,dfSize                :: TVar Int -- bytes
                 ,dfProgress            :: TVar Double
                 ,dfSpeed               :: TVar Int
                 ,dfRestTime            :: TVar Int -- second
                 ,dfBuffers             :: TVar [DownloadBuffer]
                 ,dfFinishLock          :: MVar DownloadResult
                 ,dfDownloadStatus      :: TVar DownloadStatus
                 ,dfFlushCounter        :: TVar Int -- for debug
                 }

data DownloadBuffer =
    DownloadBuffer {dbIndex             :: Int
                   ,dbRange             :: (Int, Int)
                   ,dbThreadId          :: TVar (Maybe ThreadId)
                   ,dbDownloadOffset    :: Int -- keep pause position
                   ,dbDownloadSize      :: TVar Int -- bytes
                   ,dbCacheSize         :: TVar Int -- bytes
                   ,dbRecordSize        :: TVar Int -- for calculate download speed
                   ,dbIsFinish          :: TVar Bool
                   }

data DownloadStatus = Running
                    | Pause
                    | Finish
                    | Failed
                      deriving (Eq, Show, Read)

data DownloadResult = DownloadPause
                    | DownloadError String
                    | DownloadFinish
                      deriving (Eq, Show, Read)

data DownloadLog = 
    DownloadLog {dlURL          :: String
                ,dlName         :: String
                ,dlSize         :: Int
                ,dlBuffers      :: Map Int DownloadLogBuffer
                } deriving (Show, Read, Eq, Ord, Typeable)

data DownloadLogBuffer =
    DownloadLogBuffer {dlbRange         :: (Int, Int)
                      ,dlbCacheSize     :: Int -- bytes
                      ,dlbIsFinish      :: Bool
                      } deriving (Show, Read, Eq, Ord, Typeable)

data DownloadStatusPixbuf =
    DownloadStatusPixbuf {dspPause      :: Pixbuf
                         ,dspRunning    :: Pixbuf
                         ,dspFinish     :: Pixbuf
                         ,dspFailed     :: Pixbuf}

instance Show DownloadFile where
  show = dfURL
  
instance Eq DownloadFile where
    (==) = (==) `on` dfURL

instance Ord DownloadFile where  
    compare = comparing dfURL

data BufferCache = BufferCache !(Ptr Word8) !Int

data DownloadFileOption = DFName | DFSize | DFDownloadSize | DFRestSize 
                        | DFSpeed | DFRestTime | DFThread | DFURL
                          deriving (Eq, Show, Read)

$(derive makeBinary ''DownloadLogBuffer)
$(derive makeBinary ''DownloadLog)
