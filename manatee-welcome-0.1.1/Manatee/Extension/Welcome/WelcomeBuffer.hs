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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Extension.Welcome.WelcomeBuffer where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Exception
import Control.Monad
import Data.Typeable
import DBus.Client hiding (Signal)
import Graphics.UI.Gtk hiding (get)
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Extension.Welcome.PageMode
import Manatee.Toolkit.Gtk.Gtk
import Data.Binary
import Data.DeriveTH
import Manatee.Toolkit.General.STM

import qualified Data.Map as M
import qualified Control.Exception as Exc

data WelcomeBuffer =
    WelcomeBuffer {welcomeBufferFilePath        :: TVar String
                  ,welcomeBufferClient          :: Client
                  ,welcomeBufferPageId          :: PageId
                  ,welcomeBufferMode            :: PageMode
                  ,welcomeBufferStore           :: WelcomeBufferStore
                  ,welcomeBufferState           :: TVar WelcomeState
                  } 
    deriving Typeable

type WelcomeBufferStore = ListStore ((String, Pixbuf), (PageType, String, [String]))

-- | Application name column id.
applicationNameColumnId = makeColumnIdString 0

-- | Application icon column id.
applicationIconColumnId = makeColumnIdPixbuf 1

-- | Application space between items.
applicationSpace :: Int
applicationSpace = 20

-- | Application column number.
applicationColumns :: Int
applicationColumns = 3

data WelcomeState =
    WelcomeState {welcomeStateScrolledPosition  :: (Double, Double)}

-- | Init state.
welcomeInitState :: WelcomeState
welcomeInitState =
    WelcomeState (0, 0)

-- | New welcome buffer.
welcomeBufferNew :: String        -- Buffer name, also is Tab name
                  -> [String]      -- option pass application to parse
                  -> Client        -- DBus client to handle DBus signal/method
                  -> PageId        -- Page Id to handle signal from daemon process
                  -> CustomizeWrap -- user's customize option
                  -> IO WelcomeBuffer
welcomeBufferNew path _ client pageId _ = do
  -- Create store.
  store <- listStoreNew []
  customStoreSetColumn store applicationNameColumnId (fst . fst)
  customStoreSetColumn store applicationIconColumnId (snd . fst)

  -- Add application snapshots.
  addApplicationSnapshots store

  -- Create welcome buffer.
  WelcomeBuffer <$> newTVarIO path
                <*> pure client
                <*> pure pageId
                <*> pure welcomeMode
                <*> pure store
                <*> newTVarIO welcomeInitState

-- | Get application snapshots.
addApplicationSnapshots :: WelcomeBufferStore -> IO () 
addApplicationSnapshots store = 
  Exc.catch (do
              -- Clean store first.
              listStoreClear store

              -- Add application infos.
              (width, _) <- getDefaultScreenSize
              let appWidth = floor (fromIntegral 
                                    (width - applicationSpace * fromIntegral (applicationColumns - 1) * 2) 
                                    / fromIntegral applicationColumns)
                  appHeight = floor (fromIntegral appWidth * (9 / 16))
              (WelcomeApplication applications) <- readConfig welcomeApplicationPath (WelcomeApplication M.empty)
              forM_ (M.toList applications) $ \ ((appName, imgPath), (pType, pPath, pArgs)) -> do
                        pixbuf <- pixbufNewFromFile imgPath 
                        scalePixbuf <- pixbufScaleSimple pixbuf appWidth appHeight InterpBilinear
                        listStoreAppend store ((appName, scalePixbuf), (pType, pPath, pArgs)))
             (\ (_ :: SomeException) -> do
                -- Clear list store if got error when load sub-module's snapshot.
                putStrLn "Got error when load sub-module's snapshot."
                listStoreClear store)

-- | Write state.
welcomeBufferWriteState :: WelcomeBuffer -> FilePath -> IO ()
welcomeBufferWriteState buffer path = do
  state <- readTVarIO $ welcomeBufferState buffer
  writeConfigPath path state

-- | Read state.
welcomeBufferReadState :: WelcomeBuffer -> FilePath -> IO ()  
welcomeBufferReadState buffer path = do
  state <- readConfigPath path welcomeInitState
  writeTVarIO (welcomeBufferState buffer) state
  
$(derive makeBinary ''WelcomeState)
