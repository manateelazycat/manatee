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
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Extension.Editor.SourceBuffer where

import Control.Applicative
import Control.Concurrent.STM 
import DBus.Client hiding (Signal)
import Data.Binary
import Data.DeriveTH
import Data.List 
import Data.Maybe
import Data.Typeable
import Graphics.UI.Gtk.Multiline.TextBuffer
import Manatee.Core.Config
import Manatee.Core.PageMode
import Manatee.Core.Types
import Manatee.Extension.Editor.PageMode
import Manatee.Toolkit.General.STM
import Text.Regex.TDFA

import qualified Data.ByteString as BS
import qualified Graphics.UI.Gtk.SourceView.SourceBuffer as SB

data SourceBuffer =
    SourceBuffer {sourceBufferFilePath          :: TVar String
                 ,sourceBufferClient            :: Client
                 ,sourceBufferPageId            :: PageId
                 ,sourceBufferMode              :: PageMode
                 ,sourceBufferBuffer            :: SB.SourceBuffer
                 ,sourceBufferState             :: TVar SourceState
                 } 
    deriving Typeable

data SourceState =
    SourceState {sourceStateCursor              :: Maybe (Int, Int)
                ,sourceStateScrolledPosition    :: (Double, Double)
                } deriving Show

-- | Init state.
sourceInitState :: SourceState
sourceInitState =
    SourceState (Just (1, 0)) (0, 0)

-- | New source buffer.
sourceBufferNew :: String -> [String] -> Client -> PageId -> CustomizeWrap -> IO SourceBuffer
sourceBufferNew path _ client pageId _ = do
  buffer <- SB.sourceBufferNew Nothing
  SB.sourceBufferSetHighlightMatchingBrackets buffer True -- highlight match brackets
  SB.sourceBufferSetMaxUndoLevels buffer (-1)             -- no limit for undo level
  textBufferSetByteString buffer =<< BS.readFile path
  SourceBuffer <$> newTVarIO path
               <*> pure client
               <*> pure pageId
               <*> pure (fromMaybe defaultMode $ find (\x -> path =~ pageModeRegexp x) sourceModeList)
               <*> pure buffer
               <*> newTVarIO sourceInitState

-- | Write state.
sourceBufferWriteState :: SourceBuffer -> FilePath -> IO ()
sourceBufferWriteState buffer path = do
  state <- readTVarIO $ sourceBufferState buffer
  writeConfigPath path state

-- | Read state.
sourceBufferReadState :: SourceBuffer -> FilePath -> IO ()  
sourceBufferReadState buffer path = do
  state <- readConfigPath path sourceInitState
  writeTVarIO (sourceBufferState buffer) state
  
$(derive makeBinary ''SourceState)
