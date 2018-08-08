-- Author:     Your Name <YourMail>
-- Maintainer: Your Name <YourMail>
-- 
-- Copyright (C) 2010 Your Name, all rights reserved.
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
module Manatee.Extension.Template.TemplateBuffer where

import Control.Applicative
import Control.Concurrent.STM 
import DBus.Client hiding (Signal)
import Data.Binary
import Data.DeriveTH
import Data.Typeable
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Extension.Template.PageMode
import Manatee.Toolkit.General.STM

import qualified Graphics.UI.Gtk.SourceView.SourceBuffer as SB

data TemplateBuffer =
    TemplateBuffer {templateBufferFilePath          :: TVar String
                   ,templateBufferClient            :: Client
                   ,templateBufferPageId            :: PageId
                   ,templateBufferMode              :: PageMode
                   ,templateBufferBuffer            :: SB.SourceBuffer
                   ,templateBufferState             :: TVar TemplateState
                   } 
    deriving Typeable

data TemplateState =
    TemplateState {templateStateCursor              :: Maybe (Int, Int)
                  ,templateStateScrolledPosition    :: (Double, Double)}

-- | Init state.
templateInitState :: TemplateState
templateInitState =
    TemplateState (Just (1, 0)) (0, 0)

-- | New template buffer.
templateBufferNew :: String        -- Buffer name, also is Tab name
                  -> [String]      -- option pass application to parse
                  -> Client        -- DBus client to handle DBus signal/method
                  -> PageId        -- Page Id to handle signal from daemon process
                  -> CustomizeWrap -- user's customize option
                  -> IO TemplateBuffer
templateBufferNew path _ client pageId _ = do
  buffer <- SB.sourceBufferNew Nothing
  TemplateBuffer <$> newTVarIO path
                 <*> pure client
                 <*> pure pageId
                 <*> pure templateMode
                 <*> pure buffer
                 <*> newTVarIO templateInitState

-- | Write state.
templateBufferWriteState :: TemplateBuffer -> FilePath -> IO ()
templateBufferWriteState buffer path = do
  state <- readTVarIO $ templateBufferState buffer
  writeConfigPath path state

-- | Read state.
templateBufferReadState :: TemplateBuffer -> FilePath -> IO ()  
templateBufferReadState buffer path = do
  state <- readConfigPath path templateInitState
  writeTVarIO (templateBufferState buffer) state
  
$(derive makeBinary ''TemplateState)
