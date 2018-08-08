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
module Manatee.Extension.PdfViewer.PdfBuffer where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import DBus.Client hiding (Signal)
import Data.Binary
import Data.ByteString.UTF8
import Data.DeriveTH
import Data.Maybe
import Data.Typeable
import Graphics.UI.Gtk.Poppler.Document hiding (PageMode)
import Graphics.UI.Gtk.Poppler.Page
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Extension.PdfViewer.PageMode
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gio.Gio

data PdfBuffer =
    PdfBuffer {pdfBufferPath                :: TVar String
              ,pdfBufferClient              :: Client
              ,pdfBufferPageId              :: PageId
              ,pdfBufferMode                :: PageMode
              ,pdfBufferDocument            :: Document
              ,pdfBufferNPages              :: Int
              ,pdfBufferPageSize            :: (Double, Double)
              ,pdfBufferState               :: TVar PdfState
              }
    deriving Typeable

data PdfState =
    PdfState {pdfStatePageIndex         :: Int
             ,pdfStateScale             :: Maybe Double
             ,pdfStatePageSize          :: Maybe (Int, Int)
             ,pdfStateScrolledPosition  :: (Double, Double)}

-- | Init state.
pdfInitState :: PdfState
pdfInitState =
    PdfState 0 Nothing Nothing (0, 0)

-- | New pdf viewer buffer.
pdfBufferNew :: String -> [String] -> Client -> PageId -> CustomizeWrap -> IO PdfBuffer
pdfBufferNew path _ client pageId _ = do
  -- Get document.
  document <- liftM (fromMaybe (error $ "pdfBufferNew: error when open file " ++ filepath)) 
                    (documentNewFromFile ("file://" ++ filepath) Nothing)
  -- Get page number.
  nPages   <- documentGetNPages document

  -- Get page size.
  size <- pageGetSize =<< documentGetPage document 0
  
  -- Build buffer.
  PdfBuffer <$> newTVarIO path
            <*> pure client
            <*> pure pageId
            <*> pure pdfMode
            <*> pure document
            <*> pure nPages
            <*> pure size
            <*> newTVarIO pdfInitState

      where filepath = filepathGetDisplayName (fromString path)

-- | Write state.
pdfBufferWriteState :: PdfBuffer -> FilePath -> IO ()
pdfBufferWriteState buffer path = do
  state <- readTVarIO $ pdfBufferState buffer
  writeConfigPath path state

-- | Read state.
pdfBufferReadState :: PdfBuffer -> FilePath -> IO ()  
pdfBufferReadState buffer path = do
  state <- readConfigPath path pdfInitState
  writeTVarIO (pdfBufferState buffer) state
  
$(derive makeBinary ''PdfState)

