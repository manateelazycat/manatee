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
module Manatee.Extension.PdfViewer.PdfView where

import Control.Applicative
import Control.Arrow
import Control.Concurrent.STM 
import Control.Monad.State
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Graphics.UI.Gtk.Poppler.Document
import Graphics.UI.Gtk.Poppler.Page
import Manatee.Core.Config
import Manatee.Core.PageFrame
import Manatee.Core.PageView 
import Manatee.Core.Types
import Manatee.Extension.PdfViewer.PdfBuffer
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Event
import Manatee.Toolkit.Gtk.ScrolledWindow
import Paths_manatee_pdfviewer
import System.FilePath

import qualified Data.Map as M

data PdfView =
    PdfView {pdfViewPlugId          :: TVar PagePlugId
            ,pdfViewFrame           :: PageFrame
            ,pdfViewView            :: DrawingArea
            ,pdfViewBuffer          :: PdfBuffer
            ,pdfViewPageIndex       :: TVar Int
            ,pdfViewScale           :: TVar Double 
            }
    deriving Typeable

instance PageBuffer PdfBuffer where
  pageBufferGetName             = readTVarIO . pdfBufferPath
  pageBufferSetName a           = writeTVarIO (pdfBufferPath a)
  pageBufferClient              = pdfBufferClient
  pageBufferCreateView a pId    = PageViewWrap <$> pdfViewNew a pId
  pageBufferMode                = pdfBufferMode
  pageBufferPackageName _       = fmap takeFileName getDataDir

instance PageView PdfView where
    pageViewBuffer               = PageBufferWrap . pdfViewBuffer
    pageViewPlugId               = pdfViewPlugId
    pageViewFrame                = pdfViewFrame
    pageViewLocalKeymap _        = pdfViewLocalKeymap
    pageViewLocalCommandMap _    = pdfViewLocalCommandMap
    pageViewFocus                = widgetGrabFocus . pdfViewView
    pageViewPropagateWidget      = castToWidget . pdfViewView
    pageViewSaveState view       = pdfViewSaveState view Nothing
    pageViewRestoreState view    = pdfViewRestoreState view Nothing
    pageViewWriteState view path = pdfViewSaveState view (Just path)
    pageViewReadState view path  = pdfViewRestoreState view (Just path)

-- | New pdf viewer view.
pdfViewNew :: PdfBuffer -> PagePlugId -> IO PdfView
pdfViewNew buffer plugId = do
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ pdfBufferMode buffer)
  
  -- Create view.
  view <- drawingAreaNew
  scrolledWindowAddWithViewport (pageFrameScrolledWindow pFrame) view
  pageIndex <- newTVarIO 0
  scale <- newTVarIO 0
  let viewer = PdfView pId pFrame view buffer pageIndex scale

  -- Redraw signal.
  view `after` exposeEvent $ tryEvent $ pdfViewHandleExpose viewer

  return viewer

-- | Handle expose signal.
pdfViewHandleExpose :: PdfView -> EventM EExpose ()
pdfViewHandleExpose view@(PdfView {pdfViewBuffer        = buffer
                                  ,pdfViewScale         = scaleTVar
                                  }) = do
  (winWidth, _) <- eventWindowSize                    
  liftIO $ do
    let (pageWidth, pageHeight) = pdfBufferPageSize buffer
        area = pdfViewView view
    -- Init scale and size request.
    scale <- readTVarIO scaleTVar
    when (scale == 0) $ do
             putStrLn "Hello"
             let scaleValue = (winWidth / pageWidth)
             writeTVarIO scaleTVar scaleValue
             widgetSetSizeRequest 
                        area 
                        (truncate winWidth) 
                        (truncate (scaleValue * pageHeight * i2d (pdfBufferNPages buffer)))

    -- Draw.
    pdfViewDraw view 

-- | Draw for viewer.
pdfViewDraw :: PdfView -> IO ()
pdfViewDraw view@(PdfView {pdfViewBuffer        = buffer
                          ,pdfViewScale         = viewScale
                          ,pdfViewPageIndex     = pageIndex
                          }) = do
  -- Init.
  let (_, pageHeight) = pdfBufferPageSize buffer
      doc  = pdfBufferDocument buffer
      area = pdfViewView view

  -- Get render index range and scale value.
  (indexRange, currentIndex) <- pdfViewGetIndexRange view
  scaleValue <- readTVarIO viewScale

  -- Update page information.
  writeTVarIO pageIndex currentIndex
  pageFrameUpdateStatusbar
    (pdfViewFrame view) 
    "Page" 
    ("Page (" ++ show (currentIndex + 1) ++ "/" ++ show (pdfBufferNPages buffer) ++ ")")

  -- Render page.
  forM_ indexRange $ \ index -> do
        page <- documentGetPage doc index
        drawWindow <- widgetGetDrawWindow area
        renderWithDrawable drawWindow $ do
          setSourceRGB 1.0 1.0 1.0
          translate 0 (scaleValue * pageHeight * i2d index)
          scale scaleValue scaleValue
          pageRender page
                                        
-- | Keymap.
pdfViewLocalKeymap :: Map Text Text
pdfViewLocalKeymap = 
  M.fromList [("j",        "Scroll step up")
             ,("k",        "Scroll step down")
             ,("Down",     "Scroll step up")
             ,("Up",       "Scroll step down")
             ,(" ",        "Scroll page up")
             ,("b",        "Scroll page down")
             ,("J",        "Scroll to bottom")
             ,("K",        "Scroll to top")
             ,("n",        "Next page")
             ,("p",        "Previous page")
             ,("N",        "Last page")
             ,("P",        "First page")
             ,("g",        "Goto page")
             ]

-- | Keymap.
pdfViewLocalCommandMap :: Map Text (PdfView -> IO ())
pdfViewLocalCommandMap = 
  M.fromList [("Scroll step up",        pageViewScrollVerticalStep True)
             ,("Scroll step down",      pageViewScrollVerticalStep False)
             ,("Scroll page up",        pageViewScrollVerticalPage True)
             ,("Scroll page down",      pageViewScrollVerticalPage False)
             ,("Scroll to bottom",      pageViewScrollToBottom)
             ,("Scroll to top",         pageViewScrollToTop)
             ,("Next page",             pdfViewNextPage)
             ,("Previous page",         pdfViewPrevPage)
             ,("Last page",             pdfViewLastPage)
             ,("First page",            pdfViewFirstPage)
             ,("Goto page",             pdfViewGotoPage)
             ]

-- | Get index range to render visible are.
pdfViewGetIndexRange :: PdfView -> IO ([Int], Int)
pdfViewGetIndexRange view@(PdfView {pdfViewBuffer             = buffer
                                   ,pdfViewScale              = scale}) = do
  vAdj       <- scrolledWindowGetVAdjustment $ pdfViewScrolledWindow view
  vAdjTop    <- adjustmentGetValue vAdj
  vAdjSize   <- adjustmentGetPageSize vAdj
  scaleValue <- readTVarIO scale
  let vAdjBottom  = vAdjTop + vAdjSize
      (_, height) = ((*) scaleValue *** (*) scaleValue) $ pdfBufferPageSize buffer
      nPages      = pdfBufferNPages buffer
      topIndex    = truncate $ vAdjTop / height
      bottonIndex = truncate $ vAdjBottom / height
      startIndex  = (\x -> if x < 0 then 0 else x) topIndex
      endIndex    = (\x -> if x >= nPages then nPages - 1 else x) 
                    $ if vAdjBottom - i2d bottonIndex * height == 0.0
                         then bottonIndex - 1
                         else bottonIndex
      -- Use point at half of visible area avoid bound value problem.
      currIndex   = truncate $ (vAdjTop + vAdjSize / 2) / height
  return ([startIndex..endIndex], currIndex)

-- | Set page.
pdfViewSetPage :: PdfView -> Int -> IO ()
pdfViewSetPage view@(PdfView {pdfViewBuffer          = buffer
                             ,pdfViewScale           = viewScale
                             }) page = do
  scaleValue   <- readTVarIO viewScale
  let (_, height) = pdfBufferPageSize buffer
      minIndex    = 0
      maxIndex    = pdfBufferNPages buffer - 1
  destIndex <-
    if page < minIndex
       then do
         pageViewShowOutputbar view "Out of bound, goto first page." Nothing 
         return minIndex
    else if page > maxIndex
       then do
         pageViewShowOutputbar view "Out of bound, goto last page." Nothing
         return maxIndex
       else return page
  vAdj <- scrolledWindowGetVAdjustment $ pdfViewScrolledWindow view
  adjustmentSetValue vAdj (i2d destIndex * height * scaleValue)

-- | View next page.
pdfViewNextPage :: PdfView -> IO ()
pdfViewNextPage view@(PdfView {pdfViewPageIndex = pageIndex}) = do
  currentIndex <- readTVarIO pageIndex 
  pdfViewSetPage view (currentIndex + 1)

-- | View prev page.
pdfViewPrevPage :: PdfView -> IO ()
pdfViewPrevPage view@(PdfView {pdfViewPageIndex = pageIndex}) = do
  currentIndex <- readTVarIO pageIndex 
  pdfViewSetPage view (currentIndex - 1)

-- | First page.
pdfViewFirstPage :: PdfView -> IO ()
pdfViewFirstPage view = 
  pdfViewSetPage view 0

-- | Last page.
pdfViewLastPage :: PdfView -> IO ()
pdfViewLastPage view@(PdfView {pdfViewBuffer = buffer}) = do
  let maxIndex = pdfBufferNPages buffer - 1
  pdfViewSetPage view maxIndex

-- | Goto page.
pdfViewGotoPage :: PdfView -> IO ()
pdfViewGotoPage view@(PdfView {pdfViewBuffer = buffer}) = do
  let maxPage = pdfBufferNPages buffer
  interactive view [(INum, "Goto page (1 - " ++ show maxPage ++ ") : ", "")] $ \ [page] -> do
    let pageNumber = read page :: Int
    pdfViewSetPage view (pageNumber - 1)

-- | Scrolled window.
pdfViewScrolledWindow :: PdfView -> ScrolledWindow    
pdfViewScrolledWindow =
    pageFrameScrolledWindow . pdfViewFrame

-- | Save state.
pdfViewSaveState :: PdfView -> Maybe FilePath -> IO ()
pdfViewSaveState view@(PdfView {pdfViewBuffer     = buffer
                               ,pdfViewView       = area}) 
                 statePath = do
  -- Get page index.
  pageIndex <- readTVarIO (pdfViewPageIndex view)

  -- Get scale.
  scale <- readTVarIO (pdfViewScale view)

  -- Get page size.
  (Rectangle _ _ w h) <- widgetGetAllocation area

  -- Get scroll position.
  scrolledWindowPosition <- scrolledWindowGetValue (pdfViewScrolledWindow view)

  -- Save state.
  let state = PdfState pageIndex (Just scale) (Just (w, h)) scrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (pdfBufferState buffer) state
    Just path -> writeConfigPath path state

-- | Restore state.
pdfViewRestoreState :: PdfView -> Maybe FilePath -> IO ()
pdfViewRestoreState view@(PdfView {pdfViewBuffer                = buffer
                                  ,pdfViewView                  = area})
                    statePath = do
  let scrolledWindow = pdfViewScrolledWindow view

  bufferState <- readTVarIO (pdfBufferState buffer)
  (PdfState pageIndex scale size scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore page index.
  writeTVarIO (pdfViewPageIndex view) pageIndex

  -- Restore scale.
  scale ?>= (writeTVarIO (pdfViewScale view))

  -- Restore page size.
  size ?>= \ (w, h) -> do
    widgetSetSizeRequest area w h
    scrolledWindowSetUpper scrolledWindow (i2d w, i2d h)

  -- Restore scroll position.
  scrolledWindowSetValue scrolledWindow scrolledWindowPosition

  return ()
