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
module Manatee.Extension.ImageViewer.ImageView where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import Data.ByteString.UTF8 hiding (length)
import Data.List
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Manatee.Core.Config
import Manatee.Core.PageFrame
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Extension.ImageViewer.ImageBuffer
import Manatee.Toolkit.GConf.GConf
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Misc
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gio.Gio
import Manatee.Toolkit.Gtk.Concurrent
import Manatee.Toolkit.Gtk.ScrolledWindow
import Paths_manatee_imageviewer
import System.FilePath

import qualified Graphics.UI.Gtk.ImageView.ImageView as I
import qualified Data.Map as M

data ImageView =
    ImageView {imageViewPlugId          :: TVar PagePlugId
              ,imageViewFrame           :: PageFrame
              ,imageViewView            :: I.ImageView
              ,imageViewBuffer          :: ImageBuffer
              ,imageViewDirection       :: TVar ImageDirection
              ,imageViewBroadcastChannel:: ViewChannel String}
    deriving Typeable

instance PageBuffer ImageBuffer where
  pageBufferGetName             = readTVarIO . imageBufferPath
  pageBufferSetName a           = writeTVarIO (imageBufferPath a)
  pageBufferClient              = imageBufferClient
  pageBufferCreateView a pId    = PageViewWrap <$> imageViewNew a pId
  pageBufferMode                = imageBufferMode
  pageBufferPackageName _       = fmap takeFileName getDataDir

instance PageView ImageView where
    pageViewBuffer               = PageBufferWrap . imageViewBuffer
    pageViewPlugId               = imageViewPlugId
    pageViewFrame                = imageViewFrame
    pageViewLocalKeymap _        = imageViewLocalKeymap
    pageViewLocalCommandMap _    = imageViewLocalCommandMap
    pageViewFocus                = widgetGrabFocus . imageViewView
    pageViewPropagateWidget      = castToWidget . imageViewView
    pageViewSaveState view       = imageViewSaveState view Nothing
    pageViewRestoreState view    = imageViewRestoreState view Nothing
    pageViewWriteState view path = imageViewSaveState view (Just path)
    pageViewReadState view path  = imageViewRestoreState view (Just path)

-- | The intervals of slide show (ms).
imageViewSlideShowInterval :: Int
imageViewSlideShowInterval = 4000

-- | New image view.
imageViewNew :: ImageBuffer -> PagePlugId -> IO ImageView
imageViewNew buffer plugId = do
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create scrolled window.
  pFrame <- pageFrameNewWithModeName (pageModeName $ imageBufferMode buffer)
  
  -- Load image.
  view <- I.imageViewNew
  pageFrameAddChild pFrame view

  -- Init direction.
  direction <- newTVarIO DirectionUp

  -- Duplicate broadcast channel.
  channel <- createViewChannel (imageBufferBroadcastChannel buffer) view

  -- Build image view.
  let imageView = ImageView pId pFrame view buffer direction channel

  -- Listen broadcast channel.
  imageViewListenChannel imageView

  -- Update zoom status.
  view `on` I.zoomChanged $ imageViewUpdateZoomStatus imageView

  return imageView

-- | Draw image view.
imageViewDraw :: ImageView -> ImageDirection -> IO ()
imageViewDraw view direction = do
  path <- readTVarIO $ imageBufferPath $ imageViewBuffer view
  oldPixbuf <- pixbufNewFromFile (filepathGetDisplayName (fromString path))
  newPixbuf <- 
      case direction of
        DirectionUp     -> return oldPixbuf
        DirectionRight  -> pixbufRotateSimple oldPixbuf PixbufRotateClockwise
        DirectionLeft   -> pixbufRotateSimple oldPixbuf PixbufRotateCounterclockwise
        DirectionDown   -> pixbufRotateSimple oldPixbuf PixbufRotateUpsidedown
  writeTVarIO (imageViewDirection view) direction
  I.imageViewSetPixbuf (imageViewView view) (Just newPixbuf) True

-- | Browse.
imageViewBrowse :: ImageView -> FilePath -> IO ()
imageViewBrowse view path = do
  -- Update current path in ImageBuffer.
  writeTVarIO (imageBufferPath $ imageViewBuffer view) path

  -- Broadcast current path for synchronous in multiple ImageView.
  writeTChanIO (viewChannel $ imageViewBroadcastChannel view) path

-- | Listen broadcast channel for draw view synchronous.
imageViewListenChannel :: ImageView -> IO ()
imageViewListenChannel view = 
  listenViewChannel (imageViewBroadcastChannel view) $ \_ ->
      imageViewDraw view DirectionUp

-- | Zoom out image.
imageViewZoomOut :: ImageView -> IO ()
imageViewZoomOut = I.imageViewZoomOut . imageViewView

-- | Zoom int image.
imageViewZoomIn :: ImageView -> IO ()
imageViewZoomIn = I.imageViewZoomIn . imageViewView

-- | Fit the window.
imageViewFit :: ImageView -> IO ()
imageViewFit view = 
  I.imageViewSetFitting (imageViewView view) True

-- | View next file.
imageViewBrowseNext :: ImageView -> IO ()
imageViewBrowseNext view = do
  currentFile <- readTVarIO $ imageBufferPath $ imageViewBuffer view
  files <- readTVarIO $ imageBufferFiles $ imageViewBuffer view
  imageViewGetNextFile currentFile files ?>= \file -> 
      imageViewBrowse view file
  
-- | View Prev file.
imageViewBrowsePrev :: ImageView -> IO ()
imageViewBrowsePrev view = do
  currentFile <- readTVarIO $ imageBufferPath $ imageViewBuffer view
  files <- readTVarIO $ imageBufferFiles $ imageViewBuffer view
  imageViewGetPrevFile currentFile files ?>= \file -> 
      imageViewBrowse view file

-- | View first file.
imageViewBrowseFirst :: ImageView -> IO ()
imageViewBrowseFirst view = do
  files <- readTVarIO $ imageBufferFiles $ imageViewBuffer view
  getFirst files ?>= \ file -> imageViewBrowse view file

-- | View last file.
imageViewBrowseLast :: ImageView -> IO ()
imageViewBrowseLast view = do
  files <- readTVarIO $ imageBufferFiles $ imageViewBuffer view
  getLast files ?>= \ file -> imageViewBrowse view file

-- | Update direction.
imageViewUpdateDirection :: ImageView -> RotateAction -> IO ()  
imageViewUpdateDirection ImageView {imageViewDirection = directionTVar}
                         action = do
  -- Get old direction.
  direction <- readTVarIO directionTVar
  
  -- Get new direction.
  let newDirection = 
          case action of
            RotateCounterclockwise ->
                case direction of
                  DirectionUp    -> DirectionLeft
                  DirectionRight -> DirectionUp
                  DirectionDown  -> DirectionRight
                  DirectionLeft  -> DirectionDown
            RotateClockwise ->
                case direction of
                  DirectionUp    -> DirectionRight
                  DirectionRight -> DirectionDown
                  DirectionDown  -> DirectionLeft
                  DirectionLeft  -> DirectionUp
            RotateMirror ->
                case direction of
                  DirectionUp    -> DirectionDown
                  DirectionRight -> DirectionLeft
                  DirectionDown  -> DirectionUp
                  DirectionLeft  -> DirectionRight

  -- Update direction.
  writeTVarIO directionTVar newDirection

-- | Rotate clockwise
imageViewRotateClockwise :: ImageView -> IO ()
imageViewRotateClockwise view = do
  oldPixbuf <- I.imageViewGetPixbuf $ imageViewView view
  newPixbuf <- pixbufRotateSimple oldPixbuf PixbufRotateClockwise
  I.imageViewSetPixbuf (imageViewView view) (Just newPixbuf) True
  imageViewUpdateDirection view RotateClockwise
  
-- | Rotate counterclockwise.
imageViewRotateCounterclockwise :: ImageView -> IO ()
imageViewRotateCounterclockwise view = do
  oldPixbuf <- I.imageViewGetPixbuf $ imageViewView view
  newPixbuf <- pixbufRotateSimple oldPixbuf PixbufRotateCounterclockwise
  I.imageViewSetPixbuf (imageViewView view) (Just newPixbuf) True
  imageViewUpdateDirection view RotateCounterclockwise
  
-- | Rotate mirror.
imageViewRotateMirror :: ImageView -> IO ()
imageViewRotateMirror view = do
  oldPixbuf <- I.imageViewGetPixbuf $ imageViewView view
  newPixbuf <- pixbufRotateSimple oldPixbuf PixbufRotateUpsidedown
  I.imageViewSetPixbuf (imageViewView view) (Just newPixbuf) True
  imageViewUpdateDirection view RotateMirror

-- | Update zoom status.
imageViewUpdateZoomStatus :: ImageView -> IO ()
imageViewUpdateZoomStatus view = do
  zoom <- liftM (\x -> floor $ formatFloatN x 2 * 100) $ I.imageViewGetZoom $ imageViewView view
  pageViewUpdateStatusbar view "Zoom" (" Zoom (" ++ show zoom ++ "%)")

-- | Find next image file under current directory.
imageViewGetNextFile :: FilePath -> [FilePath] -> Maybe FilePath
imageViewGetNextFile _ []  = Nothing
imageViewGetNextFile _ [_] = Nothing
imageViewGetNextFile currentFile files = 
  case findIndex (== currentFile) files of
    Just i -> 
        Just $ if i >= length files - 1
                 -- Use first image when reach last image.
                 then head files
                 -- Otherwise use next image.
                 else (!!) files (i + 1)
    Nothing -> Nothing

-- | Find previous image file under current directory.
imageViewGetPrevFile :: FilePath -> [FilePath] -> Maybe FilePath
imageViewGetPrevFile _ []  = Nothing
imageViewGetPrevFile _ [_] = Nothing
imageViewGetPrevFile currentFile files = 
  case findIndex (== currentFile) files of
    Just i -> 
        Just $ if i <= 0
                 -- Use last image when reach first image.
                 then last files
                 -- Otherwise use previous image.
                 else (!!) files (i - 1)
    Nothing -> Nothing

-- | Slide show.
imageViewSlideShow :: ImageView -> IO ()
imageViewSlideShow view@(ImageView {imageViewBuffer = 
                                        ImageBuffer {imageBufferSlideShowHanlderId = slideShowId}}) = do
  sId <- readTVarIO slideShowId
  case sId of
    -- Stop slide show.
    Just id -> do
      pageViewUpdateStatusbar view "SlideShow" " SlideShow (off)"
      timeoutRemove id
      writeTVarIO slideShowId Nothing
    -- Start slide show.
    Nothing -> do
      pageViewUpdateStatusbar view "SlideShow" " SlideShow (on)"
      handlerId <- timeoutAdd (imageViewBrowseNext view >> return True) imageViewSlideShowInterval
      writeTVarIO slideShowId (Just handlerId)

-- | Set as background, (just work for gnome).
imageViewSetAsBackground :: ImageView -> IO ()
imageViewSetAsBackground (ImageView {imageViewBuffer = 
                                         ImageBuffer {imageBufferPath = bufferPath}}) = do
  -- Get image path.
  path <- readTVarIO bufferPath
  setDesktopBackground path 100 Zoom

-- | Keymap.
imageViewLocalKeymap :: Map Text Text
imageViewLocalKeymap = 
    M.fromList [("j",           "Scroll step up")
               ,("k",           "Scroll step down")
               ,("Down",        "Scroll step up")
               ,("Up",          "Scroll step down")
               ,("h",           "Scroll step right")
               ,("l",           "Scroll step left")
               ,("Right",       "Scroll step right")
               ,("Left",        "Scroll step left")
               ,(" ",           "Scroll page up")
               ,("b",           "Scroll page down")
               ,("PageDown",    "Scroll page up")
               ,("PageUp",      "Scroll page down")
               ,("J",           "Scroll to bottom")
               ,("K",           "Scroll to top")
               ,("End",         "Scroll to bottom")
               ,("Home",        "Scroll to top")
               ,(",",           "Zoom out")
               ,(".",           "Zoom in")
               ,("-",           "Zoom out")
               ,("=",           "Zoom in")
               ,("m",           "Fit size")
               ,("n",           "Browse next")
               ,("Return",      "Browse previous")
               ,("p",           "Browse previous")
               ,("N",           "Browse last")
               ,("P",           "Browse first")
               ,("<",           "Rotate counterclockwise")
               ,(">",           "Rotate clockwise")
               ,("/",           "Rotate mirror")
               ,("s",           "Slide show")
               ,("B",           "Set as background")
               ]

-- | Keymap.
imageViewLocalCommandMap :: Map Text (ImageView -> IO ())
imageViewLocalCommandMap = 
    M.fromList [("Scroll step up",              pageViewScrollStepUp)
               ,("Scroll step down",            pageViewScrollStepDown)
               ,("Scroll step right",           pageViewScrollStepRight)
               ,("Scroll step left",            pageViewScrollStepLeft)
               ,("Scroll page up",              pageViewScrollPageUp)
               ,("Scroll page down",            pageViewScrollPageDown)
               ,("Scroll to bottom",            pageViewScrollToBottom)
               ,("Scroll to top",               pageViewScrollToTop)
               ,("Zoom out",                    imageViewZoomOut)
               ,("Zoom in",                     imageViewZoomIn)
               ,("Fit size",                    imageViewFit)
               ,("Browse next",                 imageViewBrowseNext)
               ,("Browse previous",             imageViewBrowsePrev)
               ,("Browse last",                 imageViewBrowseLast)
               ,("Browse first",                imageViewBrowseFirst)
               ,("Rotate counterclockwise",     imageViewRotateCounterclockwise)
               ,("Rotate clockwise",            imageViewRotateClockwise)
               ,("Rotate mirror",               imageViewRotateMirror)
               ,("Slide show",                  imageViewSlideShow)
               ,("Set as background",           imageViewSetAsBackground)
               ]

-- | Scrolled window.
imageViewScrolledWindow :: ImageView -> ScrolledWindow
imageViewScrolledWindow =
  pageFrameScrolledWindow . imageViewFrame

-- | Save state.
imageViewSaveState :: ImageView -> Maybe FilePath -> IO ()
imageViewSaveState view@(ImageView {imageViewBuffer     = buffer
                                   ,imageViewView       = imageView
                                   ,imageViewDirection  = directionTVar}) 
                   statePath = do
  -- Get direction.
  direction <- readTVarIO directionTVar

  -- Get zoom level.
  zoom <- I.imageViewGetZoom imageView

  -- Get scroll position.
  scrolledWindowPosition <- scrolledWindowGetValue (imageViewScrolledWindow view)

  -- Save state.
  let state = ImageState direction (Just zoom) scrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (imageBufferState buffer) state
    Just path -> writeConfigPath path state

-- | Restore state.
imageViewRestoreState :: ImageView -> Maybe FilePath -> IO ()
imageViewRestoreState view@(ImageView {imageViewBuffer  = buffer
                                      ,imageViewView    = imageView})
                      statePath = do
  bufferState <- readTVarIO (imageBufferState buffer)
  (ImageState direction zoom scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore direction.
  imageViewDraw view direction

  -- Restore zoom level.
  zoom ?>= (I.imageViewSetZoom imageView)

  -- Restore scroll position.
  scrolledWindowSetValue (imageViewScrolledWindow view) scrolledWindowPosition
