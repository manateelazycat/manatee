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
module Manatee.Extension.Browser.BrowserView where

import Codec.Binary.UTF8.String
import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad.State
import Data.Map (Map)
import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Graphics.UI.Gtk.WebKit.Download
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebResource
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.PageFrame
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Extension.Browser.BrowserBuffer
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Concurrent
import Manatee.Toolkit.Gtk.ScrolledWindow
import Paths_manatee_browser
import System.FilePath

import qualified Data.Map as M

data BrowserView =
    BrowserView {browserViewPlugId               :: TVar PagePlugId
                ,browserViewFrame                :: PageFrame
                ,browserViewView                 :: WebView
                ,browserViewBuffer               :: BrowserBuffer
                ,browserViewBroadcastChannel     :: ViewChannel BrowserBufferSignal
                ,browserViewStatusLengthLimit    :: Int
                }
    deriving Typeable

instance PageBuffer BrowserBuffer where
  pageBufferGetName             = readTVarIO . browserBufferUri
  pageBufferSetName a           = writeTVarIO (browserBufferUri a)
  pageBufferClient              = browserBufferClient
  pageBufferCreateView a pId    = PageViewWrap <$> browserViewNew a pId
  pageBufferMode                = browserBufferMode
  pageBufferPackageName _       = fmap takeFileName getDataDir

instance PageView BrowserView where
    pageViewBuffer               = PageBufferWrap . browserViewBuffer
    pageViewPlugId               = browserViewPlugId
    pageViewFrame                = browserViewFrame
    pageViewCut                  = browserViewCut
    pageViewCopy                 = browserViewCopy
    pageViewPaste                = browserViewPaste
    pageViewLocalKeymap _        = browserViewLocalKeymap
    pageViewLocalCommandMap _    = browserViewLocalCommandMap
    pageViewFocus                = widgetGrabFocus . browserViewView    
    pageViewPropagateWidget      = castToWidget . browserViewView
    pageViewSaveState view       = browserViewSaveState view Nothing
    pageViewRestoreState view    = browserViewRestoreState view Nothing
    pageViewWriteState view path = browserViewSaveState view (Just path)
    pageViewReadState view path  = browserViewRestoreState view (Just path)

-- | New browser view.
browserViewNew :: BrowserBuffer -> PagePlugId -> IO BrowserView    
browserViewNew buffer plugId = do
  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ browserBufferMode buffer)
  
  -- Create view.
  webView <- webViewNew
  pageFrameAddChild pFrame webView
  uri <- pageBufferGetName buffer
  webViewLoadUri webView uri
  channel <- createViewChannel (browserBufferBroadcastChannel buffer) webView
  pId <- newTVarIO plugId
  let browserView = BrowserView pId pFrame webView buffer channel 100

  -- Set font.
  setting <- webViewGetWebSettings webView
  let defaultFont = "DejaVu Sans YuanTi Mono"
  set setting [webSettingsDefaultFontFamily   := defaultFont
              ,webSettingsSansFontFamily      := defaultFont
              ,webSettingsCursiveFontFamily   := defaultFont
              ,webSettingsFantasyFontFamily   := defaultFont
              ,webSettingsMonospaceFontFamily := defaultFont
              ,webSettingsSerifFontFamily     := defaultFont]

  -- Create new tab when request open new window.
  webView `on` createWebView $ browserViewNewTab browserView

  -- Synchronous other webkit view when loadCommitted signal.
  webView `on` loadCommitted $ browserViewChangeUri browserView 

  -- Record web history after load finished.
  webView `on` loadFinished $ browserRecordHistory

  -- Synchronous title when title changed.
  webView `on` titleChanged $ browserViewChangeTitle browserView

  -- Display link over mouse.
  webView `on` hoveringOverLink $ \ _ mouseUri -> 
    mouseUri ?>= \ uri -> 
        pageViewShowOutputbar browserView uri (Just (browserViewStatusLengthLimit browserView))

  -- Display resource request status. 
  webView `on` resourceRequestStarting $ \ _ resource _ _ -> do
    -- Get request uri.
    uri <- webResourceGetUri resource
    pageViewShowOutputbar browserView uri (Just (browserViewStatusLengthLimit browserView))

  webView `on` progressChanged $ \ progress ->
    pageViewUpdateProgress browserView (i2d progress)

  -- Download request signal.
  webView `on` downloadRequested $ \ download -> do
    downloadGetUri download
        >?>= \url -> do 
          -- Print downloading message.
          pageViewShowOutputbar browserView ("Downloading " ++ url) Nothing
          -- Send download request to download manager.
          ifM (isBusNameExist $ packGenericBusName "curl") 
              -- Send DBus signal if curl process exit.
              (mkGenericDaemonSignal (pageViewClient browserView) "curl" Generic (GenericArgs "Download" [url]))
              -- Otherwise start process and pass url.
              (mkDaemonSignal (pageViewClient browserView) NewTab (NewTabArgs "PageCurl" "Download" [url]))
    return False

  -- Listen broadcast channel for synchronous view.
  browserViewListenChannel browserView

  return browserView

-- -- | Handle key action.
-- | Local keymap.
browserViewLocalKeymap :: Map Text Text
browserViewLocalKeymap =
    M.fromList 
         [("M-r",           "Reload")
         ,("M-R",           "Reload pass cache")
         ,("XF86Back",      "Go back")
         ,("XF86Forward",   "Go forward")
         ,("M-,",           "Go back")
         ,("M-.",           "Go forward")
         ,("M--",           "Zoom out")
         ,("M-=",           "Zoom in")
         ]

-- | Keymap.
browserViewLocalCommandMap :: Map Text (BrowserView -> IO ())
browserViewLocalCommandMap = 
  M.fromList [("Reload",                browserViewReload)
             ,("Reload pass cache",     browserViewReloadBypassCache)
             ,("Go back",               browserViewGoBack)
             ,("Go forward",            browserViewGoForward)
             ,("Zoom out",              browserViewZoomOut)
             ,("Zoom in",               browserViewZoomIn)
             ]

-- | New tab.
browserViewNewTab :: BrowserView -> WebFrame -> IO WebView
browserViewNewTab view _ = do
  -- Because WebKit API will return new webView by signal `createWebView`,
  -- we use `webViewNew` build temporary webView to intercept new uri,
  -- then we send NewTab DBus signal and stop loading temporary webView.
  webView <- webViewNew
  webView `on` loadCommitted $ \ frame -> do
    -- Intercept new uri.
    webFrameGetUri frame >?>= \uri -> 
      mkDaemonSignal (pageViewClient view) NewTab (NewTabArgs "PageBrowser" uri [])
    -- Stop loading.
    webViewStopLoading webView

  -- Don't use WebFrame in signal `createWebView` to return webView,
  -- otherwise will change current tab.
  return webView

-- | Change uri.
browserViewChangeUri :: BrowserView -> WebFrame -> IO ()
browserViewChangeUri view frame = 
  webFrameGetUri frame >?>= \uri -> do
    putStrLn $ "Change url to : " ++ show uri
    writeTVarIO (browserBufferUri $ browserViewBuffer view) uri
    writeTChanIO (viewChannel $ browserViewBroadcastChannel view) (SyncUri uri)

-- | Record browse history.
browserRecordHistory :: WebFrame -> IO ()
browserRecordHistory frame = 
  webFrameGetUri frame >?>= \ uri -> do
    title <- liftM (fromMaybe uri) $ webFrameGetTitle frame
    (BrowseHistoryList browseHistory) <- readConfig browseHistoryPath (BrowseHistoryList M.empty)
    case findMinMatch browseHistory (\ bUri bTitle -> bUri == uri && bTitle == title) of
      Just _  -> return ()
      Nothing -> writeConfig browseHistoryPath (BrowseHistoryList (M.insert uri (decodeString title) browseHistory))
  
-- | Change title.
browserViewChangeTitle :: BrowserView -> WebFrame -> String -> IO ()
browserViewChangeTitle view _ title = 
  writeTChanIO (viewChannel $ browserViewBroadcastChannel view) (SyncTitle title)

-- | Listen broadcast channel to synchronous view.
browserViewListenChannel :: BrowserView -> IO ()
browserViewListenChannel view = 
  listenViewChannel (browserViewBroadcastChannel view) $ \ signal -> 
      case signal of
        SyncUri uri -> 
            webViewGetUri (browserViewView view) >?>= \currentUri -> 
                when (currentUri /= uri) $ webViewLoadUri (browserViewView view) uri
        _ -> return ()

-- | Reload page. 
browserViewReload :: BrowserView -> IO ()
browserViewReload = 
  webViewReload . browserViewView

-- | Reload page without using any cached data.
browserViewReloadBypassCache :: BrowserView -> IO ()
browserViewReloadBypassCache = 
  webViewReloadBypassCache . browserViewView

-- | Loads the previous history item.
browserViewGoBack :: BrowserView -> IO ()
browserViewGoBack = 
  webViewGoBack . browserViewView

-- | Loads the next history item. 
browserViewGoForward :: BrowserView -> IO ()
browserViewGoForward = 
  webViewGoForward . browserViewView

-- | Increases the zoom level of WebView. 
browserViewZoomIn :: BrowserView -> IO ()
browserViewZoomIn =
  webViewZoomIn . browserViewView

-- | Decreases the zoom level of WebView. 
browserViewZoomOut :: BrowserView -> IO ()
browserViewZoomOut =
  webViewZoomOut . browserViewView

-- | Browser cut.
browserViewCut :: BrowserView -> IO Bool
browserViewCut view@ (BrowserView {browserViewView = webView}) = do
  ifM (webViewCanCutClipboard webView)
      (webViewCutClipboard webView)
      (pageViewShowOutputbar view "Can't cut." Nothing)
  return True

-- | Browser copy.
browserViewCopy :: BrowserView -> IO Bool
browserViewCopy view@ (BrowserView {browserViewView = webView}) = do
  ifM (webViewCanCopyClipboard webView)
      (webViewCopyClipboard webView)
      (pageViewShowOutputbar view "Can't copy." Nothing)
  return True

-- | Browser paste.
browserViewPaste :: BrowserView -> IO Bool
browserViewPaste view@ (BrowserView {browserViewView = webView}) = do
  ifM (webViewCanPasteClipboard webView)
      (webViewPasteClipboard webView)
      (pageViewShowOutputbar view "Can't paste." Nothing)
  return True
  
-- | Scrolled window.
browserViewScrolledWindow :: BrowserView -> ScrolledWindow
browserViewScrolledWindow =
  pageFrameScrolledWindow . browserViewFrame

-- | Save state.
browserViewSaveState :: BrowserView -> Maybe FilePath -> IO ()
browserViewSaveState view@(BrowserView {browserViewBuffer = buffer}) 
                     statePath = do
  -- Get scroll position.
  scrolledWindowPosition <- scrolledWindowGetValue (browserViewScrolledWindow view)

  -- Save state.
  let state = BrowserState scrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (browserBufferState buffer) state
    Just path -> writeConfigPath path state              

-- | Restore state.
browserViewRestoreState :: BrowserView -> Maybe FilePath -> IO ()
browserViewRestoreState view@(BrowserView {browserViewBuffer  = buffer})
                        statePath = do
  -- TODO, I think it's need some web technology to restore web page's position.
  -- Just set scrolledWindow's position can't work.
  bufferState <- readTVarIO (browserBufferState buffer)
  (BrowserState scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore scroll position.
  scrolledWindowSetValue (browserViewScrolledWindow view) scrolledWindowPosition
