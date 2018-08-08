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
module Manatee.Extension.Reader.ReaderView where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.WebView
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.PageFrame
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Extension.Reader.ReaderBuffer
import Manatee.Extension.Reader.Types
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Concurrent
import Manatee.Toolkit.Gtk.ModelView
import Manatee.Toolkit.Gtk.ScrolledWindow
import Paths_manatee_reader
import System.FilePath

import qualified Data.Map as M

data ReaderView =
    ReaderView {readerViewPlugId                :: TVar PagePlugId
               ,readerViewFrame                 :: PageFrame
               ,readerViewTreeView              :: TreeView
               ,readerViewTreeFrame             :: Frame
               ,readerViewTreeScrolledWindow    :: ScrolledWindow
               ,readerViewTreeListStore         :: ListStore FeedTreeItem
               ,readerViewTreeSortModel         :: TypedTreeModelSort FeedTreeItem
               ,readerViewNavView               :: TreeView
               ,readerViewNavFrame              :: Frame
               ,readerViewNavScrolledWindow     :: ScrolledWindow
               ,readerViewNavListStore          :: ListStore FeedNavItem
               ,readerViewNavSortModel          :: TypedTreeModelSort FeedNavItem
               ,readerViewWebView               :: WebView
               ,readerViewWebFrame              :: Frame
               ,readerViewWebScrolledWindow     :: ScrolledWindow
               ,readerViewHPaned                :: HPaned
               ,readerViewVPaned                :: VPaned
               ,readerViewBuffer                :: ReaderBuffer
               ,readerViewBroadcastChannel      :: ViewChannel ReaderTChanSignal
               }
    deriving Typeable

instance PageBuffer ReaderBuffer where
  pageBufferGetName             = return . readerBufferName
  pageBufferSetName _ _         = return ()
  pageBufferClient              = readerBufferClient
  pageBufferCreateView a pId    = PageViewWrap <$> readerViewNew a pId
  pageBufferMode                = readerBufferMode
  pageBufferPackageName _       = fmap takeFileName getDataDir

instance PageView ReaderView where
    pageViewBuffer               = PageBufferWrap . readerViewBuffer
    pageViewPlugId               = readerViewPlugId
    pageViewFrame                = readerViewFrame
    pageViewLocalKeymap _        = readerViewLocalKeymap
    pageViewLocalCommandMap _    = readerViewLocalCommandMap
    pageViewFocus                = widgetGrabFocus . readerViewHPaned
    pageViewPropagateWidget      = castToWidget . readerViewHPaned
    pageViewSaveState view       = readerViewSaveState view Nothing
    pageViewRestoreState view    = readerViewRestoreState view Nothing
    pageViewWriteState view path = readerViewSaveState view (Just path)
    pageViewReadState view path  = readerViewRestoreState view (Just path)

-- | Create reader view.
readerViewNew :: ReaderBuffer -> PagePlugId -> IO ReaderView
readerViewNew buffer plugId = do
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create frame. 
  treeFrame <- frameNew
  navFrame  <- frameNew
  webFrame  <- frameNew

  -- Create scrolled window.
  pFrame <- pageFrameNewWithModeName (pageModeName $ readerBufferMode buffer)
  treeScrolledWindow <- scrolledWindowNew_
  navScrolledWindow  <- scrolledWindowNew_
  webScrolledWindow  <- scrolledWindowNew_

  -- Create paned.
  vPaned <- vPanedNew
  hPaned <- hPanedNew

  -- Create view.
  treeView <- treeViewNew
  navView  <- treeViewNew
  webView  <- webViewNew

  -- List store and sort model.
  treeListStore <- listStoreNew []
  treeSortModel <- treeModelSortNewWithModel treeListStore
  
  navListStore <- listStoreNew []
  navSortModel <- treeModelSortNewWithModel navListStore

  -- Connect.
  treeScrolledWindow `containerAdd` treeView
  navScrolledWindow `containerAdd` navView
  webScrolledWindow `containerAdd` webView

  treeFrame `containerAdd` treeScrolledWindow
  navFrame `containerAdd` navScrolledWindow
  webFrame `containerAdd` webScrolledWindow

  scrolledWindowAddWithViewport (pageFrameScrolledWindow pFrame) vPaned

  panedPack1 hPaned treeFrame True True
  panedPack2 hPaned navFrame  True True 
  panedPack1 vPaned hPaned    True True
  panedPack2 vPaned webFrame  True True

  -- Set default size.
  hPaned `after` realize $ do
    -- Adjust width.
    toplevelWidget <- widgetGetToplevel hPaned
    (width, _)     <- widgetGetSize toplevelWidget
    let widthLeft  = truncate (fromIntegral width / 4)
        widthRight = width - widthLeft
    widgetSetSizeRequest treeFrame widthLeft  (-1)
    widgetSetSizeRequest navFrame  widthRight (-1)
  vPaned `after` realize $ do
    -- Adjust height.
    toplevelWidget <- widgetGetToplevel hPaned
    (_, height)    <- widgetGetSize toplevelWidget
    let heightUp     = truncate (fromIntegral height / 4)
        heightBottom = height - heightUp
    widgetSetSizeRequest hPaned   (-1) heightUp
    widgetSetSizeRequest webFrame (-1) heightBottom

  -- Duplicate broadcast channel.
  channel <- createViewChannel (readerBufferBroadcastChannel buffer) treeView

  -- Build reader view.
  let readerView = ReaderView pId pFrame
                              treeView treeFrame treeScrolledWindow treeListStore treeSortModel
                              navView navFrame navScrolledWindow navListStore navSortModel
                              webView webFrame webScrolledWindow
                              hPaned vPaned 
                              buffer 
                              channel

  -- Read channel.
  readerViewListenChannel readerView

  -- Draw reader view.
  readerViewDraw readerView

  -- Focus first feed.
  readerViewSelectFirstFeed readerView

  -- Signal handle.

  -- Update navigation view when tree view cursor changed.
  treeView `on` cursorChanged $ readerViewUpdateNavItems readerView
  -- Browse page when navigation view cursor changed.
  navView `on` cursorChanged $ readerViewBrowseItemLink readerView
  -- Create new tab when request open new window.
  webView `on` createWebView $ readerViewOpenUrl readerView

  return readerView

-- | Draw view.
readerViewDraw :: ReaderView -> IO ()
readerViewDraw view = do
  -- Draw tree item.
  let buffer = readerViewBuffer view
  treeItems <- readTVarIO $ readerBufferFeedTreeItems buffer
  let treeView  = readerViewTreeView view
      store     = readerViewTreeListStore view
      model     = readerViewTreeSortModel view
      
  -- Append to list store.
  listStoreClear store
  forM_ treeItems (listStoreAppend store) 

  -- Set tree view model.
  treeViewSetModel treeView model

  -- Clean tree view.
  treeViewRemoveColumns treeView

  -- Add column file info to tree view.
  forM_ (readerBufferFeedTreeOptions buffer) (readerViewTreeViewAddColumn treeView store model)

  return ()

-- | Listen broadcast channel for draw view synchronous.
readerViewListenChannel :: ReaderView -> IO ()
readerViewListenChannel view = 
  listenViewChannel (readerViewBroadcastChannel view) $ \signal -> 
      case signal of
        FeedUpdated name -> 
            treeViewGetSelectedValue (readerViewTreeView view) 
                                     (readerViewTreeSortModel view)
                                     (readerViewTreeListStore view)
            >?>= \itemInfo -> 
                when (name == ftiName itemInfo) $                        
                     readerViewDrawNavItems view name

-- | Browse item link.
readerViewBrowseItemLink :: ReaderView -> IO ()
readerViewBrowseItemLink view = do
  let webView = readerViewWebView view
  treeViewGetSelectedValue (readerViewNavView view)
                           (readerViewNavSortModel view)
                           (readerViewNavListStore view)
    >?>= \ itemInfo -> do
      let url = fniUrl itemInfo
      unless (null url) $ do
           -- Stop current loading first.
           webViewStopLoading webView
           -- Load new url.
           webViewLoadUri webView url

-- | Update navigation times.
readerViewUpdateNavItems :: ReaderView -> IO ()
readerViewUpdateNavItems view =
  treeViewGetSelectedValue (readerViewTreeView view)
                           (readerViewTreeSortModel view)
                           (readerViewTreeListStore view)
    >?>= \ itemInfo -> 
      readerViewDrawNavItems view (ftiName itemInfo)

-- | Draw navigation items.
readerViewDrawNavItems :: ReaderView -> FeedName -> IO ()
readerViewDrawNavItems view feedName = do
  let navView = readerViewNavView view
      buffer = readerViewBuffer view
      store = readerViewNavListStore view
      model = readerViewNavSortModel view
  navItems <- readTVarIO $ readerBufferFeedNavItems buffer
  case findMinMatch navItems (\ name _ -> name == feedName) of
    Just (_, items) -> do
      -- Append to list store.
      listStoreClear store
      forM_ items (listStoreAppend store) 

      -- Set tree view model.
      treeViewSetModel navView model

      -- Clean tree view.
      treeViewRemoveColumns navView

      -- Add column file info to tree view.
      forM_ (readerBufferFeedNavOptions buffer) (readerViewNavViewAddColumn navView store model)
    Nothing -> return ()

  -- Browse first item and link.
  readerViewSelectFirstItem view
  readerViewBrowseItemLink view

-- | Add column.
readerViewTreeViewAddColumn :: (TreeItemClass t,
                               TreeViewClass self1,
                               TreeModelClass self,
                               TreeModelSortClass self,
                               TypedTreeModelClass model,
                               TreeSortableClass self) =>
                              self1
                            -> model FeedTreeItem
                            -> self
                            -> (t, SortColumnId)
                            -> IO ()
readerViewTreeViewAddColumn treeView model sortModel option@(info,sortId) = do
  readerViewTreeViewSetSortFunc model sortModel option

  let name = tiGetColumnTitle info
  tvc <- treeViewAddColumnWithTitle treeView name sortId
  
  cell <- cellRendererTextNew
  treeViewColumnPackStart tvc cell True

  readerViewTreeViewSetCellText tvc cell model sortModel info

-- | Set sort function.
readerViewTreeViewSetSortFunc :: (TreeSortableClass self,
                                 TypedTreeModelClass model,
                                 TreeItemClass a) =>
                                model FeedTreeItem
                              -> self
                              -> (a, SortColumnId)
                              -> IO ()
readerViewTreeViewSetSortFunc model sortModel (info, sortId) = 
  treeSortableSetSortFunc sortModel sortId $ \iter1 iter2 -> do
    row1 <- treeModelGetRow model iter1
    row2 <- treeModelGetRow model iter2
    tiCompareRow info row1 row2

-- | Set cell text.
readerViewTreeViewSetCellText :: (CellLayoutClass self,
                                 CellRendererTextClass cell,
                                 TreeModelClass model,
                                 TreeModelSortClass model,
                                 TypedTreeModelClass model1,
                                 TreeItemClass a) =>
                                self
                              -> cell
                              -> model1 FeedTreeItem
                              -> model
                              -> a
                              -> IO ()
readerViewTreeViewSetCellText tvc cell model sortModel info = 
    cellLayoutSetAttributeFunc tvc cell sortModel $ \iter -> do
      row <- treeModelSortGetRow model sortModel iter
      set cell [cellText   := tiGetCellText info row
               ,cellXAlign := tiGetCellXAlign info]

-- | Add column.
readerViewNavViewAddColumn :: (NavItemClass t,
                              TreeViewClass self1,
                              TreeModelClass self,
                              TreeModelSortClass self,
                              TypedTreeModelClass model,
                              TreeSortableClass self) =>
                             self1
                           -> model FeedNavItem
                           -> self
                           -> (t, SortColumnId)
                           -> IO ()
readerViewNavViewAddColumn treeView model sortModel option@(info,sortId) = do
  readerViewNavViewSetSortFunc model sortModel option

  let name = niGetColumnTitle info
      maxWidth = niGetColumnMaxWidth info
  tvc <- treeViewAddColumnWithTitle treeView name sortId
  maxWidth ?>= \width -> treeViewColumnSetMaxWidth tvc width
  
  cell <- cellRendererTextNew
  treeViewColumnPackStart tvc cell True

  readerViewNavViewSetCellText tvc cell model sortModel info

-- | Set sort function.
readerViewNavViewSetSortFunc :: (TreeSortableClass self,
                                TypedTreeModelClass model,
                                NavItemClass a) =>
                               model FeedNavItem
                             -> self
                             -> (a, SortColumnId)
                             -> IO ()
readerViewNavViewSetSortFunc model sortModel (info, sortId) = 
  treeSortableSetSortFunc sortModel sortId $ \iter1 iter2 -> do
    row1 <- treeModelGetRow model iter1
    row2 <- treeModelGetRow model iter2
    niCompareRow info row1 row2

-- | Set cell text.
readerViewNavViewSetCellText :: (CellLayoutClass self,
                                CellRendererTextClass cell,
                                TreeModelClass model,
                                TreeModelSortClass model,
                                TypedTreeModelClass model1,
                                NavItemClass a) =>
                               self
                             -> cell
                             -> model1 FeedNavItem
                             -> model
                             -> a
                             -> IO ()
readerViewNavViewSetCellText tvc cell model sortModel info = 
    cellLayoutSetAttributeFunc tvc cell sortModel $ \iter -> do
      row <- treeModelSortGetRow model sortModel iter
      set cell [cellText   := niGetCellText info row
               ,cellXAlign := niGetCellXAlign info]

-- | Select next feed.
readerViewSelectNextFeed :: ReaderView -> IO ()
readerViewSelectNextFeed view = do
  atLast <- treeViewAtLastToplevelNode (readerViewTreeView view)
  unless atLast $ 
    treeViewFocusNextToplevelNode (readerViewTreeView view) 
    
-- | Select previous feed.
readerViewSelectPrevFeed :: ReaderView -> IO ()    
readerViewSelectPrevFeed view = do
  atFirst <- treeViewAtFirstToplevelNode (readerViewTreeView view)
  unless atFirst $
    treeViewFocusPrevToplevelNode (readerViewTreeView view) 

-- | Select first feed.
readerViewSelectFirstFeed :: ReaderView -> IO ()
readerViewSelectFirstFeed view = do
  atFirst <- treeViewAtFirstToplevelNode (readerViewTreeView view)
  unless atFirst $
    treeViewFocusFirstToplevelNode (readerViewTreeView view) 

-- | Select last feed.
readerViewSelectLastFeed :: ReaderView -> IO ()
readerViewSelectLastFeed view = do
  atLast <- treeViewAtLastToplevelNode (readerViewTreeView view)
  unless atLast $ 
    treeViewFocusLastToplevelNode (readerViewTreeView view) 

-- | Select next item.
readerViewSelectNextItem :: ReaderView -> IO ()
readerViewSelectNextItem view = do
  atLast <- treeViewAtLastToplevelNode (readerViewNavView view)
  unless atLast $ 
    treeViewFocusNextToplevelNode (readerViewNavView view) 
    
-- | Select previous item.
readerViewSelectPrevItem :: ReaderView -> IO ()    
readerViewSelectPrevItem view = do
  atFirst <- treeViewAtFirstToplevelNode (readerViewNavView view)
  unless atFirst $
    treeViewFocusPrevToplevelNode (readerViewNavView view) 

-- | Select first item.
readerViewSelectFirstItem :: ReaderView -> IO ()
readerViewSelectFirstItem view = do
  atFirst <- treeViewAtFirstToplevelNode (readerViewNavView view)
  unless atFirst $
    treeViewFocusFirstToplevelNode (readerViewNavView view) 

-- | Select last item.
readerViewSelectLastItem :: ReaderView -> IO ()
readerViewSelectLastItem view = do
  atLast <- treeViewAtLastToplevelNode (readerViewNavView view)
  unless atLast $ 
    treeViewFocusLastToplevelNode (readerViewNavView view) 

-- | Scroll web view step up.
readerViewScrollWebViewStepUp :: ReaderView -> IO ()
readerViewScrollWebViewStepUp view =
  scrolledWindowScrollVerticalStep True (readerViewWebScrolledWindow view) 

-- | Scroll web view step down.
readerViewScrollWebViewStepDown :: ReaderView -> IO ()
readerViewScrollWebViewStepDown view =
  scrolledWindowScrollVerticalStep False (readerViewWebScrolledWindow view)

-- | Scroll web view page up.
readerViewScrollWebViewPageUp :: ReaderView -> IO ()
readerViewScrollWebViewPageUp view =
  scrolledWindowScrollVerticalPage True (readerViewWebScrolledWindow view)

-- | Scroll web view page down.
readerViewScrollWebViewPageDown :: ReaderView -> IO ()
readerViewScrollWebViewPageDown view =
  scrolledWindowScrollVerticalPage False (readerViewWebScrolledWindow view)

-- | New tab in browser.
readerViewOpenUrl :: ReaderView -> WebFrame -> IO WebView
readerViewOpenUrl view _ = do
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

-- | Browse item in browser module.
readerViewBrowseInBrowser :: ReaderView -> IO ()
readerViewBrowseInBrowser view = 
  treeViewGetSelectedValue (readerViewNavView view)
                           (readerViewNavSortModel view)
                           (readerViewNavListStore view)
    >?>= \ itemInfo -> do
      let url = fniUrl itemInfo
      unless (null url) $   
             mkDaemonSignal (pageViewClient view) NewTab (NewTabArgs "PageBrowser" url [])

-- | Keymap.
readerViewLocalKeymap :: Map Text Text
readerViewLocalKeymap = 
    M.fromList 
         [("n",         "Next feed")
         ,("p",         "Previous feed")
         ,("N",         "Last feed")
         ,("P",         "First feed")
         ,("j",         "Next item")
         ,("k",         "Previous item")
         ,("Down",      "Next item")
         ,("Up",        "Previous item")
         ,("J",         "Last item")
         ,("K",         "First item")
         ,(",",         "Scroll step up")
         ,(".",         "Scroll step down")
         ,(" ",         "Scroll page up")
         ,("b",         "Scroll page down")
         ,("m",         "Read in browser")
         ]

-- | Keymap.
readerViewLocalCommandMap :: Map Text (ReaderView -> IO ())
readerViewLocalCommandMap = 
    M.fromList 
         [("Next feed",         readerViewSelectNextFeed)
         ,("Previous feed",     readerViewSelectPrevFeed)
         ,("Last feed",         readerViewSelectLastFeed)
         ,("First feed",        readerViewSelectFirstFeed)
         ,("Next item",         readerViewSelectNextItem)
         ,("Previous item",     readerViewSelectPrevItem)
         ,("Last item",         readerViewSelectLastItem)
         ,("First item",        readerViewSelectFirstItem)
         ,("Scroll step up",    readerViewScrollWebViewStepUp)
         ,("Scroll step down",  readerViewScrollWebViewStepDown)
         ,("Scroll page up",    readerViewScrollWebViewPageUp)
         ,("Scroll page down",  readerViewScrollWebViewPageDown)
         ,("Read in browser",   readerViewBrowseInBrowser)
         ]

-- | Save state.
readerViewSaveState :: ReaderView -> Maybe FilePath -> IO ()
readerViewSaveState (ReaderView {readerViewBuffer            = buffer
                                ,readerViewTreeView          = feedView
                                ,readerViewNavView           = itemView
                                ,readerViewNavScrolledWindow = itemScrolledWindow
                                ,readerViewWebScrolledWindow = webScrolledWindow}) 
                    statePath = do
  -- Get feed selected path.
  feedSelectedPath <- treeViewGetSelectedPath feedView

  -- Get item selected path.
  itemSelectedPath <- treeViewGetSelectedPath itemView

  -- Get item scroll position.
  itemScrolledWindowPosition <- scrolledWindowGetValue itemScrolledWindow

  -- Get web scroll position.
  webScrolledWindowPosition <- scrolledWindowGetValue webScrolledWindow

  -- Save state.
  let state = ReaderState feedSelectedPath 
                          itemSelectedPath 
                          itemScrolledWindowPosition 
                          webScrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (readerBufferState buffer) state
    Just path -> writeConfigPath path state      

-- | Restore state.
readerViewRestoreState :: ReaderView -> Maybe FilePath -> IO ()
readerViewRestoreState (ReaderView {readerViewBuffer            = buffer
                                   ,readerViewTreeView          = feedView
                                   ,readerViewNavView           = itemView
                                   ,readerViewNavScrolledWindow = itemScrolledWindow
                                   ,readerViewWebScrolledWindow = webScrolledWindow})
                       statePath = do
  bufferState <- readTVarIO (readerBufferState buffer)
  (ReaderState feedSelectedPath 
               itemSelectedPath 
               itemScrolledWindowPosition 
               webScrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore feed selected path.
  feedSelectedPath ?>= \path -> treeViewSetCursor feedView path Nothing

  -- Restore item selected path.
  itemSelectedPath ?>= \path -> treeViewSetCursor itemView path Nothing

  -- Restore item scroll position.
  scrolledWindowSetValue itemScrolledWindow itemScrolledWindowPosition

  -- Restore web scroll position.
  scrolledWindowSetValue webScrolledWindow webScrolledWindowPosition
