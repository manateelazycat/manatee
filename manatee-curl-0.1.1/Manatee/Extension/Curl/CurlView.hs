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
module Manatee.Extension.Curl.CurlView where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import Data.List
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.FileOpenRule
import Manatee.Core.PageFrame
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Extension.Curl.CurlBuffer
import Manatee.Extension.Curl.Types
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Functor
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gio.Gio
import Manatee.Toolkit.Gtk.Concurrent
import Manatee.Toolkit.Gtk.ModelView
import Manatee.Toolkit.Gtk.ScrolledWindow
import Paths_manatee_curl
import System.Directory
import System.FilePath
import System.GIO
import Text.Printf

import qualified Data.Map as M

data CurlView =
    CurlView {curlViewPlugId            :: TVar PagePlugId
             ,curlViewFrame             :: PageFrame
             ,curlViewBuffer            :: CurlBuffer
             ,curlViewTreeView          :: TreeView
             ,curlViewListStore         :: ListStore DownloadFile
             ,curlViewSortModel         :: TypedTreeModelSort DownloadFile
             ,curlViewBroadcastChannel  :: ViewChannel CurlTChanSignal
             }
    deriving Typeable

instance PageBuffer CurlBuffer where
    pageBufferGetName           = return . curlBufferName
    pageBufferSetName _ _       = return ()
    pageBufferClient            = curlBufferClient
    pageBufferCreateView a pId  = PageViewWrap <$> curlViewNew a pId
    pageBufferMode              = curlBufferMode
    pageBufferPackageName _     = fmap takeFileName getDataDir

instance PageView CurlView where
    pageViewBuffer               = PageBufferWrap . curlViewBuffer
    pageViewPlugId               = curlViewPlugId
    pageViewFrame                = curlViewFrame
    pageViewLocalKeymap _        = curlViewLocalKeymap
    pageViewLocalCommandMap _    = curlViewLocalCommandMap
    pageViewFocus                = treeViewFocus . curlViewTreeView
    pageViewPropagateWidget      = castToWidget . curlViewTreeView
    pageViewSaveState view       = curlViewSaveState view Nothing
    pageViewRestoreState view    = curlViewRestoreState view Nothing
    pageViewWriteState view path = curlViewSaveState view (Just path)
    pageViewReadState view path  = curlViewRestoreState view (Just path)
    pageViewScrollToTop          = curlViewScrollToTop
    pageViewScrollToBottom       = curlViewScrollToBottom
    pageViewScrollVerticalPage   = curlViewScrollVerticalPage
    pageViewScrollVerticalStep   = curlViewScrollVerticalStep

-- | New.
curlViewNew :: CurlBuffer -> PagePlugId -> IO CurlView
curlViewNew buffer plugId = do
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ curlBufferMode buffer)

  -- Tree view.
  treeView <- treeViewNew
  treeViewSetEnableTreeLines treeView True
  pageFrameAddChild pFrame treeView

  -- List store.
  listStore <- listStoreNew []

  -- Sort model.
  sortModel <- treeModelSortNewWithModel listStore

  -- Channel.
  channel <- createViewChannel (curlBufferBroadcastChannel buffer) treeView

  -- Curl view.
  let curlView = CurlView pId pFrame buffer treeView listStore sortModel channel

  -- Read channel.
  curlViewListenChannel curlView

  -- Draw view.
  curlViewDraw curlView

  -- Display file name when cursor change.
  treeView `on` cursorChanged $ curlViewDisplayFilenameStatus curlView

  return curlView

-- | Listen broadcast channel for draw view synchronous.
curlViewListenChannel :: CurlView -> IO ()
curlViewListenChannel view = 
  listenViewChannel (curlViewBroadcastChannel view) $ \ signal -> 
      case signal of
        AddDownload downloadFile -> do
          listStoreAppend (curlViewListStore view) downloadFile
          return ()
        UpdateStatus -> 
          widgetQueueDraw (curlViewTreeView view)
        DeleteDownload url -> do
          list <- listStoreToList (curlViewListStore view)
          findIndex (\x -> dfURL x == url) list ?>= \ i -> 
              listStoreRemove (curlViewListStore view) i

-- | Draw curl view.
curlViewDraw :: CurlView -> IO ()
curlViewDraw view = do
  -- Get value.
  let buffer = curlViewBuffer view
  fileInfos <- readTVarIO $ curlBufferFileInfos buffer 
  let treeView  = curlViewTreeView view
      store     = curlViewListStore view
      model     = curlViewSortModel view

  -- Append to list store.
  listStoreClear store
  forM_ fileInfos (listStoreAppend store) 

  -- Set tree view model.
  treeViewSetModel treeView model

  -- Clean tree view.
  treeViewRemoveColumns treeView

  -- Add status icon column.
  curlViewAddStatusIconColumn treeView store (curlBufferDownloadStatusPixbuf buffer)

  -- Add type icon column.
  curlViewAddTypeIconColumn treeView store (curlBufferIconPixbufDatabase buffer)

  -- Get option index.
  let options = curlBufferFileInfoOptions buffer
      splitIndex = 
          case findIndex (\x -> fst x == DFSize) options of
            Just i  -> i + 1
            Nothing -> length options
      (leftOptions, rightOptions) = splitAt splitIndex options
  -- Add column at left of progress.
  forM_ leftOptions (curlViewAddColumn treeView store model)
  -- Add progress column.
  curlViewAddProgressColumn treeView store
  -- Add column at right of progress.
  forM_ rightOptions (curlViewAddColumn treeView store model)

  -- Sort column.
  sortStatus <- readTVarIO $ curlBufferSortStatus buffer
  curlViewSortInternal view sortStatus

  return ()

-- | Add type icon.
curlViewAddStatusIconColumn :: TreeViewClass tv => tv -> ListStore DownloadFile -> DownloadStatusPixbuf -> IO ()
curlViewAddStatusIconColumn treeView store 
                            (DownloadStatusPixbuf {dspPause        = pausePixbuf
                                                  ,dspRunning      = runningPixbuf
                                                  ,dspFinish       = finishPixbuf
                                                  ,dspFailed       = failedPixbuf}) = do
  tvc <- treeViewColumnNew
  set tvc [treeViewColumnTitle := "Status"]
  treeViewAppendColumn treeView tvc
  
  icon <- cellRendererPixbufNew
  treeViewColumnPackStart tvc icon True
  cellLayoutSetAttributes tvc icon store $ \DownloadFile {dfDownloadStatus = statusTVar} ->
    [cellPixbuf :=> do
       status <- readTVarIO statusTVar
       return $ case status of
                  Pause   -> pausePixbuf
                  Running -> runningPixbuf
                  Finish  -> finishPixbuf
                  Failed  -> failedPixbuf]

-- | Add type icon.
curlViewAddTypeIconColumn :: TreeViewClass tv => tv -> ListStore DownloadFile -> TVar FileIconPixbufDatabase -> IO ()
curlViewAddTypeIconColumn treeView store databaseTVar = do
  tvc <- treeViewColumnNew
  set tvc [treeViewColumnTitle := "Type"]
  treeViewAppendColumn treeView tvc
  
  icon <- cellRendererPixbufNew
  treeViewColumnPackStart tvc icon True
  cellLayoutSetAttributes tvc icon store $ \DownloadFile {dfName = nameTVar} ->
    [cellPixbuf :=> do
        name <- readTVarIO nameTVar
        (_, fMime) <- contentTypeGuess name "" 0
        modifyTVarIOM databaseTVar (updateFileIconPixbufDatabaseWithFilePath name)
        database <- readTVarIO databaseTVar
        return $ maybeError (findIconPixbuf database fMime)
                     ("curlViewAddTypeIconColumn: can't find pixbuf match in database : " ++ show fMime)]

-- | Add progress.
curlViewAddProgressColumn :: TreeViewClass tv => tv -> ListStore DownloadFile -> IO ()
curlViewAddProgressColumn treeView store = do
  tvc <- treeViewColumnNew
  set tvc [treeViewColumnTitle    := "Progress"
          ,treeViewColumnMinWidth := 100]
  treeViewAppendColumn treeView tvc
  
  progress <- cellRendererProgressNew
  treeViewColumnPackStart tvc progress True
  cellLayoutSetAttributes tvc progress store $ \DownloadFile {dfProgress = progressTVar} ->
    [cellProgressValue :=> do
       progressValue <- readTVarIO progressTVar
       return $ floor progressValue
    ,cellProgressText :=> do
       progressValue <- readTVarIO progressTVar
       return $ Just (printf "%.2f" progressValue ++ " %")]

-- | Add column.
curlViewAddColumn :: (CurlFileInfoClass t,
                      TreeViewClass self1,
                      TreeModelClass self,
                      TreeModelSortClass self,
                      TypedTreeModelClass model,
                      TreeSortableClass self) =>
                     self1
                   -> model DownloadFile
                   -> self
                   -> (t, SortColumnId)
                   -> IO ()
curlViewAddColumn treeView model sortModel option@(info,sortId) = do
  curlViewSetSortFunc model sortModel option

  let name = getColumnTitle info
  tvc <- treeViewAddColumnWithTitle treeView name sortId
  -- Set max width.
  getCellMaxWidth info 
      ?>= \width -> set tvc [treeViewColumnMaxWidth := width]

  cell <- cellRendererTextNew
  -- Set min width request. 
  set cell [cellTextWidthChars := getCellWidthChars info]
  treeViewColumnPackStart tvc cell True

  curlViewSetCellText tvc cell model sortModel info

-- | Set sort function.
curlViewSetSortFunc :: (TreeSortableClass self,
                        TypedTreeModelClass model,
                        CurlFileInfoClass a) =>
                       model DownloadFile
                     -> self
                     -> (a, SortColumnId)
                     -> IO ()
curlViewSetSortFunc model sortModel (info, sortId) = 
  treeSortableSetSortFunc sortModel sortId $ \iter1 iter2 -> do
    row1 <- treeModelGetRow model iter1
    row2 <- treeModelGetRow model iter2
    compareRow info row1 row2

-- | Set cell text.
curlViewSetCellText :: (CellLayoutClass self,
                        CellRendererTextClass cell,
                        TreeModelClass model,
                        TreeModelSortClass model,
                        TypedTreeModelClass model1,
                        CurlFileInfoClass a) =>
                       self
                     -> cell
                     -> model1 DownloadFile
                     -> model
                     -> a
                     -> IO ()
curlViewSetCellText tvc cell model sortModel info = 
    cellLayoutSetAttributeFunc tvc cell sortModel $ \iter -> do
      row <- treeModelSortGetRow model sortModel iter
      set cell [cellText   :=> getCellText info row
               ,cellXAlign := getCellXAlign info]

-- | Internal sort function.
curlViewSortInternal :: CurlView -> (DownloadFileOption, SortType) -> IO ()
curlViewSortInternal view (option, sortType) = do
  let options = curlBufferFileInfoOptions $ curlViewBuffer view 
  lookup option options ?>= \x -> 
      treeSortableSetSortColumnId (curlViewSortModel view) x sortType

-- | Begin.
curlViewScrollToTop :: CurlView -> IO ()
curlViewScrollToTop = 
    treeViewFocusFirstToplevelNode . curlViewTreeView

-- | End.
curlViewScrollToBottom :: CurlView -> IO ()
curlViewScrollToBottom = 
    treeViewFocusLastToplevelNode . curlViewTreeView 

-- | Scroll page vertically.
curlViewScrollVerticalPage :: Bool -> CurlView -> IO ()
curlViewScrollVerticalPage isDown a = do
  let sw = curlViewScrolledWindow a
      tv = curlViewTreeView a
  pageInc <- (<=<) adjustmentGetPageIncrement scrolledWindowGetVAdjustment sw
  treeViewScrollVertical tv sw (if isDown then pageInc else (- pageInc))

-- | Scroll step vertically.
curlViewScrollVerticalStep :: Bool -> CurlView -> IO ()
curlViewScrollVerticalStep isDown a = do
  let sw = curlViewScrolledWindow a
      tv = curlViewTreeView a
  stepInc <- (<<<=) i2d treeViewGetSelectedCellHeight tv
  treeViewScrollVertical tv sw (if isDown then stepInc else (- stepInc))

-- | Next node.
curlViewNextNode :: CurlView -> IO ()
curlViewNextNode = treeViewFocusNextToplevelNode . curlViewTreeView
    
-- | Previous node.
curlViewPrevNode :: CurlView -> IO ()    
curlViewPrevNode = treeViewFocusPrevToplevelNode . curlViewTreeView

-- | Display file name status.
curlViewDisplayFilenameStatus :: CurlView -> IO ()
curlViewDisplayFilenameStatus view = 
  treeViewGetSelectedValue (curlViewTreeView view)
                           (curlViewSortModel view)
                           (curlViewListStore view)
    >?>= \ downloadFile -> do
      name <- readTVarIO (dfName downloadFile)
      pageViewUpdateStatusbar view "Filename" ("Filename : " ++ name)

-- | Sort column.
curlViewSort :: CurlView -> DownloadFileOption -> IO ()
curlViewSort view option = do
  -- Get model and options.
  let model   = curlViewSortModel view 
      buffer  = curlViewBuffer view
      options = curlBufferFileInfoOptions buffer
  -- Get current sortType and columnId.
  (curSortType, _, curSortColumnId) <- treeSortableGetSortColumnId model
  lookup option options ?>= \id -> do
    treeSortableSetSortColumnId model id $
      if id == curSortColumnId
         -- Just change sort order when sort column id is same.
         then 
             -- Just change sort order.
             case curSortType of
               SortAscending  -> SortDescending
               SortDescending -> SortAscending
         -- Otherwise sort ascending.
         else SortAscending

    -- Get new sort type.
    (newSortType, _, _) <- treeSortableGetSortColumnId model

    -- Update sort status of buffer.
    writeTVarIO (curlBufferSortStatus $ curlViewBuffer view) (option, newSortType)

    -- Focus to cell.
    treeViewFocus (curlViewTreeView view)

-- | Sort by name.
curlViewSortByName :: CurlView -> IO ()
curlViewSortByName view = curlViewSort view DFName

-- | Sort by size.
curlViewSortBySize :: CurlView -> IO ()
curlViewSortBySize view = curlViewSort view DFSize

-- | Sort by download size.
curlViewSortByDownloadSize :: CurlView -> IO ()
curlViewSortByDownloadSize view = curlViewSort view DFDownloadSize

-- | Sort by rest size.
curlViewSortByRestSize :: CurlView -> IO ()
curlViewSortByRestSize view = curlViewSort view DFRestSize

-- | Sort by speed.
curlViewSortBySpeed :: CurlView -> IO ()
curlViewSortBySpeed view = curlViewSort view DFSpeed

-- | Sort by rest time.
curlViewSortByRestTime :: CurlView -> IO ()
curlViewSortByRestTime view = curlViewSort view DFRestTime

-- | Sort by thread number.
curlViewSortByThread :: CurlView -> IO ()
curlViewSortByThread view = curlViewSort view DFThread

-- | Sort by URL.
curlViewSortByUrl :: CurlView -> IO ()
curlViewSortByUrl view = curlViewSort view DFURL

-- | Add download.
curlViewAddDownload :: CurlView -> IO ()
curlViewAddDownload view@(CurlView {curlViewBuffer = buffer}) =
  interactive view [(IString, "URL : ", "")] $ \ [url] -> do
    files <- readTVarIO $ curlBufferFileInfos buffer
    if url `elem` map dfURL files
       then pageViewShowOutputbar view ("Has exist " ++ url) Nothing
       else curlBufferAddDownload buffer url

-- | Pause
curlViewPause :: CurlView -> IO ()
curlViewPause view =
  treeViewGetSelectedValue (curlViewTreeView view)
                           (curlViewSortModel view)
                           (curlViewListStore view)
    >?>= curlBufferPause

-- | Pause all.
curlViewPauseAll :: CurlView -> IO ()
curlViewPauseAll view = 
  listStoreToList (curlViewListStore view)
    >>= \list -> forM_ list curlBufferPause
        
-- | Continue.
curlViewContinue :: CurlView -> IO ()
curlViewContinue view@(CurlView {curlViewBuffer = buffer}) =
  treeViewGetSelectedValue (curlViewTreeView view)
                           (curlViewSortModel view)
                           (curlViewListStore view)
    >?>= curlBufferContinue buffer

-- | Continue all.
curlViewContinueAll :: CurlView -> IO ()
curlViewContinueAll view@(CurlView {curlViewBuffer = buffer}) = 
  listStoreToList (curlViewListStore view)
     >>= \list -> forM_ list (curlBufferContinue buffer) 

-- | Delete download.
curlViewDeleteDownload :: CurlView -> IO ()
curlViewDeleteDownload view@(CurlView {curlViewBuffer = buffer}) =
  treeViewGetSelectedValue (curlViewTreeView view)
                           (curlViewSortModel view)
                           (curlViewListStore view)
    >?>= curlBufferDeleteDownload buffer

-- | Jump to file.
curlViewJumpToFile :: CurlView -> IO ()
curlViewJumpToFile view = 
  treeViewGetSelectedValue (curlViewTreeView view)
                           (curlViewSortModel view)
                           (curlViewListStore view)
    >?>= \ downloadFile -> do
      downloadStatus <- readTVarIO (dfDownloadStatus downloadFile)
      case downloadStatus of
        Failed -> pageViewShowOutputbar view "No file." Nothing
        Finish -> do
          downloadDir <- fmap (</> defaultDownloadDir) getConfigDirectory
          mkDaemonSignal (pageViewClient view) NewTab (NewTabArgs "PageFileManager" downloadDir [])
        _ -> do
          cacheDir <- fmap (</> defaultCacheDir) getConfigDirectory
          mkDaemonSignal (pageViewClient view) NewTab (NewTabArgs "PageFileManager" cacheDir [])

-- | Open file.
curlViewOpenFile :: CurlView -> IO ()
curlViewOpenFile view = 
  treeViewGetSelectedValue (curlViewTreeView view)
                           (curlViewSortModel view)
                           (curlViewListStore view)
    >?>= \ downloadFile -> do
      downloadStatus <- readTVarIO (dfDownloadStatus downloadFile)
      if downloadStatus == Finish
         -- Just try when file download complete.
         then do
           -- Init.
           downloadDir <- fmap (</> defaultDownloadDir) getConfigDirectory
           name <- readTVarIO (dfName downloadFile)
           let filePath = downloadDir </> name
           isExist <- doesFileExist filePath

           if isExist
              -- Try open file.
              then do           
                (_, fileType) <- contentTypeGuess filePath "" 0
                openRule <- fileOpenRule filePath fileType
                if null openRule
                   then pageViewShowOutputbar view ("Don't know how to open file : " ++ filePath) Nothing
                   else do
                     let rule = snd $ head openRule -- use default open rule.
                     rule (pageViewClient view)
              -- Otherwise print information.
              else pageViewShowOutputbar view ("Can't find " ++ filePath) Nothing
         -- Otherwise print information.
         else pageViewShowOutputbar view "Haven't download complete." Nothing

-- | Scrolled window.
curlViewScrolledWindow :: CurlView -> ScrolledWindow      
curlViewScrolledWindow =
  pageFrameScrolledWindow . curlViewFrame

-- | Keymap.
curlViewLocalKeymap :: Map Text Text
curlViewLocalKeymap = 
    M.fromList
         [("j",         "Next")
         ,("k",         "Previous")
         ,("Down",      "Next")
         ,("Up",        "Previous")          
         ,("J",         "Scroll to bottom")
         ,("K",         "Scroll to top")
         ,(" ",         "Scroll page up")
         ,("b",         "Scroll page down")
         ,("PageDown",  "Scroll page up")
         ,("PageUp",    "Scroll page down")
         ,("a",         "Add")
         ,("d",         "Delete")
         ,("p",         "Pause")
         ,("P",         "Pause all")
         ,("n",         "Continue")
         ,("N",         "Continue all")
         ,("f",         "Jump to file")
         ,("m",         "Open file")
         ,("3",         "Sort by name")
         ,("4",         "Sort by size")
         ,("6",         "Sort by speed")
         ,("7",         "Sort by rest time")
         ,("8",         "Sort by thread")
         ,("9",         "Sort by url")
         ]

-- | Keymap.
curlViewLocalCommandMap :: Map Text (CurlView -> IO ())
curlViewLocalCommandMap = 
    M.fromList
         [("Next",              curlViewNextNode)
         ,("Previous",          curlViewPrevNode)
         ,("Scroll to bottom",  curlViewScrollToBottom)
         ,("Scroll to top",     curlViewScrollToTop)
         ,("Scroll page up",    curlViewScrollVerticalPage True)
         ,("Scroll page down",  curlViewScrollVerticalPage False)
         ,("Add",               curlViewAddDownload)
         ,("Delete",            curlViewDeleteDownload)
         ,("Pause",             curlViewPause)
         ,("Pause all",         curlViewPauseAll)
         ,("Continue",          curlViewContinue)
         ,("Continue all",      curlViewContinueAll)
         ,("Jump to file",      curlViewJumpToFile)
         ,("Open file",         curlViewOpenFile)
         ,("Sort by name",      curlViewSortByName)
         ,("Sort by size",      curlViewSortBySize)
         ,("Sort by speed",     curlViewSortBySpeed)
         ,("Sort by rest time", curlViewSortByRestTime)
         ,("Sort by thread",    curlViewSortByThread)
         ,("Sort by url",       curlViewSortByUrl)
         ]

-- | Save state.
curlViewSaveState :: CurlView -> Maybe FilePath -> IO ()
curlViewSaveState view@(CurlView {curlViewBuffer     = buffer
                                 ,curlViewTreeView   = treeView})  
                  statePath = do
  -- Get selected path.
  selectedPath <- treeViewGetSelectedPath treeView

  -- Get scroll position.
  scrolledWindowPosition <- scrolledWindowGetValue (curlViewScrolledWindow view)

  -- Save state.
  let state = CurlState selectedPath scrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (curlBufferState buffer) state
    Just path -> writeConfigPath path state

-- | Restore state.
curlViewRestoreState :: CurlView -> Maybe FilePath -> IO ()
curlViewRestoreState view@(CurlView {curlViewBuffer  = buffer
                                    ,curlViewTreeView= treeView})
                     statePath = do
  bufferState <- readTVarIO (curlBufferState buffer)
  (CurlState selectedPath scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore selected path.
  selectedPath ?>= \path -> treeViewSetCursor treeView path Nothing

  -- Restore scroll position.
  scrolledWindowSetValue (curlViewScrolledWindow view) scrolledWindowPosition
