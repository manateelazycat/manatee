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

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Manatee.Extension.FileManager.DiredView where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import Data.ByteString.UTF8
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
import Manatee.Extension.FileManager.DiredBuffer
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.FilePath
import Manatee.Toolkit.General.Functor
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gio.Gio
import Manatee.Toolkit.Gtk.Concurrent
import Manatee.Toolkit.Gtk.ModelView
import Manatee.Toolkit.Gtk.ScrolledWindow
import Paths_manatee_filemanager
import System.FilePath

import qualified Data.Map as M

data DiredView = 
    DiredView {diredViewPlugId              :: TVar PagePlugId
              ,diredViewFrame               :: PageFrame
              ,diredViewBuffer              :: DiredBuffer
              ,diredViewTreeView            :: TreeView
              ,diredViewListStore           :: ListStore DiredFileInfo
              ,diredViewSortModel           :: TypedTreeModelSort DiredFileInfo
              ,diredViewBroadcastChannel    :: ViewChannel DiredBufferSignal
              } deriving Typeable

instance PageBuffer DiredBuffer where
    pageBufferGetName           = readTVarIO . diredBufferCurrentDirectory
    pageBufferSetName a         = writeTVarIO (diredBufferCurrentDirectory a)
    pageBufferClient            = diredBufferClient
    pageBufferCreateView a pId  = PageViewWrap <$> diredViewNew a pId
    pageBufferMode              = diredBufferMode
    pageBufferPackageName _     = fmap takeFileName getDataDir

instance PageView DiredView where
    pageViewBuffer               = PageBufferWrap . diredViewBuffer
    pageViewPlugId               = diredViewPlugId
    pageViewFrame                = diredViewFrame
    pageViewLocalKeymap _        = diredViewLocalKeymap
    pageViewLocalCommandMap _    = diredViewLocalCommandMap
    pageViewPropagateWidget      = castToWidget . diredViewTreeView
    pageViewSaveState view       = diredViewSaveState view Nothing
    pageViewRestoreState view    = diredViewRestoreState view Nothing
    pageViewWriteState view path = diredViewSaveState view (Just path)
    pageViewReadState view path  = diredViewRestoreState view (Just path)
    pageViewFocus                = treeViewFocus . diredViewTreeView
    pageViewScrollToTop          = diredViewScrollToTop
    pageViewScrollToBottom       = diredViewScrollToBottom
    pageViewScrollVerticalPage   = diredViewScrollVerticalPage
    pageViewScrollVerticalStep   = diredViewScrollVerticalStep

-- | Internal new function.
diredViewNew :: DiredBuffer -> PagePlugId -> IO DiredView
diredViewNew buffer plugId = do 
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ diredBufferMode buffer)

  -- Tree view.
  treeView <- treeViewNew
  treeViewSetEnableTreeLines treeView True
  pageFrameAddChild pFrame treeView

  -- List store.
  listStore <- listStoreNew []

  -- Sort model.
  sortModel <- treeModelSortNewWithModel listStore

  -- Channel.
  channel <- createViewChannel (diredBufferBroadcastChannel buffer) treeView

  -- Dired view.
  let diredView = DiredView pId pFrame buffer treeView listStore sortModel channel

  -- Read channel.
  diredViewListenChannel diredView

  -- Draw view.
  diredViewDraw diredView

  return diredView

-- | Listen broadcast channel for draw view synchronous.
diredViewListenChannel :: DiredView -> IO ()
diredViewListenChannel view = 
  listenViewChannel (diredViewBroadcastChannel view) $ \ signal -> 
      case signal of
        UpdateView directory -> do
            -- Draw dired view.
            diredViewDraw view
            case directory of 
              -- Select current directory.
              Just dir -> do
                list <- listStoreToList (diredViewListStore view)
                findIndex (\x -> fst (dfiNameDescrible x) == dir) list ?>= \ i -> do
                    path <- treeModelSortConvertChildPathToPath (diredViewSortModel view) [i]
                    treeViewSetCursor (diredViewTreeView view) path Nothing
              -- Or select first file.
              Nothing -> 
                treeViewFocusFirstToplevelNode (diredViewTreeView view)
        UpdateFile addInfos deleteInfos -> do
          putStrLn "Got UpdateFile signal."
          diredViewUpdateFile view addInfos deleteInfos
        _ -> return ()

-- | Update file information.
diredViewUpdateFile :: DiredView -> [DiredFileInfo] -> [DiredFileInfo] -> IO ()
diredViewUpdateFile view addInfos deleteInfos = do
  -- Get store.
  let store = diredViewListStore view

  -- Delete files.
  forM_ deleteInfos $ \deleteItem -> do
    list <- listStoreToList (diredViewListStore view)
    findIndex (\x -> dfiNameDescrible x == dfiNameDescrible deleteItem) list ?>= \ i ->
        listStoreRemove store i

  -- Add files.
  forM_ addInfos $ \addItem -> 
      listStoreAppend store addItem

-- | Entry directory.
diredViewEntryDirectory :: DiredView -> FilePath -> Maybe String -> IO ()
diredViewEntryDirectory view path currentDir = do
  let channel = viewChannel $ diredViewBroadcastChannel view 
  writeTChanIO channel (UpdateBuffer path)
  diredBufferLoad (diredViewBuffer view) path
  writeTChanIO channel (UpdateView currentDir)

-- | Draw dired view.
diredViewDraw :: DiredView -> IO ()
diredViewDraw view = do
  -- Get value.
  let buffer = diredViewBuffer view
  fileInfos <- readTVarIO $ diredBufferFileInfos buffer 
  let treeView  = diredViewTreeView view
      store     = diredViewListStore view
      model     = diredViewSortModel view

  -- Append to list store.
  listStoreClear store
  forM_ fileInfos (listStoreAppend store) 

  -- Set tree view model.
  treeViewSetModel treeView model

  -- Clean tree view.
  treeViewRemoveColumns treeView

  -- Add icon column.
  diredViewAddIconColumn treeView store (diredBufferIconPixbufDatabase buffer)

  -- Add column file info to tree view.
  forM_ (diredBufferFileInfoOptions buffer) (diredViewAddColumn treeView store model)

  -- Sort column.
  sortStatus <- readTVarIO $ diredBufferSortStatus buffer
  diredViewSortInternal view sortStatus

  return ()

-- | Add file icon.
diredViewAddIconColumn :: TreeViewClass tv => tv -> ListStore DiredFileInfo -> TVar FileIconPixbufDatabase -> IO ()
diredViewAddIconColumn treeView store databaseTVar = do
  tvc <- treeViewColumnNew
  set tvc [treeViewColumnTitle := ""]
  treeViewAppendColumn treeView tvc
  
  icon <- cellRendererPixbufNew
  treeViewColumnPackStart tvc icon True

  cellLayoutSetAttributes tvc icon store $ \DiredFileInfo {dfiMimeDescrible = (fMime, _)} ->
    [cellPixbuf :=> do
       database <- readTVarIO databaseTVar
       return $ maybeError (findIconPixbuf database fMime)
                   ("diredViewAddIconColumn: can't find pixbuf match in database : " ++ show fMime)]

-- | Add column.
diredViewAddColumn :: (DiredFileInfoClass t,
                      TreeViewClass self1,
                      TreeModelClass self,
                      TreeModelSortClass self,
                      TypedTreeModelClass model,
                      TreeSortableClass self) =>
                     self1
                   -> model DiredFileInfo
                   -> self
                   -> (t, SortColumnId)
                   -> IO ()
diredViewAddColumn treeView model sortModel option@(info,sortId) = do
  diredViewSetSortFunc model sortModel option

  let name = getColumnTitle info
  tvc <- treeViewAddColumnWithTitle treeView name sortId

  cell <- cellRendererTextNew
  treeViewColumnPackStart tvc cell True

  diredViewSetCellText tvc cell model sortModel info

-- | Set sort function.
diredViewSetSortFunc :: (TreeSortableClass self,
                        TypedTreeModelClass model,
                        DiredFileInfoClass a) =>
                       model DiredFileInfo
                     -> self
                     -> (a, SortColumnId)
                     -> IO ()
diredViewSetSortFunc model sortModel (info, sortId) = 
  treeSortableSetSortFunc sortModel sortId $ \iter1 iter2 -> do
    row1 <- treeModelGetRow model iter1
    row2 <- treeModelGetRow model iter2
    compareRow info row1 row2

-- | Set cell text.
diredViewSetCellText :: (CellLayoutClass self,
                        CellRendererTextClass cell,
                        TreeModelClass model,
                        TreeModelSortClass model,
                        TypedTreeModelClass model1,
                        DiredFileInfoClass a) =>
                       self
                     -> cell
                     -> model1 DiredFileInfo
                     -> model
                     -> a
                     -> IO ()
diredViewSetCellText tvc cell model sortModel info = 
    cellLayoutSetAttributeFunc tvc cell sortModel $ \iter -> do
      row <- treeModelSortGetRow model sortModel iter
      set cell [cellText   := getCellText info row
               ,cellXAlign := getCellXAlign info]

-- | Next node.
diredViewNextNode :: DiredView -> IO ()
diredViewNextNode = treeViewFocusNextToplevelNode . diredViewTreeView
    
-- | Previous node.
diredViewPrevNode :: DiredView -> IO ()    
diredViewPrevNode = treeViewFocusPrevToplevelNode . diredViewTreeView

-- | Entry.
diredViewEntryNode :: Bool -> DiredView -> IO ()
diredViewEntryNode newTab view = 
  treeViewGetSelectedValue (diredViewTreeView view)
                           (diredViewSortModel view)
                           (diredViewListStore view)
    >?>= \ fileInfo -> do
      let fileName = (fst . dfiNameDescrible) fileInfo
          fileType = (fst . dfiMimeDescrible) fileInfo 
      filePath <- liftM (</> fileName) $ pageBufferGetName (diredViewBuffer view)
      let displayPath = filepathGetDisplayName (fromString filePath)
      if directoryDoesExist (fromString filePath)
         -- Open directory.
         then if newTab
                 then mkDaemonSignal (pageViewClient view) NewTab (NewTabArgs "PageFileManager" filePath [])
                 else diredViewEntryDirectory view filePath Nothing
         -- Or open file.
         else if fileDoesExist (fromString filePath) 
                 then do
                   -- Open file with rule.
                   openRule <- fileOpenRule filePath fileType
                   if null openRule
                      then pageFrameShowOutputbar
                               (diredViewFrame view) 
                               ("Don't know how to open file : " ++ displayPath) 
                               Nothing
                      else do
                        let rule = snd $ head openRule -- use default open rule
                        rule (pageViewClient view)
                 else pageFrameShowOutputbar 
                          (diredViewFrame view) 
                          ("diredViewEntryNode: " ++ displayPath ++ " is not valid filepath.") 
                          Nothing

-- | Entry upper directory.
diredViewUpperDirectory :: Bool -> DiredView -> IO ()
diredViewUpperDirectory newTab view = do
  -- Get current directory.
  let buffer = diredViewBuffer view
  dir <- pageBufferGetName buffer
  -- Get upper directory.
  let upperDir = getUpperDirectory dir 
  unless (dir == upperDir) $ 
    if newTab
       -- Send 'NewTab' signal to daemon process if open upper directory in new tab.
       then mkDaemonSignal (pageViewClient view) NewTab (NewTabArgs "PageFileManager" upperDir [])
       -- Otherwise new upper directory in current buffer and select current directory.
       else diredViewEntryDirectory view upperDir (Just (takeFileName $ dropTrailingPathSeparator dir))

-- | Sort by file name.
diredViewSortByName :: DiredView -> IO ()
diredViewSortByName view = diredViewSort view FIName

-- | Sort by file size.
diredViewSortBySize :: DiredView -> IO ()
diredViewSortBySize view = diredViewSort view FISize


-- | Sort by file mime type.
diredViewSortByMime :: DiredView -> IO ()
diredViewSortByMime view = diredViewSort view FIMime

-- | Sort by modified time.
diredViewSortByMTime :: DiredView -> IO ()
diredViewSortByMTime view = diredViewSort view FIMTime

-- | Sort column.
diredViewSort :: DiredView -> FileInfoOption -> IO ()
diredViewSort view option = do
  -- Get model and options.
  let model = diredViewSortModel view 
      buffer = diredViewBuffer view
      options = diredBufferFileInfoOptions buffer
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
    writeTVarIO (diredBufferSortStatus $ diredViewBuffer view) (option, newSortType)

    -- Focus to cell.
    treeViewFocus (diredViewTreeView view)
    
-- | Internal sort function.
diredViewSortInternal :: DiredView -> (FileInfoOption, SortType) -> IO ()
diredViewSortInternal view (option, sortType) = do
  let options = diredBufferFileInfoOptions $ diredViewBuffer view 
  lookup option options ?>= \x -> 
      treeSortableSetSortColumnId (diredViewSortModel view) x sortType

-- | Keymap.
diredViewLocalKeymap :: Map Text Text
diredViewLocalKeymap = 
    M.fromList
         [("j",         "Next")
         ,("k",         "Previous")
         ,("Down",      "Next")
         ,("Up",        "Previous")          
         ,("J",         "Scroll to top")
         ,("K",         "Scroll to bottom")
         ,(" ",         "Scroll page up")
         ,("b",         "Scroll page download")
         ,("PageDown",  "Scroll page up")
         ,("PageUp",    "Scroll page download")
         ,("n",         "Sort by name")
         ,("x",         "Sort by type")
         ,("s",         "Sort by size")
         ,("t",         "Sort by modified time")
         ,("'",         "Upper directory")
         ,("\"",        "Upper directory in new tab")
         ,("m",         "Entry")
         ,("Return",    "Entry")
         ,("M",         "Entry in new tab")
         ]

-- | Keymap.
diredViewLocalCommandMap :: Map Text (DiredView -> IO ())
diredViewLocalCommandMap = 
    M.fromList
         [("Next",                      diredViewNextNode)
         ,("Previous",                  diredViewPrevNode)
         ,("Scroll to top",             diredViewScrollToBottom)
         ,("Scroll to bottom",          diredViewScrollToTop)
         ,("Scroll page up",            diredViewScrollVerticalPage True)
         ,("Scroll page down",          diredViewScrollVerticalPage False)
         ,("Sort by name",              diredViewSortByName)
         ,("Sort by type",              diredViewSortByMime)
         ,("Sort by size",              diredViewSortBySize)
         ,("Sort by modified time",     diredViewSortByMTime)
         ,("Upper directory",           diredViewUpperDirectory False)
         ,("Upper directory in new tab",diredViewUpperDirectory True)
         ,("Entry",                     diredViewEntryNode False)
         ,("Entry in new tab",          diredViewEntryNode True)
         ]

-- | Begin.
diredViewScrollToTop :: DiredView -> IO ()
diredViewScrollToTop = 
    treeViewFocusFirstToplevelNode . diredViewTreeView

-- | End.
diredViewScrollToBottom :: DiredView -> IO ()
diredViewScrollToBottom = 
    treeViewFocusLastToplevelNode . diredViewTreeView 

-- | Scroll page vertically.
diredViewScrollVerticalPage :: Bool -> DiredView -> IO ()
diredViewScrollVerticalPage isDown a = do
  let sw = diredViewScrolledWindow a
      tv = diredViewTreeView a
  pageInc <- (<=<) adjustmentGetPageIncrement scrolledWindowGetVAdjustment sw
  treeViewScrollVertical tv sw (if isDown then pageInc else (- pageInc))

-- | Scroll step vertically.
diredViewScrollVerticalStep :: Bool -> DiredView -> IO ()
diredViewScrollVerticalStep isDown a = do
  let sw = diredViewScrolledWindow a
      tv = diredViewTreeView a
  stepInc <- (<<<=) i2d treeViewGetSelectedCellHeight tv
  treeViewScrollVertical tv sw (if isDown then stepInc else (- stepInc))

-- | Scrolled window.
diredViewScrolledWindow :: DiredView -> ScrolledWindow
diredViewScrolledWindow =
  pageFrameScrolledWindow . diredViewFrame

-- | Save state.
diredViewSaveState :: DiredView -> Maybe FilePath -> IO ()
diredViewSaveState view@(DiredView {diredViewBuffer     = buffer
                                   ,diredViewTreeView   = treeView}) 
                   statePath = do
  -- Get selected path.
  selectedPath <- treeViewGetSelectedPath treeView

  -- Get scroll position.
  scrolledWindowPosition <- scrolledWindowGetValue (diredViewScrolledWindow view)

  -- Save state.
  let state = DiredState selectedPath scrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (diredBufferState buffer) state
    Just path -> writeConfigPath path state

-- | Restore state.
diredViewRestoreState :: DiredView -> Maybe FilePath -> IO ()
diredViewRestoreState view@(DiredView {diredViewBuffer  = buffer
                                      ,diredViewTreeView= treeView})
                      statePath = do
  bufferState <- readTVarIO (diredBufferState buffer)
  (DiredState selectedPath scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore selected path.
  selectedPath ?>= \path -> treeViewSetCursor treeView path Nothing

  -- Restore scroll position.
  scrolledWindowSetValue (diredViewScrolledWindow view) scrolledWindowPosition
