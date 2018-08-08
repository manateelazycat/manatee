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
module Manatee.Extension.ProcessManager.ProcessView where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import Data.List (findIndex)
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Manatee.Core.Config
import Manatee.Core.PageFrame
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Extension.ProcessManager.ProcessBuffer
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Functor
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Concurrent
import Manatee.Toolkit.Gtk.ModelView
import Manatee.Toolkit.Gtk.ScrolledWindow
import Paths_manatee_processmanager
import System.FilePath
import System.Linux.Proc

import qualified Data.Map as M

data ProcessView =
    ProcessView {processViewPlugId              :: TVar PagePlugId
                ,processViewFrame               :: PageFrame
                ,processViewBuffer              :: ProcessBuffer
                ,processViewTreeView            :: TreeView
                ,processViewListStore           :: ListStore ProcStatus
                ,processViewSortModel           :: TypedTreeModelSort ProcStatus
                ,processViewBroadcastChannel    :: ViewChannel ProcTChanSignal
                }
    deriving Typeable

instance PageBuffer ProcessBuffer where
    pageBufferGetName           = return . processBufferName
    pageBufferSetName _ _       = return ()
    pageBufferClient            = processBufferClient
    pageBufferCreateView a pId  = PageViewWrap <$> processViewNew a pId
    pageBufferMode              = processBufferMode
    pageBufferPackageName _     = fmap takeFileName getDataDir

instance PageView ProcessView where
    pageViewBuffer               = PageBufferWrap . processViewBuffer
    pageViewPlugId               = processViewPlugId
    pageViewFrame                = processViewFrame
    pageViewLocalKeymap _        = processViewLocalKeymap
    pageViewLocalCommandMap _    = processViewLocalCommandMap
    pageViewFocus                = treeViewFocus . processViewTreeView
    pageViewPropagateWidget      = castToWidget . processViewTreeView
    pageViewSaveState view       = processViewSaveState view Nothing
    pageViewRestoreState view    = processViewRestoreState view Nothing
    pageViewWriteState view path = processViewSaveState view (Just path)
    pageViewReadState view path  = processViewRestoreState view (Just path)
    pageViewScrollToTop          = processViewScrollToTop
    pageViewScrollToBottom       = processViewScrollToBottom
    pageViewScrollVerticalPage   = processViewScrollVerticalPage
    pageViewScrollVerticalStep   = processViewScrollVerticalStep

-- | Create process view.
processViewNew :: ProcessBuffer -> PagePlugId -> IO ProcessView
processViewNew buffer plugId = do
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ processBufferMode buffer)

  -- Tree view.
  treeView <- treeViewNew
  treeViewSetEnableTreeLines treeView True
  pageFrameAddChild pFrame treeView

  -- List store.
  listStore <- listStoreNew []

  -- Sort model.
  sortModel <- treeModelSortNewWithModel listStore

  -- Channel.
  channel <- createViewChannel (processBufferBroadcastChannel buffer) treeView

  -- Process view.
  let processView = ProcessView pId pFrame buffer treeView listStore sortModel channel

  -- Read channel.
  processViewListenChannel processView

  -- Tick view counter.
  counter <- tickTVarIO (processBufferViewCounter buffer)
  when (counter == 1) $ processBufferUpdate buffer
  treeView `onDestroy` do
    crockTVarIO (processBufferViewCounter buffer)
    return ()

  -- Draw view.
  processViewDraw processView

  return processView

-- | Listen broadcast channel for draw view synchronous.
processViewListenChannel :: ProcessView -> IO ()
processViewListenChannel view = 
    listenViewChannel (processViewBroadcastChannel view) $ \signal -> 
        case signal of
          KillProcess index -> 
              listStoreRemove (processViewListStore view) index
          UpdateProcesses updateInfos -> do
              -- Get selected process id before update.
              pid <- 
                      treeViewGetSelectedValue (processViewTreeView view)
                                               (processViewSortModel view)
                                               (processViewListStore view)
                      >?>=> \info ->
                              return $ Just $ psProcessId info
              
              -- Update process view.
              processViewUpdate view updateInfos
              
              -- Restore select path after update.
              pid ?>= \id -> do
                      list <- listStoreToList (processViewListStore view)
                      findIndex (\x -> psProcessId x == id) list ?>= \ i -> do
                          path <- treeModelSortConvertChildPathToPath (processViewSortModel view) [i]
                          treeViewSetCursor (processViewTreeView view) path Nothing
              
          _ -> return ()

-- | Draw process view.
processViewDraw :: ProcessView -> IO ()
processViewDraw view = do
  -- Get value.
  let buffer = processViewBuffer view
  fileInfos <- readTVarIO $ processBufferStatus buffer 
  let treeView  = processViewTreeView view
      store     = processViewListStore view
      model     = processViewSortModel view

  -- Append to list store.
  listStoreClear store
  forM_ fileInfos (listStoreAppend store) 

  -- Set tree view model.
  treeViewSetModel treeView model

  -- Clean tree view.
  treeViewRemoveColumns treeView

  -- Add column file info to tree view.
  forM_ (processBufferOptions buffer) (processViewAddColumn treeView store model)

  -- Sort column.
  sortStatus <- readTVarIO $ processBufferSortStatus buffer
  processViewSortInternal view sortStatus

  return ()

-- | Update process view.
processViewUpdate :: ProcessView -> ([ProcStatus], [ProcStatus], [ProcStatus]) -> IO ()
processViewUpdate view (addInfos, deleteInfos, diffInfos) = do
  -- Get value.
  let store  = processViewListStore view

  -- Delete old process first.
  forM_ deleteInfos $ \deleteItem -> do
      list <- listStoreToList (processViewListStore view)
      findIndex (\x -> psProcessId x == psProcessId deleteItem) list ?>= \ i -> 
          listStoreRemove store i

  -- Update change value.
  forM_ diffInfos $ \diffItem -> do
      list <- listStoreToList (processViewListStore view)
      findIndex (\x -> psProcessId x == psProcessId diffItem) list ?>= \ i -> 
          listStoreSetValue store i diffItem
          
  -- Add new process.
  forM_ addInfos $ \addItem -> 
      listStoreAppend store addItem

-- | Internal sort function.
processViewSortInternal :: ProcessView -> (ProcOption, SortType) -> IO ()
processViewSortInternal view (option, sortType) = do
  let options = processBufferOptions $ processViewBuffer view 
  lookup option options ?>= \x -> 
      treeSortableSetSortColumnId (processViewSortModel view) x sortType

-- | Add column.
processViewAddColumn :: (ProcStatusClass t,
                      TreeViewClass self1,
                      TreeModelClass self,
                      TreeModelSortClass self,
                      TypedTreeModelClass model,
                      TreeSortableClass self) =>
                     self1
                   -> model ProcStatus
                   -> self
                   -> (t, SortColumnId)
                   -> IO ()
processViewAddColumn treeView model sortModel option@(info,sortId) = do
  processViewSetSortFunc model sortModel option

  let name = getColumnTitle info
      maxWidth = getColumnMaxWidth info
  tvc <- treeViewAddColumnWithTitle treeView name sortId
  maxWidth ?>= \width -> treeViewColumnSetMaxWidth tvc width

  cell <- cellRendererTextNew
  treeViewColumnPackStart tvc cell True

  processViewSetCellText tvc cell model sortModel info

-- | Set sort function.
processViewSetSortFunc :: (TreeSortableClass self,
                        TypedTreeModelClass model,
                        ProcStatusClass a) =>
                       model ProcStatus
                     -> self
                     -> (a, SortColumnId)
                     -> IO ()
processViewSetSortFunc model sortModel (info, sortId) = 
  treeSortableSetSortFunc sortModel sortId $ \iter1 iter2 -> do
    row1 <- treeModelGetRow model iter1
    row2 <- treeModelGetRow model iter2
    compareRow info row1 row2

-- | Set cell text.
processViewSetCellText :: (CellLayoutClass self,
                        CellRendererTextClass cell,
                        TreeModelClass model,
                        TreeModelSortClass model,
                        TypedTreeModelClass model1,
                        ProcStatusClass a) =>
                       self
                     -> cell
                     -> model1 ProcStatus
                     -> model
                     -> a
                     -> IO ()
processViewSetCellText tvc cell model sortModel info = 
    cellLayoutSetAttributeFunc tvc cell sortModel $ \iter -> do
      row <- treeModelSortGetRow model sortModel iter
      set cell [cellText   := getCellText info row
               ,cellXAlign := getCellXAlign info]

-- | Keymap.
processViewLocalKeymap :: Map Text Text
processViewLocalKeymap = 
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
         ,(";",         "Kill process")
         ,("1",         "Sort by name")
         ,("2",         "Sort by process id")
         ,("3",         "Sort by user")
         ,("4",         "Sort by state")
         ,("5",         "Sort by memory")
         ,("6",         "Sort by cpu")
         ,("7",         "Sort by priority")
         ,("8",         "Sort by threads")
         ,("9",         "Sort by command")
         ]

-- | Keymap.
processViewLocalCommandMap :: Map Text (ProcessView -> IO ())
processViewLocalCommandMap = 
    M.fromList
         [("Next",              processViewNextNode)
         ,("Previous",          processViewPrevNode)
         ,("Scroll to bottom",  processViewScrollToBottom)
         ,("Scroll to top",     processViewScrollToTop)
         ,("Scroll page up",    processViewScrollVerticalPage True)
         ,("Scroll page down",  processViewScrollVerticalPage False)
         ,("Kill process",      processViewKillProcess)
         ,("Sort by name",      processViewSortByName)
         ,("Sort by process id",processViewSortByProcessId)
         ,("Sort by user",      processViewSortByUser)
         ,("Sort by state",     processViewSortByState)
         ,("Sort by memory",    processViewSortByMemory)
         ,("Sort by cpu",       processViewSortByCPU)
         ,("Sort by priority",  processViewSortByPriority)
         ,("Sort by threads",   processViewSortByThreads)
         ,("Sort by command",   processViewSortByCmdline)
         ]

-- | Next node.
processViewNextNode :: ProcessView -> IO ()
processViewNextNode = treeViewFocusNextToplevelNode . processViewTreeView
    
-- | Previous node.
processViewPrevNode :: ProcessView -> IO ()    
processViewPrevNode = treeViewFocusPrevToplevelNode . processViewTreeView

-- | Scroll to top.
processViewScrollToTop :: ProcessView -> IO ()
processViewScrollToTop = 
    treeViewFocusFirstToplevelNode . processViewTreeView

-- | Scroll to bottom.
processViewScrollToBottom :: ProcessView -> IO ()
processViewScrollToBottom = 
    treeViewFocusLastToplevelNode . processViewTreeView 

-- | Scroll page vertically.
processViewScrollVerticalPage :: Bool -> ProcessView -> IO ()
processViewScrollVerticalPage isDown a = do
  let sw = processViewScrolledWindow a
      tv = processViewTreeView a
  pageInc <- (<=<) adjustmentGetPageIncrement scrolledWindowGetVAdjustment sw
  treeViewScrollVertical tv sw (if isDown then pageInc else (- pageInc))

-- | Scroll step vertically.
processViewScrollVerticalStep :: Bool -> ProcessView -> IO ()
processViewScrollVerticalStep isDown a = do
  let sw = processViewScrolledWindow a
      tv = processViewTreeView a
  stepInc <- (<<<=) i2d treeViewGetSelectedCellHeight tv
  treeViewScrollVertical tv sw (if isDown then stepInc else (- stepInc))

-- | Sort by process name.
processViewSortByName :: ProcessView -> IO ()
processViewSortByName view = processViewSort view MCommand

-- | Sort by user.
processViewSortByUser :: ProcessView -> IO ()
processViewSortByUser view = processViewSort view MUser

-- | Sort by process id.
processViewSortByProcessId :: ProcessView -> IO ()
processViewSortByProcessId view = processViewSort view MProcessId

-- | Sort by process state.
processViewSortByState :: ProcessView -> IO ()
processViewSortByState view = processViewSort view MState

-- | Sort by parent process id.
processViewSortByParentId :: ProcessView -> IO ()
processViewSortByParentId view = processViewSort view MParentProcessId

-- | Sort by group id.
processViewSortByGroupId :: ProcessView -> IO ()
processViewSortByGroupId view = processViewSort view MProcessGroupId

-- | Sort by session id.
processViewSortBySessionId :: ProcessView -> IO ()
processViewSortBySessionId view = processViewSort view MSessionId

-- | Sort by priority.
processViewSortByPriority :: ProcessView -> IO ()
processViewSortByPriority view = processViewSort view MPriority

-- | Sort by child threads of process.
processViewSortByThreads :: ProcessView -> IO ()
processViewSortByThreads view = processViewSort view MThreads

-- | Sort by CPU percent.
processViewSortByCPU :: ProcessView -> IO ()
processViewSortByCPU view = processViewSort view MCPUPercent

-- | Sort by memory.
processViewSortByMemory :: ProcessView -> IO ()
processViewSortByMemory view = processViewSort view MResidentMemory

-- | Sort by command line.
processViewSortByCmdline :: ProcessView -> IO ()
processViewSortByCmdline view = processViewSort view MCmdline

-- | Sort column.
processViewSort :: ProcessView -> ProcOption -> IO ()
processViewSort view option = do
  -- Get model and options.
  let model   = processViewSortModel view 
      buffer  = processViewBuffer view
      options = processBufferOptions buffer
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
    writeTVarIO (processBufferSortStatus $ processViewBuffer view) (option, newSortType)

    -- Focus to cell.
    treeViewFocus (processViewTreeView view)

-- | Kill process.
processViewKillProcess :: ProcessView -> IO ()
processViewKillProcess view = do
  let treeView = processViewTreeView view
  treeViewGetSelectedPath treeView
    >?>= \ path -> do
      -- Get process id.
      currentPath <- treeModelSortConvertPathToChildPath (processViewSortModel view) path
      info <- listStoreGetValue (processViewListStore view) (head currentPath)
      let processId = psProcessId info 
          processName = psCommand info

      -- Kill process.
      runCommand_ ("kill " ++ show processId)

      -- Remove process from view.
      processViewRemoveProcess view (head currentPath)

      -- Popup notify message.
      pageViewShowOutputbar view ("Kill process '" ++ processName ++ "' (" ++ show processId ++ ")") Nothing
  
-- | Remove process from view.  
processViewRemoveProcess :: ProcessView -> Int -> IO ()
processViewRemoveProcess view index = 
    -- Write message to broadcast channel to update all view of current buffer.
    writeTChanIO (viewChannel $ processViewBroadcastChannel view) (KillProcess index)

-- | Scrolled window.
processViewScrolledWindow :: ProcessView -> ScrolledWindow
processViewScrolledWindow =
    pageFrameScrolledWindow . processViewFrame

-- | Save state.
processViewSaveState :: ProcessView -> Maybe FilePath -> IO ()
processViewSaveState view@(ProcessView {processViewBuffer     = buffer
                                       ,processViewTreeView   = treeView}) 
                     statePath = do
  -- Get selected path.
  selectedPath <- treeViewGetSelectedPath treeView

  -- Get scroll position.
  scrolledWindowPosition <- scrolledWindowGetValue (processViewScrolledWindow view)

  -- Save state.
  let state = ProcessState selectedPath scrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (processBufferState buffer) state
    Just path -> writeConfigPath path state

-- | Restore state.
processViewRestoreState :: ProcessView -> Maybe FilePath -> IO ()
processViewRestoreState view@(ProcessView {processViewBuffer  = buffer
                                          ,processViewTreeView= treeView}) 
                        statePath = do
  bufferState <- readTVarIO (processBufferState buffer)
  (ProcessState selectedPath scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore selected path.
  selectedPath ?>= \path -> treeViewSetCursor treeView path Nothing

  -- Restore scroll position.
  scrolledWindowSetValue (processViewScrolledWindow view) scrolledWindowPosition
