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

module Manatee.Action.Tab where

import Control.Concurrent.STM.TVar
import Control.Monad
import Data.List (partition)
import Graphics.UI.Gtk hiding (Action, Frame, Window)
import Manatee.Action.Basic
import Manatee.Action.BufferList
import Manatee.Action.Tabbar
import Manatee.Action.Window
import Manatee.Core.DBus
import Manatee.Core.PageMode
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Toolkit.General.Basic hiding (swap)
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Notebook
import Manatee.Types
import Manatee.UI.UIFrame
import Manatee.UI.Window hiding (windowNew)

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Sequence as Seq

-- | New tab.
-- If tab has created, switch to tab.
-- Otherwise create tab.
newTab :: PageType -> PagePath -> [String] -> Environment -> IO ()
newTab pType pPath options env = do
  bufferList <- envGet env

  modeName <- getPageModeName pType pPath
  case bufferListGetBufferIndex bufferList modeName pPath of
    -- Switch to buffer if it has exist.
    Just i -> do
      tabSwitchGroupCurrentWindow env modeName
      window <- envGet env
      notebookSetCurrentPage (windowNotebook window) i
    -- Otherwise create buffer.
    Nothing -> newTabInternal pType pPath options env

-- | New path.
newTabInternal :: PageType -> PagePath -> [String] -> Environment -> IO ()
newTabInternal pType pPath options env = do
  (PageTypeRule rule) <- readConfig pageTypeRulePath (PageTypeRule M.empty)
  case findMinMatch rule (\ typ _ -> typ == pType) of
    Nothing -> putStrLn $ "newTabInternal : Can't found rule for `" ++ pType ++ "`"
    Just (_, binaryPath) -> do
      (window, (tabbarTVar, (bufferListTVar, signalBoxList))) <- envGet env
      
      -- Create socket with current window.
      let notebook = windowNotebook window
      pageId <- tickTVarIO (envPageIdCounter env)
      
      -- Switch mode before add new tab.
      modeName <- getPageModeName pType pPath
      tabSwitchGroupCurrentWindow env modeName
      
      -- Add to socket frame in notebook.
      -- Because socket will failed to attached in notebook, 
      -- if haven't any plug add in socket.
      -- So we use frame as socket container, we add socket in 
      -- frame after plug create complete and return plug id.
      uiFrame <- uiFrameStick notebook
      
      -- Build signal box.
      let windowId = windowGetId window
      signalBox <- signalBoxNew uiFrame windowId (envSignalBoxIdCounter env) signalBoxList
      let sId = signalBoxId signalBox
      
      -- Add page id information to buffer list and tabbar,
      -- then will replace other information after render page create. 
      -- And make sure render page will insert at correct place when call 'daemonHandleNewPageConfirm'.
      modifyTVarIO bufferListTVar (bufferListAddBuffer (modeName, 0, pageId, pType, ""))
      modifyTVarIO tabbarTVar (tabbarAddTab windowId modeName (Tab 0 pageId sId 0 0 uiFrame))
      
      -- Spawn render process.
      runProcess_ binaryPath [show (SpawnRenderProcessArgs pageId pType pPath options [sId] False)] 
      
      return ()

-- | Move tab to left in next window.
tabMoveToLeftWithNextWindow :: Environment -> IO ()  
tabMoveToLeftWithNextWindow env = 
  withNextWindow env $ \win -> tabMoveToLeft (win, env)

-- | Move tab to right in next window.
tabMoveToRightWithNextWindow :: Environment -> IO ()  
tabMoveToRightWithNextWindow env = 
  withNextWindow env $ \win -> tabMoveToRight (win, env)

-- | Move tab to begin in next window.
tabMoveToBeginWithNextWindow :: Environment -> IO ()  
tabMoveToBeginWithNextWindow env = 
  withNextWindow env $ \win -> tabMoveToBegin (win, env)

-- | Move tab to end in next window.
tabMoveToEndWithNextWindow :: Environment -> IO ()  
tabMoveToEndWithNextWindow env = 
  withNextWindow env $ \win -> tabMoveToEnd (win, env)

-- | Move tab to left.
tabMoveToLeft :: (Window, Environment) -> IO ()
tabMoveToLeft (window, env) = do
  (windowList, tabbar) <- envGet env
  
  windowListGetWindow (windowGetId window) windowList ?>= \ window -> do
    let notebook = windowNotebook window
    unlessM (notebookAtStart notebook) $ 
        tabbarGetPageModeName tabbar (windowGetId window) ?>= \modeName -> do  
          currentPageIndex <- notebookGetCurrentPage notebook
          tabMove env modeName currentPageIndex (currentPageIndex - 1)

-- | Move tab to right.
tabMoveToRight :: (Window, Environment) -> IO ()
tabMoveToRight (window, env) = do
  (windowList, tabbar) <- envGet env
  
  windowListGetWindow (windowGetId window) windowList ?>= \ window -> do
    let notebook = windowNotebook window
    unlessM (notebookAtEnd notebook) $ 
        tabbarGetPageModeName tabbar (windowGetId window) ?>= \modeName -> do  
          currentPageIndex <- notebookGetCurrentPage notebook
          tabMove env modeName currentPageIndex (currentPageIndex + 1)

-- | Move tab to first.
tabMoveToBegin :: (Window, Environment) -> IO ()
tabMoveToBegin (window, env) = do
  (windowList, tabbar) <- envGet env
  
  windowListGetWindow (windowGetId window) windowList ?>= \ window -> do
    let notebook = windowNotebook window
    unlessM (notebookAtStart notebook) $ 
        tabbarGetPageModeName tabbar (windowGetId window) ?>= \modeName -> do  
          currentPageIndex <- notebookGetCurrentPage notebook
          firstIndex <- notebookFirstIndex notebook
          tabMove env modeName currentPageIndex firstIndex

-- | Move tab to end.
tabMoveToEnd :: (Window, Environment) -> IO ()
tabMoveToEnd (window, env) = do
  (windowList, tabbar) <- envGet env

  windowListGetWindow (windowGetId window) windowList ?>= \ window -> do
    let notebook = windowNotebook window
    unlessM (notebookAtEnd notebook) $ 
        tabbarGetPageModeName tabbar (windowGetId window) ?>= \modeName -> do  
          currentPageIndex <- notebookGetCurrentPage notebook
          lastIndex <- notebookLastIndex notebook
          tabMove env modeName currentPageIndex lastIndex

-- | Move tab.
tabMove :: Environment -> PageModeName -> Int -> Int -> IO ()
tabMove env pageModeName currentIndex targetIndex = do
  (bufferListTVar, tabbarTVar) <- envGet env

  -- Swap buffer.
  modifyTVarIO bufferListTVar (bufferListSwapBuffer pageModeName currentIndex targetIndex)
  -- Swap tab in notebook.
  windowSwapTab env pageModeName currentIndex targetIndex
  -- Swap tabbar.
  modifyTVarIO tabbarTVar (tabbarSwapTab pageModeName currentIndex targetIndex)

-- | Select next tab.
tabSelectNext :: Window -> IO ()
tabSelectNext = 
  notebookSelectNextPage . windowNotebook

-- | Select prev tab.
tabSelectPrev :: Window -> IO ()
tabSelectPrev = 
  notebookSelectPrevPage . windowNotebook

-- | Select first tab.
tabSelectFirst :: Window -> IO ()
tabSelectFirst =
  notebookSelectFirstPage . windowNotebook

-- | Select last tab.
tabSelectLast :: Window -> IO ()
tabSelectLast = 
  notebookSelectLastPage . windowNotebook

-- | Select next tab in other window.
tabSelectNextWithNextWindow :: Environment -> IO ()
tabSelectNextWithNextWindow env = 
    withNextWindow env tabSelectNext

-- | Select previous tab in other window.
tabSelectPrevWithNextWindow :: Environment -> IO ()    
tabSelectPrevWithNextWindow env = 
    withNextWindow env tabSelectPrev

-- | Select first tab in other window.
tabSelectFirstWithNextWindow :: Environment -> IO ()    
tabSelectFirstWithNextWindow env = 
    withNextWindow env tabSelectFirst

-- | Select last tab in other window.
tabSelectLastWithNextWindow :: Environment -> IO ()    
tabSelectLastWithNextWindow env = 
    withNextWindow env tabSelectLast

-- | Close specify tab.
tabClose :: Environment -> PageId -> IO ()
tabClose env tPageId = do
  (client, (window, (tabbarTVar, bufferListTVar))) <- envGet env
  
  (Tabbar tabbar) <- readTVarIO tabbarTVar
  tabbarGetTabInfo (Tabbar tabbar) (windowGetId window)
      ?>= \ (modeName, tabSeq) -> 
          -- Find page index that match page id.
          Seq.findIndexL (\x -> tabPageId x == tPageId) tabSeq
      ?>= \ currentPageIndex -> 
          -- Get match tab.
          F.toList tabSeq ?! currentPageIndex 
      ?>= \ Tab {tabProcessId = processId
                ,tabPageId    = pageId} -> do

          -- When delete last buffer in buffer list, 
          -- mode name information will delete from buffer list.
          -- So we need get next mode name before update buffer list
          -- for switch next mode.
          nextMode <- 
            getWindowPageModeName env window 
                >?>=> \currentModeName -> do
                    (BufferList bufferList) <- readTVarIO bufferListTVar
                    return $ findNextCycle (== currentModeName) (M.keys bufferList)

          -- Push close tab to history list.
          pushCloseTab env modeName pageId

          -- Update buffer list. 
          modifyTVarIO bufferListTVar (bufferListRemoveBuffer modeName currentPageIndex) 

          -- Just unique tab name when current page mode NOT in 'pageModeDuplicateTabList'.
          pageModeDuplicateTabList <- getDuplicateTabList
          unless (modeName `elem` pageModeDuplicateTabList) $
                 modifyTVarIO bufferListTVar (bufferListUniqueName modeName)
                                              
          -- Remove tab widget from window notebook.
          windowRemoveTab env modeName currentPageIndex nextMode

          -- Update tabbar
          modifyTVarIO tabbarTVar (tabbarRemoveTab modeName currentPageIndex)

          -- Send `ExitRenderProcess` signal to exit render process.
          mkRenderSignal client processId ExitRenderProcess (ExitRenderProcessArgs pageId False)    

          -- Close all window if haven't any buffer exist.
          bufferList <- readTVarIO bufferListTVar
          unless (bufferListHaveBufferExist bufferList) $ 
            windowCloseAll env

-- | Push to tab close history.
pushCloseTab :: Environment -> PageModeName -> PageId -> IO ()
pushCloseTab env pageModeName pageId = do
  (bufferList, tabCloseHistoryTVar) <- envGet env
  
  bufferListGetBuffer bufferList pageModeName pageId 
      ?>= \ Buffer {bufferPageType = pageType
                   ,bufferPath     = pagePath} -> 
          -- Push close tab to history list.
          modifyTVarIO tabCloseHistoryTVar $ \ (TabCloseHistory historyList) -> 
              let newItem   = (pageModeName, pageType, pagePath)
                  (_, list) = partition (== newItem) historyList
              in TabCloseHistory (newItem : list)

-- | Undo close tab.
tabUndoCloseGlobal :: Environment -> IO ()
tabUndoCloseGlobal env = do
  tabCloseHistoryTVar <- envGet env
  (TabCloseHistory history) <- readTVarIO tabCloseHistoryTVar

  -- Undo last close tab if history list not empty.
  unless (null history) $ do
    -- Open last close tab.
    let ([(_, pageType, pagePath)], restList) = splitAt 1 history
    newTab pageType pagePath [] env         

    -- Update history list.
    writeTVarIO tabCloseHistoryTVar (TabCloseHistory restList)

-- | Undo close tab that same mode as current mode.
tabUndoCloseLocal :: Environment -> IO ()
tabUndoCloseLocal env = do
      (tabCloseHistoryTVar, window) <- envGet env
      (TabCloseHistory history) <- readTVarIO tabCloseHistoryTVar
      getWindowPageModeName env window
          >?>= \ currentModeName -> do
            -- Get history list that same as current mode.
            let filterList = filter (\ (modeName, _, _) -> modeName == currentModeName) history

            -- Just open last tab when history list not empty.
            unless (null filterList) $ do
              -- Open last close tab.
              let ([undoItem@(_, pageType, pagePath)], _) = splitAt 1 filterList
              newTab pageType pagePath [] env         

              -- Update history list.
              writeTVarIO tabCloseHistoryTVar (TabCloseHistory (snd $ partition (== undoItem) history))

-- | Get current page id.
tabGetCurrentPageId :: Environment -> IO (Maybe PageId)
tabGetCurrentPageId env = do
  (window, tabbarTVar) <- envGet env

  -- Get current page index.
  let windowId = windowGetId window
  currentPageIndex <- notebookGetCurrentPage (windowNotebook window)
  
  -- Get current page id.
  (Tabbar tabbar) <- readTVarIO tabbarTVar
  tabbarGetTabInfo (Tabbar tabbar) windowId 
      ?>=> \ (_, tabSeq) -> 
          F.toList tabSeq ?! currentPageIndex 
      ?>=> \ Tab {tabPageId = pageId} -> 
          return $ Just pageId

-- | Close current tab.
tabCloseCurrent :: Environment -> IO ()
tabCloseCurrent env = 
  tabGetCurrentPageId env 
      >?>= \ pageId ->
          tabClose env pageId

-- | Close all tabs except current one.
tabCloseOthers :: Environment -> IO ()
tabCloseOthers env = do
  (window, tabbarTVar) <- envGet env

  tabGetCurrentPageId env 
      >?>= \ currentPageId -> do
          let windowId = windowGetId window
          (Tabbar tabbar) <- readTVarIO tabbarTVar
          -- Loop tab sequence, close all tabs except current one.
          tabbarGetTabInfo (Tabbar tabbar) windowId 
              ?>= \ (_, tabSeq) -> 
                  forM_ (F.toList tabSeq) $ \ Tab {tabPageId = pageId} -> 
                    when (pageId /= currentPageId) $
                         tabClose env pageId

-- | Switch to next mode.
tabForwardGroup :: (Window, Environment) -> IO ()  
tabForwardGroup (window, env) = 
  tabForwardGroupWithWindow env window

-- | Switch to previous mode.
tabBackwardGroup :: (Window, Environment) -> IO ()  
tabBackwardGroup (window, env) = 
  tabBackwardGroupWithWindow env window

-- | Switch to next mode with next window.
tabForwardGroupWithNextWindow :: Environment -> IO ()
tabForwardGroupWithNextWindow env = 
    withNextWindow env (tabForwardGroupWithWindow env)

-- | Switch to next mode with next window.
tabBackwardGroupWithNextWindow :: Environment -> IO ()
tabBackwardGroupWithNextWindow env = 
    withNextWindow env (tabBackwardGroupWithWindow env)

-- | Switch to next mode with window.
tabBackwardGroupWithWindow :: Environment -> Window -> IO ()  
tabBackwardGroupWithWindow env window = do
  (BufferList bufferList) <- envGet env
  
  getWindowPageModeName env window 
    >?>= \currentModeName -> 
        findPrevCycle (\ (modeName, _) -> modeName == currentModeName) (M.toList bufferList)    
    ?>= \ (prevModeName, _) ->
        tabSwitchGroupWithWindow env window prevModeName

-- | Switch to next mode with window.
tabForwardGroupWithWindow :: Environment -> Window -> IO ()  
tabForwardGroupWithWindow env window = do
  (BufferList bufferList) <- envGet env
  
  getWindowPageModeName env window 
    >?>= \currentModeName -> 
        findNextCycle (\ (modeName, _) -> modeName == currentModeName) (M.toList bufferList)    
    ?>= \ (nextModeName, _) ->
        tabSwitchGroupWithWindow env window nextModeName

-- | Switch mode.
tabSwitchGroupCurrentWindow :: Environment -> PageModeName -> IO ()
tabSwitchGroupCurrentWindow env pageModeName = do
  window <- envGet env
  tabSwitchGroupWithWindow env window pageModeName

-- | Switch mode with window.
tabSwitchGroupWithWindow :: Environment -> Window -> PageModeName -> IO ()
tabSwitchGroupWithWindow env window pageModeName = do
  (Tabbar tabbar, (tabbarTVar, (tabbarSelectTVar, (BufferList bufferList, (client, signalBoxList))))) <- envGet env

  let windowId = windowGetId window
      windowMode = findMinMatch tabbar (\ (wId, wModeName) _ -> wId == windowId && wModeName == pageModeName)
  case windowMode of
    -- Don't switch if current mode same as request one.
    Just _  -> return ()
    Nothing -> do
      -- Save tab index status that match current mode.
      tabbarGetPageModeName (Tabbar tabbar) (windowGetId window) 
          ?>= \ currentPageMode -> do
                    currentTabIndex <- notebookGetCurrentPage (windowNotebook window)
                    modifyTVarIO tabbarSelectTVar $ \ (TabbarSelect oldMap) -> 
                        TabbarSelect (M.insert currentPageMode currentTabIndex oldMap)

      -- Remove all tabs match window id.
      modifyTVarIOM tabbarTVar $ \ tabs -> removeTabs tabs client window
      
      -- Rebuild all tabs match target mode if can found same mode in buffer list.
      findMinMatch bufferList (\ name _ -> name == pageModeName) 
        ?>= \ (_, bufferSeq) -> do
            -- Clone tabs.
            let tabDataList = map (\x -> (pageModeName, bufferProcessId x, bufferPageId x)) $ F.toList bufferSeq
            cloneTabs window client tabbarTVar signalBoxList (envSignalBoxIdCounter env) tabDataList

            -- Restore tab index status that match current mode.
            TabbarSelect tabbarSelect <- readTVarIO tabbarSelectTVar
            findMinMatch tabbarSelect (\ n _ -> n == pageModeName) 
                         ?>= \ (_, tabIndex) -> 
                             notebookSetCurrentPage (windowNotebook window) tabIndex

-- | Remove tab from window.
windowRemoveTab :: Environment -> PageModeName -> Int -> Maybe PageModeName -> IO ()
windowRemoveTab env modeName currentPageIndex nextMode = do
  (Tabbar tabbar, windowList) <- envGet env

  forM_ (M.toList tabbar) $ \ ((windowId, pageModeName), _) -> 
      when (pageModeName == modeName) $ 
           windowListGetWindow windowId windowList 
           ?>= \ window -> do
             -- Remove tab widget.
             notebookRemovePage (windowNotebook window) currentPageIndex
             -- Synchronization tab name.
             syncTabName env windowId
             -- Switch to next mode when all tabs in current mode has delete.
             number <- notebookGetNPages (windowNotebook window)
             when (number == 0) $ 
                  nextMode ?>= \mode ->
                    tabSwitchGroupWithWindow env window mode

-- | Swap tab in window.
windowSwapTab :: Environment -> PageModeName -> Int -> Int -> IO ()
windowSwapTab env pageModeName currentIndex targetIndex = do
  (Tabbar tabbar, windowList) <- envGet env

  let filterMap = M.filterWithKey (\(_, modeName) _ -> modeName == pageModeName) tabbar
  forM_ (M.toList filterMap) $ \ ((windowId, _), _) -> 
    windowListGetWindow windowId windowList ?>= \ window -> 
      notebookGetNthPage (windowNotebook window) currentIndex >?>= \ child -> 
        notebookReorderChild (windowNotebook window) child targetIndex
