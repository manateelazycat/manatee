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

module Manatee.Action.Window where

import Control.Monad
import GHC.Conc
import Graphics.UI.Gtk hiding (Window)
import Manatee.Action.Basic
import Manatee.Action.Tabbar
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Types
import Manatee.UI.UIFrame
import Manatee.UI.Window hiding (windowNew)
import Manatee.UI.WindowNode

import qualified Manatee.Toolkit.Data.ListZipper as LZ

-- | Split window vertically.
windowSplitVertically :: Environment -> IO ()
windowSplitVertically = windowSplit DVertical

-- | Split window horizontally.
windowSplitHortizontally :: Environment -> IO ()
windowSplitHortizontally = windowSplit DHortizontal

-- | Spit window with specify direction.
windowSplit :: WindowNodeDirection -> Environment -> IO ()
windowSplit direction env = do
  (tabbarTVar, (tabbar, (windowParent, (container, (focusNotifierList, (windowListTVar, windowNodeListTVar)))))) <- envGet env

  let winParentId = windowGetId windowParent
  tabbarGetPageModeName tabbar winParentId ?>= \modeName -> do
      -- Get data before split parent window.
      let seqList = tabbarGetTabList winParentId tabbar
      currentTabIndex <- notebookGetCurrentPage (windowNotebook windowParent)
      
      -- Split window.
      (windowChild1, windowChild2) <- windowSplitInternal 
                                       direction 
                                       windowParent 
                                       windowListTVar
                                       windowNodeListTVar
                                       focusNotifierList
                                       container
      
      -- Build re-parent sockets for new child window.
      windowChildReparentTabs env windowChild1 seqList modeName 
        
      -- Build clone sockets for new child window.
      windowChildCloneTabs env windowChild2 seqList modeName

      -- Update state.
      modifyTVarIO tabbarTVar (tabbarRemoveTabs winParentId)
      modifyTVarIO windowListTVar (`windowListFocus` windowChild1)

      -- Store tab index with window children.
      notebookSetCurrentPage (windowNotebook windowChild1) currentTabIndex
      notebookSetCurrentPage (windowNotebook windowChild2) currentTabIndex
      
-- | Select next window and focus page.
windowSelectNext :: TVar WindowList -> IO ()
windowSelectNext windowList = 
  modifyTVarIO windowList windowListNextCircular

-- | Select previous window and focus page.
windowSelectPrev :: TVar WindowList -> IO ()
windowSelectPrev windowList = 
  modifyTVarIO windowList windowListPrevCircular

-- | Close current window.
windowCloseCurrent :: Environment -> IO ()
windowCloseCurrent env = do
  -- Get current window and close it.
  window <- envGet env
  windowRemove env window

-- | Close all windows except current one.
windowCloseOthers :: Environment -> IO ()  
windowCloseOthers env = do
  -- Get current window and window list.
  (currentWindow, windowList) <- envGet env
  
  -- Loop window list to close windows.
  forM_ (LZ.toList windowList) $ \ window -> 
    -- Close window that not match current window id.
    when (windowGetId window /= windowGetId currentWindow) $ 
         windowRemove env window

-- | Close all windows.
windowCloseAll :: Environment -> IO ()
windowCloseAll env = do
  windowList <- envGet env
  forM_ (LZ.toList windowList) $ \ window -> 
      windowRemove env window

-- | Remove window.
windowRemove :: Environment -> Window -> IO ()
windowRemove env window = do
  -- Get args.
  (client, (tabbar, args)) <- envGet env

  -- Remove all tabs match window id. 
  newTabbar <- removeTabs tabbar client window
  
  -- Remove window from window list.
  (newWindowList, newWindowNodeList) <- windowRemoveInternal (window, args)

  -- Feed environment back.
  envPut env (newTabbar, (newWindowList, newWindowNodeList))

  -- Exit programe if no window exist.
  when (LZ.isEmpty newWindowList) (exit env False)

-- | Re-parent tabs of parent in child window.
windowChildReparentTabs :: Environment -> Window -> [Tab] -> PageModeName -> IO ()
windowChildReparentTabs env window seqList modeName = do
  (client, (tabbarTVar, signalBoxList)) <- envGet env
  
  let notebook = windowNotebook window
      windowId = windowGetId window
  forM_ seqList $ \ Tab {tabProcessId = processId
                        ,tabPageId    = pageId
                        ,tabPlugId    = oldPlugId} -> do
        -- Create new socket frame.
        uiFrame <- uiFrameStick notebook
         
        -- Create signal box.
        signalBox <- signalBoxNew uiFrame windowId (envSignalBoxIdCounter env) signalBoxList
        let sId = signalBoxId signalBox

        -- Add page id information to tabbar,
        -- then will replace other information after render page create. 
        -- And make sure render page will insert at correct place when call 'daemonHandleNewPageConfirm'.
        modifyTVarIO tabbarTVar (tabbarAddTab windowId modeName (Tab 0 pageId sId 0 0 uiFrame))

        -- Send `ReparentRenderPage` signal.
        mkRenderSignal client processId ReparentRenderPage (ReparentRenderPageArgs pageId oldPlugId sId)
          
-- | Clone tabs of parent in child window.
windowChildCloneTabs :: Environment -> Window -> [Tab] -> PageModeName -> IO ()
windowChildCloneTabs env window seqList modeName = do
  (client, (tabbarTVar, signalBoxList)) <- envGet env

  let tabDataList = map (\x -> (modeName, tabProcessId x, tabPageId x)) seqList
  cloneTabs window client tabbarTVar signalBoxList (envSignalBoxIdCounter env) tabDataList

-- | Enlarge window up.
windowEnlargeUp :: Environment -> IO ()
windowEnlargeUp env = do
  (window, windowNodeList) <- envGet env
  windowNodeZoom windowNodeList (windowNode window) ZUp True
                      
-- | Enlarge window down.
windowEnlargeDown :: Environment -> IO ()
windowEnlargeDown env = do
  (window, windowNodeList) <- envGet env
  windowNodeZoom windowNodeList (windowNode window) ZDown True

-- | Enlarge window left.
windowEnlargeLeft :: Environment -> IO ()
windowEnlargeLeft env = do
  (window, windowNodeList) <- envGet env
  windowNodeZoom windowNodeList (windowNode window) ZLeft True

-- | Enlarge window right.
windowEnlargeRight :: Environment -> IO ()
windowEnlargeRight env = do
  (window, windowNodeList) <- envGet env
  windowNodeZoom windowNodeList (windowNode window) ZRight True

-- | Enlarge window.
windowEnlarge :: Environment -> IO ()
windowEnlarge env = do
  windowEnlargeUp env
  windowEnlargeDown env
  windowEnlargeLeft env
  windowEnlargeRight env

-- | Shrink window up.
windowShrinkUp :: Environment -> IO ()
windowShrinkUp env = do
  (window, windowNodeList) <- envGet env
  windowNodeZoom windowNodeList (windowNode window) ZUp False
                      
-- | Shrink window down.
windowShrinkDown :: Environment -> IO ()
windowShrinkDown env = do
  (window, windowNodeList) <- envGet env
  windowNodeZoom windowNodeList (windowNode window) ZDown False

-- | Shrink window left.
windowShrinkLeft :: Environment -> IO ()
windowShrinkLeft env = do
  (window, windowNodeList) <- envGet env
  windowNodeZoom windowNodeList (windowNode window) ZLeft False

-- | Shrink window right.
windowShrinkRight :: Environment -> IO ()
windowShrinkRight env = do
  (window, windowNodeList) <- envGet env
  windowNodeZoom windowNodeList (windowNode window) ZRight False

-- | Shrink window.
windowShrink :: Environment -> IO ()
windowShrink env = do
  windowShrinkUp env
  windowShrinkDown env
  windowShrinkLeft env
  windowShrinkRight env
