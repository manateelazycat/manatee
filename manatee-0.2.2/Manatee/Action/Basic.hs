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

{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, TypeSynonymInstances, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Manatee.Action.Basic where

import Control.Applicative hiding (empty)
import Control.Concurrent.STM.TVar
import Control.Monad.State hiding (get)
import DBus.Client hiding (Signal)
import Data.List (partition)
import Data.Text.Lazy (Text)
import Graphics.UI.Gtk hiding (Action, Frame, Window, layoutPath)
import Manatee.Action.BufferList
import Manatee.Action.Tabbar
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Toolkit.Data.ListZipper hiding (length, delete, get)
import Manatee.Toolkit.Data.SetList
import Manatee.Toolkit.General.FilePath
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.Seq
import Manatee.Toolkit.General.Set hiding (mapM)
import Manatee.Toolkit.General.State
import Manatee.Toolkit.Gtk.Container
import Manatee.Toolkit.Gtk.Gtk
import Manatee.Toolkit.Widget.NotebookTab
import Manatee.Types
import Manatee.UI.FocusNotifier
import Manatee.UI.Frame
import Manatee.UI.UIFrame
import Manatee.UI.Window hiding (windowNew)
import Manatee.UI.WindowNode
import System.Directory
import System.FilePath
import System.Posix.Types (ProcessID)

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Graphics.UI.Gtk as Gtk
import qualified Manatee.Toolkit.Data.ListZipper as LZ

type PageActionInputArgs = (Window, (TVar SignalBoxList, (TVar PageId, TVar SignalBoxId)))

-- | Run action.
runAction :: Environment -> Action -> IO ()
runAction env (Action {actionFun = fun}) =
  envGet env >>= fun >>= envPut env  

(==>) :: forall a b . (ActionInputArgs a, ActionOutputArgs b) => Text -> (a -> IO b) -> (Text, Action)
key ==> command = (key, Action command)

-- | Build socket frame.
socketFrameNew :: IO Gtk.Frame
socketFrameNew = frameNewWithShadowType Nothing

-- | Create new signal box.
signalBoxNew :: UIFrame -> WindowId -> TVar SignalBoxId -> TVar SignalBoxList -> IO SignalBox
signalBoxNew uiFrame windowId signalBoxCounter signalBoxList = do
  -- Ticker signal box counter.
  signalBoxId <- tickTVarIO signalBoxCounter

  -- Create signal box.
  let signalBox = SignalBox signalBoxId uiFrame windowId

  -- Add new SignalBox to list.
  runTVarStateT signalBoxList $ put . Set.insert signalBox

  return signalBox

-- | Clone tabs.
cloneTabs :: Window -> Client 
          -> TVar Tabbar -> TVar SignalBoxList -> TVar SignalBoxId 
          -> [(PageModeName, ProcessID, PageId)] -> IO ()
cloneTabs window client tabbarTVar signalBoxList sId = 
  mapM_ (cloneTab window client tabbarTVar signalBoxList sId)

-- | Clone tab.
cloneTab :: Window -> Client 
         -> TVar Tabbar -> TVar SignalBoxList -> TVar SignalBoxId 
         -> (PageModeName, ProcessID, PageId) -> IO ()
cloneTab  window client tabbarTVar signalBoxList sId (modeName, processId, pageId) = do
  let windowId = windowGetId window
      notebook = windowNotebook window

  -- Create new socket frame.
  uiFrame <- uiFrameStick notebook
    
  -- Create signal box.
  signalBox <- signalBoxNew uiFrame windowId sId signalBoxList
  let sId = signalBoxId signalBox

  -- Add page id information to tabbar,
  -- then will replace other information after render page create. 
  -- And make sure render page will insert at correct place when call 'daemonHandleNewPageConfirm'.
  modifyTVarIO tabbarTVar (tabbarAddTab windowId modeName (Tab 0 pageId sId 0 0 uiFrame))
                             
  -- Send `CloneRenderPage` signal.
  mkRenderSignal client processId CloneRenderPage (CloneRenderPageArgs pageId sId)

-- | Synchronization tab name.
syncTabName :: Environment -> WindowId -> IO ()
syncTabName env windowId = do
  (tabbar, BufferList bufferList) <- envGet env

  tabbarGetTabInfo tabbar windowId 
    ?>= \ (modeName, tabSeq) -> 
        M.lookup modeName bufferList 
    ?>= \ seqBuffer -> do
        let nameList = map bufferName $ F.toList seqBuffer
        zipWithIndexM_ nameList $ \name index -> 
            maybeIndex tabSeq index ?>= \tab -> 
                notebookTabSetName (uiFrameNotebookTab $ tabUIFrame tab) name

-- | Get current uiFrame.
getCurrentUIFrame :: Environment -> IO (Maybe UIFrame)
getCurrentUIFrame env = 
  getCurrentTab env >?>=> \ tab -> return $ Just (tabUIFrame tab)

-- | Get page mode name of specify window.
getWindowPageModeName :: Environment -> Window -> IO (Maybe PageModeName)  
getWindowPageModeName env window = do
  tabbar <- envGet env
  return $ tabbarGetPageModeName tabbar (windowGetId window)

-- | Get current tab.
getCurrentTab :: Environment -> IO (Maybe Tab)
getCurrentTab env = do
  (tabbar, window) <- envGet env

  tabbarGetTabSeq tabbar (windowGetId window) ?>=> \ tabSeq -> do
      currentPageIndex <- notebookGetCurrentPage (windowNotebook window)
      return $ maybeIndex tabSeq currentPageIndex

-- | Get next window.
getNextWindow :: WindowList -> Maybe Window       
getNextWindow windowList
    | LZ.length windowList <= 1
        = Nothing
    | otherwise
        = LZ.getRightCircular windowList

-- | Action in next window.
withNextWindow :: Environment -> (Window -> IO ()) -> IO ()
withNextWindow env action = do
  windowList <- envGet env
  
  case getNextWindow windowList of
    Just win -> action win
    Nothing  -> putStrLn "Just current window exist."

-- | Focus current tab..
focusCurrentTab :: Environment -> IO ()
focusCurrentTab env = do
  client <- envGet env
  
  -- Send `FocusRenderPage` signal to focus page.
  getCurrentTab env >?>= \ Tab {tabProcessId = processId
                               ,tabPlugId    = plugId} -> 
          mkRenderSignal client processId FocusRenderPage (FocusRenderPageArgs plugId)

-- | Update current tab.
updateConfigure :: Environment -> IO ()
updateConfigure env = do
  client <- envGet env
  
  -- Send `UpdateConfig` signal to focus tab.
  getCurrentTab env >?>= \ Tab {tabProcessId = processId} ->
      mkRenderSignal client processId UpdateConfig UpdateConfigArgs

-- | Install current tab.
installConfigure :: Environment -> IO ()
installConfigure env = do
  client <- envGet env
  
  -- Send `InstallConfig` signal to focus tab.
  getCurrentTab env >?>= \ Tab {tabProcessId = processId} ->
      mkRenderSignal client processId InstallConfig InstallConfigArgs

-- | Remove tabs match window id. 
removeTabs :: Tabbar -> Client -> Window -> IO Tabbar    
removeTabs (Tabbar tabbar) client window = do
  let windowId = windowGetId window
      notebook = windowNotebook window
      
  -- Remove all tab widget from notebook.
  containerRemoveAll notebook

  -- Send tab destroy signal to child process. 
  forM_ (tabbarGetTabList windowId (Tabbar tabbar)) $ 
    \ Tab {tabProcessId = processId
          ,tabPlugId    = plugId} -> 
    mkRenderSignal client processId DestroyRenderPage (DestroyRenderPageArgs plugId)

  -- Return new tabbar that remove all tabs match window id.
  return $ tabbarRemoveTabs windowId (Tabbar tabbar)

-- | Exit all render processes.
exitAllRenderProcess :: Environment -> IO ()
exitAllRenderProcess env = do
  (client, bufferListTVar) <- envGet env
  
  modifyTVarIOM bufferListTVar $ \ (BufferList bufferList) -> do
    -- Send dbus signal `ExitRenderProcess` to all render processes.
    forM_ (M.toList bufferList) $ \ (_, bufferSeq) -> 
      forM_ (F.toList bufferSeq) $ \ Buffer {bufferProcessId = processId
                                            ,bufferPageId    = pageId} -> 
         mkRenderSignal client processId ExitRenderProcess (ExitRenderProcessArgs pageId True)
      
    -- Clean up buffer list.
    return $ BufferList M.empty

-- | Focus window.
focusWindow :: Environment -> IO ()    
focusWindow env = do
  focusCurrentTab env
  envGet env >>= highlightCurrentWindow

-- | Highlight window.
highlightCurrentWindow :: (Window, TVar FocusNotifierList) -> IO ()
highlightCurrentWindow (window, focusNotifierList) =  
  focusNotifierShow (windowGetId window) focusNotifierList

-- | Synchronization new tab in window.
tabbarSyncNewTab :: Environment -> WindowId -> DaemonSignalArgs -> IO ()
tabbarSyncNewTab env wId (NewRenderPageConfirmArgs pageId _ _ _ processId modeName _ _ _) = do
  (tabbarTVar, (Tabbar tabbar, (windowList, (client, signalBoxList)))) <- envGet env
  
  -- Synchronization tab in all same mode window.
  forM_ (M.toList tabbar) $ \ ((windowId, pageModeName), tabSeq) -> 
    -- When window mode same as tab mode and not current window.
    when (windowId /= wId && pageModeName == modeName) $ 
      -- And tab haven't exist in current window.
      unless (any (\x -> tabProcessId x == processId) (F.toList tabSeq)) $
          -- Then clone tab in current window.
          windowListGetWindow windowId windowList ?>= \window -> 
              cloneTab window client tabbarTVar signalBoxList (envSignalBoxIdCounter env) (modeName, processId, pageId)

-- | Get top-level container. 
getToplevelContainer :: Environment -> Container
getToplevelContainer = toContainer . envFrame

-- | Get focus window.
getFocusWindow :: Environment -> IO Window  
getFocusWindow env = do
  let container         = getToplevelContainer env
      focusNotifierList = envFocusNotifierList env
      windowList        = envWindowList env
      windowNodeList    = envWindowNodeList env          

  focusWindow <- windowListGetFocusWindow windowList windowNodeList focusNotifierList container 

  case focusWindow of
    Just window -> return window
    Nothing -> error "getFocusWindow: can't found any window."

-- | Exit program.
exit :: Environment -> Bool -> IO ()
exit env saveState = do
  -- Clean old status first.
  cleanOldLayoutState
  -- Save layout state.
  when saveState $ saveLayoutState env
  -- Send broadcast quit signal, other process can listen this signal when daemon process quit.
  client <- envGet env
  mkDaemonBroadcastSignal client ExitDaemonProcess ExitDaemonProcessArgs
  -- Need exit all render processes before exit daemon process.
  exitAllRenderProcess env
  -- Exit daemon process.
  mainQuit

-- | Clean old yout state.
cleanOldLayoutState :: IO ()
cleanOldLayoutState = do
  configDir <- getConfigDirectory
  let stateDir = configDir </> statePath 
  isExist <- doesDirectoryExist stateDir
  when isExist $ do
     removeDirectoryRecursive stateDir
     createDirectoryIfMissing True stateDir

-- | Save layout state.
saveLayoutState :: Environment -> IO ()
saveLayoutState env = do
  -- Init.
  (BufferList bufferList, (Tabbar tabbar, (windowList, (ListZipper windowLeftList windowRightList, windowNodeList)))) <- envGet env

  -- Get window node state.
  let windowNodeStateCounter = listCounter windowNodeList 
  windowNodeStateList <- 
      fmap Set.fromList $
      mapM (\ node -> 
                WindowNodeState <$> pure (windowNodeId node) 
                                <*> readTVarIO (windowNodeParentId node)
                                <*> readTVarIO (windowNodeChildLeftId node)
                                <*> readTVarIO (windowNodeChildRightId node)
                                <*> readTVarIO (windowNodeType node)
                                <*> pure (windowNodeDirection node)) 
               (setListGetList windowNodeList)

  -- Get window state.
  let getWindowStateList = 
          mapM (\ Window {windowNode = node} -> do
                    let winId = windowNodeId node
                    (Rectangle _ _ w h) <- widgetGetAllocation $ windowNodePaned node
                    return $ WindowState winId (w, h))
  leftList  <- getWindowStateList windowLeftList
  rightList <- getWindowStateList windowRightList

  -- Get tabbar state.
  pageIdCounter <- readTVarIO (envPageIdCounter env)
  signalBoxIdCounter <- readTVarIO (envSignalBoxIdCounter env)
  
  tabPages <- newTVarIO Set.empty
  tStateMap <- 
      forM (M.toList tabbar) $ \ ((windowId, pageModeName), tabSeq) -> do
         tabbarStateSeq <-
                 mapM (\tab -> do
                         let pageId = tabPageId tab
                         modifyTVarIO tabPages (Set.insert pageId)
                         return $ TabState pageId (tabSignalBoxId tab)) 
                      $ F.toList tabSeq
         selectIndex <-
             case windowListGetWindow windowId windowList of
               Nothing -> do
                  putStrLn $ "saveLayoutState(): Impossible that can't found window " 
                               ++ show windowId 
                               ++ " to save state."
                  return 0
               Just w -> notebookGetCurrentPage (windowNotebook w)
         return ((windowId, pageModeName), tabbarStateSeq, selectIndex)

  -- Get buffer list state.
  tPages <- readTVarIO tabPages
  let (foregroundPages, backgroundPages) =
        unzip $ map (\ (pageModeName, bufferSeq) -> 
                         let bufferList = map bufferPageId $ F.toList bufferSeq
                             (fList, bList) = partition (`Set.member` tPages) bufferList
                         in ((pageModeName, fList), (pageModeName, bList))
                    ) $ M.toList bufferList

  -- Save layout state.
  writeConfig layoutPath 
              (Just 
               (EnvironmentState 
                (WindowNodeStateList windowNodeStateCounter windowNodeStateList) 
                (WindowStateList leftList rightList)
                (TabbarState tStateMap pageIdCounter signalBoxIdCounter)
                (BufferListState foregroundPages backgroundPages)
               ))

-- | Restore layout.
restoreLayoutState :: Environment -> EnvironmentState -> IO ()
restoreLayoutState env 
                   EnvironmentState 
                   {envStateWindowNodeList = 
                        WindowNodeStateList {wnslCounter = nodeCounter
                                            ,wnslSet     = nodeSet}
                   ,envStateWindowList =
                       WindowStateList {wslLeft  = winLeftList
                                       ,wslRight = winRightList}
                   ,envStateTabbar = 
                       TabbarState {tabbarStateList               = tStates
                                   ,tabbarStatePageIdCounter      = pageIdCounter
                                   ,tabbarStateSignalBoxIdCounter = signalBoxIdCounter}
                   ,envStateBufferList =
                       BufferListState {bufferListStateForegroundPages = fPages
                                       ,bufferListStateBackgroundPages = bPages}
                   } = do
  -- Init.
  (tabbarTVar, (bufferListTVar, (signalBoxList, (focusNotifierListTVar, (windowNodeListTVar, windowListTVar))))) <- envGet env
                     
  -- Restore window node.
  modifyTVarIO windowNodeListTVar (`setListSetCounter` nodeCounter)

  let nodeStateList = Set.toList nodeSet
  if null nodeStateList
     then putStrLn "Got empty window node list in restoreLayoutState(), it shouldn't happend."
     else do
       let rootNode = head nodeStateList
           restoreWindowNodeState 
             WindowNodeState {windowNodeStateId           = nId
                             ,windowNodeStateParentId     = pId
                             ,windowNodeStateChildLeftId  = clId
                             ,windowNodeStateChildRightId = crId
                             ,windowNodeStateType         = nType
                             ,windowNodeStateDirection    = nDirection} = do
               -- Init.
               (windowNodeList, container) <- envGet env

               -- Get new window node.
               (newNode, newNodeList) <- 
                   windowNodeNewInternal (Just nId, pId, clId, crId, nType, nDirection) 
                                         (windowNodeList, container)

               -- Show window node.
               widgetShowAll (windowNodePaned newNode)

               -- Update window node list.
               writeTVarIO windowNodeListTVar newNodeList
               
               -- Restore child node.
               let restoreChild childId =
                       childId ?>= \ matchId ->
                           maybeFindMin nodeSet (\x -> windowNodeStateId x == matchId)
                               ?>= restoreWindowNodeState
               
               restoreChild clId
               restoreChild crId

       -- Restore all window node.
       restoreWindowNodeState rootNode

  -- Restore window state.
  windowNodeList <- readTVarIO windowNodeListTVar
  let getWindows winList = 
          forM winList $ \ WindowState {windowStateId      = wId
                                       ,windowStateSize    = (width, height)} -> 
            case windowNodeListGetNode windowNodeList wId of
              Just wNode -> do
                -- Restore window size.
                window <- windowNewWithNode wNode focusNotifierListTVar 
                widgetSetSizeRequest (windowNodePaned $ windowNode window) width height
                return [window]
              Nothing -> do
                putStrLn $ "Create window " ++ show wId ++ " failed."
                return []

  leftWindows  <- fmap concat (getWindows winLeftList)
  rightWindows <- fmap concat (getWindows winRightList)

  -- Update window list.
  writeTVarIO windowListTVar (ListZipper leftWindows rightWindows)

  -- Restore tabbar state.
  writeTVarIO (envPageIdCounter env) pageIdCounter           -- restore page id counter
  writeTVarIO (envSignalBoxIdCounter env) signalBoxIdCounter -- restore signal box id counter

  windowList <- readTVarIO windowListTVar
  forM_ tStates $ \ ((windowId, modeName), tabStateList, tabSelectIndex) -> 
    case windowListGetWindow windowId windowList of
      Nothing -> putStrLn $ "restoreLayoutState(), can't restore tabs in window " ++ show windowId
      Just window -> do
        let notebook = windowNotebook window
        forM_ tabStateList $ \ TabState {tabStatePageId      = pageId
                                        ,tabStateSignalBoxId = signalBoxId} -> do
            -- Init.
            uiFrame <- uiFrameStick notebook

            -- New signal box list.
            let signalBox = SignalBox signalBoxId uiFrame windowId
            modifyTVarIO signalBoxList (Set.insert signalBox)
            
            -- Update tabbar.
            modifyTVarIO tabbarTVar (tabbarAddTab windowId modeName (Tab 0 pageId signalBoxId 0 0 uiFrame))

        -- Restore focus tab.
        notebookSetCurrentPage notebook tabSelectIndex

  -- Startup sub-process.
  let startupRenderProcess pages =
          forM_ pages $ \ (modeName, pageIds) -> 
              forM_ pageIds $ \ pageId -> do
                -- Get startup path.
                configDir <- getConfigDirectory
                startupPath <- expandFilePath $ configDir </> statePath </> show pageId </> subprocessStartupPath

                isExist <- doesFileExist startupPath
                if isExist
                   then do
                     content <- readFile startupPath
                     case (read content :: (String, SpawnProcessArgs)) of
                       (binaryPath, startupArgs) -> do
                            -- Update buffer list.
                            modifyTVarIO bufferListTVar (bufferListAddBuffer (modeName, 0, pageId, "", ""))
          
                            -- Startup sub process.
                            runProcess_ binaryPath [show startupArgs]
                   else
                       putStrLn $ "Can't found startup file " ++ startupPath ++ " restore sub-process failed."

  startupRenderProcess fPages   -- restore foreground sub-process
  startupRenderProcess bPages   -- restore background sub-process

  -- Focus current window.
  focusWindow env

  return ()                           

instance ActionInputArgs Frame where
    envGet = return . envFrame
instance ActionInputArgs Client where
    envGet = return . envDaemonClient
instance ActionInputArgs Environment where
    envGet = return
instance ActionInputArgs Window where
    envGet = getFocusWindow
instance ActionInputArgs Container where
    envGet = return . getToplevelContainer 
instance ActionInputArgs WindowList where
    envGet = readTVarIO . envWindowList
instance ActionInputArgs WindowNodeList where
    envGet = readTVarIO . envWindowNodeList
instance ActionInputArgs FocusNotifierList where
    envGet = readTVarIO . envFocusNotifierList
instance ActionInputArgs Tabbar where
    envGet = readTVarIO . envTabbar
instance ActionInputArgs TabbarSelect where
    envGet = readTVarIO . envTabbarSelect
instance ActionInputArgs BufferList where
    envGet = readTVarIO . envBufferList
instance ActionInputArgs (TVar WindowList) where
    envGet = return . envWindowList
instance ActionInputArgs (TVar WindowNodeList) where
    envGet = return . envWindowNodeList
instance ActionInputArgs (TVar FocusNotifierList) where
    envGet = return . envFocusNotifierList
instance ActionInputArgs (TVar SignalBoxList) where
    envGet = return . envSignalBoxList
instance ActionInputArgs (TVar Tabbar) where
    envGet = return . envTabbar
instance ActionInputArgs (TVar TabbarSelect) where
    envGet = return . envTabbarSelect
instance ActionInputArgs (TVar BufferList) where
    envGet = return . envBufferList
instance ActionInputArgs (TVar TabCloseHistory) where
    envGet = return . envTabCloseHistory
instance ActionOutputArgs WindowList where
    envPut = writeTVarIO . envWindowList
instance ActionOutputArgs WindowNodeList where
    envPut = writeTVarIO . envWindowNodeList
instance ActionOutputArgs Tabbar where 
    envPut = writeTVarIO . envTabbar
instance ActionOutputArgs TabbarSelect where 
    envPut = writeTVarIO . envTabbarSelect
instance ActionOutputArgs BufferList where 
    envPut = writeTVarIO . envBufferList
