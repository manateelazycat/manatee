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

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Manatee.Daemon where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.Trans
import DBus.Client hiding (Signal)
import DBus.Types
import Data.Map (Map)
import Data.Text.Lazy (Text)
import GHC.Conc
import Graphics.UI.Gtk hiding (Window, windowNew, Frame, frameNew, 
                               Signal, Variant, Action, layoutPath,
                               plugNew, plugGetId, get, Keymap)
import Graphics.UI.Gtk.Gdk.SerializedEvent
import Manatee.Action.Basic
import Manatee.Action.BufferList
import Manatee.Action.Tab
import Manatee.Action.Tabbar
import Manatee.Action.Window
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.Debug
import Manatee.Core.PageMode
import Manatee.Core.Types
import Manatee.Environment
import Manatee.Toolkit.General.FilePath
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.Set hiding (mapM)
import Manatee.Toolkit.Gio.Gio
import Manatee.Toolkit.Gtk.Event
import Manatee.Toolkit.Gtk.Gtk
import Manatee.Toolkit.Gtk.Struct
import Manatee.Toolkit.Widget.NotebookTab
import Manatee.Types
import Manatee.UI.Frame
import Manatee.UI.Window
import System.Directory

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T

-- | Daemon process main entry.
daemonMain :: Bool -> IO ()
daemonMain restoreState = do
  -- Init.
  unsafeInitGUIForThreadedRTS

  env <- mkEnvironment 
  let frame = envFrame env

  -- Build daemon client for listen dbus signal.
  mkDaemonClient env 

  -- Read extension global keymap. 
  (ExtensionGloalKeymap keymap) <- readConfig extensionGlobalKeymapPath (ExtensionGloalKeymap M.empty)
  let extensionGlobalKeymap = 
          M.fromList $ map (\ (key, (_, (pType, pPath, pOptions))) -> 
                              (T.pack key, Action (newTab pType pPath pOptions))) (M.toList keymap) 
      extensionKeymap =
          map (\ (key, (extensionCommand, _)) -> 
                   (T.pack key, T.pack extensionCommand)) (M.toList keymap) 

  -- Build local object.
  mkDaemonMethods [("GetBufferList",            callGetBufferList env)
                  ,("GetBufferHistory",         callGetBufferHistory env)
                  ,("GetWindowAllocation",      callGetWindowAllocation env)
                  ,("GetGlobalKeymap",          callGetGlobalKeymap env extensionKeymap)
                  ] 

  -- Handle key event.
  frame `on` keyPressEvent $ tryEvent $ do
      -- Focus tab when press key.
      liftIO $ focusWindow env

      -- Get event status.
      keystoke <- eventKeystoke
      sEvent <- serializedEvent

      -- liftIO $ putStrLn $ "Debug keystoke : " ++ show keystoke

      -- Handle key press event.
      liftIO $ do
        case M.lookup keystoke extensionGlobalKeymap of
          Just action -> runAction env action
          _ -> 
            case M.lookup keystoke globalKeymap of
              Just key -> 
                  case M.lookup key globalCommandMap of
                    Just action -> runAction env action
                    _ -> envGet env >>= handlePageViewKeyPress keystoke sEvent
              _ -> envGet env >>= handlePageViewKeyPress keystoke sEvent
        -- Focus tab after handle event.
        focusWindow env

  -- Show.
  widgetShowAll frame
  frame `onDestroy` exit env True

  -- Restore layout state.
  if restoreState
     then  do
         state <- readConfig layoutPath Nothing
         case state of
           -- Show welcome view if haven't any state.
           Nothing -> newTab "PageWelcome" "Welcome" [] env
           -- Otherwise restore layout state.
           Just state -> restoreLayoutState env state
     else
         -- Show welcome view if don't need restore state.
         newTab "PageWelcome" "Welcome" [] env

  -- Loop.
  mainGUI

-- | Global keymap.
globalKeymap :: Map Text Text
globalKeymap =
  M.fromList
   [("M-t",      "Split window vertically")
   ,("M-T",      "Split window hortizontally")
   ,("M-n",      "Select next window")
   ,("M-p",      "Select previous window")
   ,("M-;",      "Close current window")
   ,("M-:",      "Window other windows")
   -- Window zoom keymap.
   ,("P-.",      "Window enlarge")
   ,("P-,",      "Window shrink")
   ,("P-j",      "Window enlarge down")
   ,("P-k",      "Window enlarge up")
   ,("P-h",      "Window enlarge left")
   ,("P-l",      "Window enlarge right")
   ,("P-J",      "Window shrink down")
   ,("P-K",      "Window shrink up")
   ,("P-H",      "Window shrink left")
   ,("P-L",      "Window shrink right")
   -- Tab keymap in current window.
   ,("M-9",      "Select previous tab group")
   ,("M-0",      "Select next tab group")
   ,("M-7",      "Select previous tab")
   ,("M-8",      "Select next tab")
   ,("M-&",      "Select first tab")
   ,("M-*",      "Select last tab")
   ,("C-7",      "Move tab to left")
   ,("C-8",      "Move tab to right")
   ,("C-&",      "Move tab to begin")
   ,("C-*",      "Move tab to end")
   ,("M-'",      "Close current tab")
   ,("M-\"",     "Close other tabs")
   -- Tab keymap with next window.
   ,("P-7",      "Select previous tab with next window")
   ,("P-8",      "Select next tab with next window")
   ,("P-&",      "Select first tab with next window")
   ,("P-*",      "Select last tab with next window")
   ,("P-9",      "Select previous tab group with next window")
   ,("P-0",      "Select next tab group with next window")
   ,("C-P-7",    "Move tab to left with next window")
   ,("C-P-8",    "Move tab to right with next window")
   ,("C-P-&",    "Move tab to begin with next window")
   ,("C-P-*",    "Move tab to end with next window")
   -- Other keymap.
   ,("M-F",      "Focus current tab")
   ,("M-[",      "View directory of buffer")
   ,("C-u",      "Update configuration")
   ,("C-i",      "Install configuration")
   ,("F11",      "Toggle fullscreen")
   ,("F12",      "Lock screen")
   ,("C-'",      "Undo tab close (global)")
   ,("C-\"",     "Undo tab close (local)")
   ,("M-A",      "Toggle play")
   ]

-- | Global keymap.
globalCommandMap :: Keymap
globalCommandMap =
  M.fromList
   ["Split window vertically"           ==> windowSplitVertically
   ,"Split window hortizontally"        ==> windowSplitHortizontally
   ,"Select next window"                ==> windowSelectNext
   ,"Select previous window"            ==> windowSelectPrev
   ,"Close current window"              ==> windowCloseCurrent  
   ,"Window other windows"              ==> windowCloseOthers
   -- Window zoom keymap.
   ,"Window enlarge"                    ==> windowEnlarge
   ,"Window shrink"                     ==> windowShrink
   ,"Window enlarge down"               ==> windowEnlargeDown
   ,"Window enlarge up"                 ==> windowEnlargeUp
   ,"Window enlarge left"               ==> windowEnlargeLeft
   ,"Window enlarge right"              ==> windowEnlargeRight
   ,"Window shrink down"                ==> windowShrinkDown
   ,"Window shrink up"                  ==> windowShrinkUp
   ,"Window shrink left"                ==> windowShrinkLeft
   ,"Window shrink right"               ==> windowShrinkRight
   -- Tab keymap in current window.
   ,"Select previous tab group"         ==> tabForwardGroup
   ,"Select next tab group"             ==> tabBackwardGroup
   ,"Select previous tab"               ==> tabSelectPrev
   ,"Select next tab"                   ==> tabSelectNext
   ,"Select first tab"                  ==> tabSelectFirst
   ,"Select last tab"                   ==> tabSelectLast
   ,"Move tab to left"                  ==> tabMoveToLeft
   ,"Move tab to right"                 ==> tabMoveToRight
   ,"Move tab to begin"                 ==> tabMoveToBegin
   ,"Move tab to end"                   ==> tabMoveToEnd
   ,"Close current tab"                 ==> tabCloseCurrent   
   ,"Close other tabs"                  ==> tabCloseOthers
   -- Tab keymap with next window.
   ,"Select previous tab with next window"        ==> tabSelectPrevWithNextWindow
   ,"Select next tab with next window"            ==> tabSelectNextWithNextWindow
   ,"Select first tab with next window"           ==> tabSelectFirstWithNextWindow
   ,"Select last tab with next window"            ==> tabSelectLastWithNextWindow
   ,"Select previous tab group with next window"  ==> tabForwardGroupWithNextWindow
   ,"Select next tab group with next window"      ==> tabBackwardGroupWithNextWindow
   ,"Move tab to left with next window"           ==> tabMoveToLeftWithNextWindow
   ,"Move tab to right with next window"          ==> tabMoveToRightWithNextWindow
   ,"Move tab to begin with next window"          ==> tabMoveToBeginWithNextWindow
   ,"Move tab to end with next window"            ==> tabMoveToEndWithNextWindow
   -- Other keymap.
   ,"Focus current tab"         ==> focusCurrentTab
   ,"View directory of buffer"  ==> viewBufferDirectory
   ,"Update configuration"      ==> updateConfigure
   ,"Install configuration"     ==> installConfigure
   ,"Toggle fullscreen"         ==> toggleFullscreen
   ,"Lock screen"               ==> lockScreen
   ,"Undo tab close (global)"   ==> tabUndoCloseGlobal
   ,"Undo tab close (local)"    ==> tabUndoCloseLocal
   ,"Toggle play"               ==> togglePlay
   ]

-- | Build daemon client for listen dbus signal.
mkDaemonClient :: Environment -> IO ()
mkDaemonClient env = do
  let client = envDaemonClient env

  mkDaemonMatchRules client 
    [(NewRenderPageConfirm,      daemonHandleNewPageConfirm env)
    ,(NewRenderProcessConfirm,   daemonHandleNewProcessConfirm env)
    ,(RenderProcessExit,         daemonHandleRenderProcessExit env)
    ,(RenderProcessExitConfirm,  daemonHandleRenderProcessExitConfirm)
    ,(NewTab,                    daemonHandleNewTab env)
    ,(SynchronizationPathName,   daemonHandleSynchronizationPathName env)
    ,(ChangeTabName,             daemonHandleChangeTabName env)
    ,(SwitchBuffer,              daemonHandleSwitchBuffer env)
    ,(Ping,                      daemonHandlePing env)
    ]

-- | Handle render process exit signal.
daemonHandleRenderProcessExit :: Environment -> DaemonSignalArgs -> IO ()
daemonHandleRenderProcessExit env (RenderProcessExitArgs pageId) =
  tabClose env pageId

-- | Handle render process exit confirm signal.
daemonHandleRenderProcessExitConfirm :: DaemonSignalArgs -> IO ()
daemonHandleRenderProcessExitConfirm (RenderProcessExitConfirmArgs pageId processId) = 
  debugDBusMessage $ "daemonHandleRenderProcessExitConfirm: child process " 
                   ++ show processId 
                   ++ " exit. With page id : " 
                   ++ show pageId

-- | Handle new tab signal.
daemonHandleNewTab :: Environment -> DaemonSignalArgs -> IO ()  
daemonHandleNewTab env (NewTabArgs pageType pagePath options) = 
  runAction env (Action (newTab pageType pagePath options))

-- | Handle synchronization tab name.
daemonHandleSynchronizationPathName :: Environment -> DaemonSignalArgs -> IO ()  
daemonHandleSynchronizationPathName env (SynchronizationPathNameArgs modeName pageId path) = do
  debugDBusMessage $ "daemonHandleSynchronizationPathName: Catch SynchronizationPathName signal. Page id : " ++ show pageId

  (bufferListTVar, Tabbar tabbar) <- envGet env

  -- Replace path name.
  modifyTVarIO bufferListTVar (bufferListReplacePath modeName pageId path)

  -- Adjust tab name.
  pageModeDuplicateTabList <- getDuplicateTabList
  modifyTVarIO bufferListTVar
                   (if modeName `elem` pageModeDuplicateTabList
                    -- Just strip tab name when current page mode in 'pageModeDuplicateTabList'
                    then bufferListStripName modeName pageId path
                    -- Otherwise unique all tab names.
                    else bufferListUniqueName modeName)

  -- Update notebook name.
  forM_ (M.toList tabbar) $ \ ((windowId, pageModeName), _) -> 
      when (pageModeName == modeName) $ syncTabName env windowId

-- | Handle synchronization tab name.
daemonHandleChangeTabName :: Environment -> DaemonSignalArgs -> IO ()  
daemonHandleChangeTabName env (ChangeTabNameArgs modeName pageId path) = do
  debugDBusMessage $ "daemonHandleChangeTabName: Catch SynchronizationPathName signal. Page id : " ++ show pageId

  (bufferListTVar, Tabbar tabbar) <- envGet env

  -- Replace tab name.
  modifyTVarIO bufferListTVar (bufferListReplaceName modeName pageId path)

  -- Update notebook name.
  forM_ (M.toList tabbar) $ \ ((windowId, pageModeName), _) -> 
      when (pageModeName == modeName) $ syncTabName env windowId

-- | Handle switch buffer.
daemonHandleSwitchBuffer :: Environment -> DaemonSignalArgs -> IO ()
daemonHandleSwitchBuffer env (SwitchBufferArgs modeName pageId) = do
  -- Switch to buffer if it has exist.
  bufferList <- envGet env
  bufferListGetBufferIndexWithId bufferList modeName pageId
    ?>= \ i -> do
      tabSwitchGroupCurrentWindow env modeName
      window <- envGet env
      notebookSetCurrentPage (windowNotebook window) i

-- | Handle ping message.
daemonHandlePing :: Environment -> DaemonSignalArgs -> IO ()       
daemonHandlePing env (PingArgs processId) = 
  mkRenderSignal (envDaemonClient env) processId Pong PongArgs

-- | Handle new page confirm signal.
daemonHandleNewPageConfirm :: Environment -> DaemonSignalArgs -> IO ()
daemonHandleNewPageConfirm env 
                           args@(NewRenderPageConfirmArgs 
                                 pageId pType sId plugId processId modeName path isFirstPage isRestore) = do
  (tabbarTVar, (bufferListTVar, (signalBoxList, windowList))) <- envGet env

  -- Get signal box.
  sbList <- readTVarIO signalBoxList
  case maybeFindMin sbList (\x -> signalBoxId x == sId) of
    Nothing -> putStrLn $ "### Impossible: daemonHandleNewPageConfirm - Can't find signal box Id " ++ show sId
    Just signalBox -> do
          -- Get window id that socket add.
          let windowId = signalBoxWindowId signalBox 
          
          -- Add page.
          debugDBusMessage $ "daemonHandleNewPageConfirm: Catch NewRenderPageConfirm signal. Box id : " ++ show sId
          debugDBusMessage "------------------------------"
          
          -- Add plug to socket.
          let uiFrame = signalBoxUIFrame signalBox
              notebookTab = uiFrameNotebookTab uiFrame
          socketId <- socketFrameAdd uiFrame plugId

          -- Stop spinner animation.
          notebookTabStop notebookTab

          -- Close current page when click close button.
          ntCloseButton notebookTab `onToolButtonClicked` do 
            -- Focus window container close button first,                                                           
            -- otherwise `tabClose` can't work.
            modifyTVarIO windowList (`windowListFocusId` windowId)
            tabClose env pageId
          
          -- Update buffer list when first page create.
          when isFirstPage $ do
            -- Add new buffer.
            modifyTVarIO bufferListTVar (bufferListAddBuffer (modeName, processId, pageId, pType, path))

            -- Adjust tab name.
            pageModeDuplicateTabList <- getDuplicateTabList
            modifyTVarIO bufferListTVar 
                             (if modeName `elem` pageModeDuplicateTabList
                              -- Just strip tab name when current page mode in 'pageModeDuplicateTabList'
                              then bufferListStripName modeName pageId path
                              -- Otherwise unique all tab names.
                              else bufferListUniqueName modeName)

            -- Update buffer history.
            bufferList <- readTVarIO bufferListTVar
            bufferListGetBuffer bufferList modeName pageId 
                ?>= \ Buffer {bufferPageType = pageType
                             ,bufferPath     = path} -> 
                    modifyTVarIO (envBufferHistory env) (insertUnique (BufferHistory modeName pageType path))
          
          -- Update tabbar.
          modifyTVarIO tabbarTVar (tabbarAddTab windowId modeName (Tab processId pageId sId socketId plugId uiFrame))
          
          -- Synchronization tab name.
          syncTabName env windowId
          
          -- Delete corresponding SignalBox from SignalBoxList.
          writeTVarIO signalBoxList (Set.delete signalBox sbList)
          
          -- Synchronization tab in all window when first page create.
          when (not isRestore && isFirstPage) $ tabbarSyncNewTab env windowId args

          -- Restore page state.
          mkRenderSignal (envDaemonClient env) processId RestoreRenderPageState (RestoreRenderPageStateArgs plugId isRestore)

-- | Handle new process confirm signal.
daemonHandleNewProcessConfirm :: Environment -> DaemonSignalArgs -> IO ()          
daemonHandleNewProcessConfirm env 
                              (NewRenderProcessConfirmArgs pageId pType processId modeName path) = do
    (bufferListTVar) <- envGet env

    -- Debug.
    debugDBusMessage $ "daemonHandleNewProcessConfirm: Catch NewRenderProcessConfirm signal. Process id : " ++ show processId
    debugDBusMessage "------------------------------"

    -- Add new buffer.
    modifyTVarIO bufferListTVar (bufferListAddBuffer (modeName, processId, pageId, pType, path))

    -- Adjust tab name.
    pageModeDuplicateTabList <- getDuplicateTabList
    modifyTVarIO bufferListTVar 
                     (if modeName `elem` pageModeDuplicateTabList
                      -- Just strip tab name when current page mode in 'pageModeDuplicateTabList'
                      then bufferListStripName modeName pageId path
                      -- Otherwise unique all tab names.
                      else bufferListUniqueName modeName)

    -- Update buffer history.
    bufferList <- readTVarIO bufferListTVar
    bufferListGetBuffer bufferList modeName pageId 
        ?>= \ Buffer {bufferPageType = pageType
                     ,bufferPath     = path} -> 
            modifyTVarIO (envBufferHistory env) (insertUnique (BufferHistory modeName pageType path))

-- | Add socket to socket frame.
socketFrameAdd :: UIFrame -> PagePlugId -> IO PageSocketId
socketFrameAdd uiFrame (GWindowId plugId) = do
  -- Add plug in UIFrame.
  let socketFrame = uiFrameBox uiFrame
  socket <- socketNew_
  socketFrame `containerAdd` socket
  socketAddId socket plugId
  GWindowId <$> socketGetId socket

-- | Return buffer history.
callGetBufferHistory :: Environment -> Member
callGetBufferHistory env = 
  Method "" "s" $ \ call -> do
    history <- readTVarIO $ envBufferHistory env
    replyReturn call [toVariant history]

-- | Return render window allocation. 
callGetWindowAllocation :: Environment -> Member
callGetWindowAllocation env = 
  Method "s" "s" $ \ call -> do
    -- Read input args.
    let Just input = fromVariant (head $ methodCallBody call)
        pagePlugId = read input :: PagePlugId

    tabbar <- envGet env
    case tabbarGetTab pagePlugId tabbar of
      -- Return render window's coordinate.
      Just tab -> do
        allocation <- widgetGetAllocation (uiFrameBox $ tabUIFrame tab)
        replyReturn call [toVariant allocation]
      -- Or return default value.
      Nothing -> replyReturn call []

-- | Get global keymap.
callGetGlobalKeymap :: Environment -> [(Text, Text)] -> Member
callGetGlobalKeymap env extensionKeymap = 
  Method "s" "s" $ \ call -> do
    -- Read input args.
    let Just input = fromVariant (head $ methodCallBody call)
        pagePlugId = read input :: PagePlugId

    tabbar <- envGet env
    case tabbarGetTab pagePlugId tabbar of
      -- Return render window's coordinate.
      Just tab -> do
        allocation <- widgetGetAllocation (uiFrameBox $ tabUIFrame tab)
        replyReturn call [toVariant (allocation, M.toList globalKeymap ++ extensionKeymap)]
      -- Or return default value.
      Nothing -> replyReturn call []
    
-- | Return buffer list.
callGetBufferList :: Environment -> Member
callGetBufferList env = 
  Method "" "s" $ \ call -> do
     (BufferList bufferList) <- readTVarIO $ envBufferList env
     let list = concatMap (\ (modeName, bufferSeq) -> 
                               map (\ Buffer {bufferPageId   = pageId
                                             ,bufferPath     = path
                                             ,bufferName     = name} -> 
                                    BufferInfo modeName path name pageId) $ F.toList bufferSeq) 
                $ M.toList bufferList
     -- Don't return current buffer.
     currentPageId <- tabGetCurrentPageId env
     let bufferInfos = 
             case currentPageId of
               Just pId -> filter (\ x -> bufferInfoId x /= pId) list 
               Nothing  -> list
     replyReturn call [toVariant bufferInfos]

-- | Reply interactive error.
replyLocalInteractiveError :: MethodCall -> Text -> IO ()
replyLocalInteractiveError call err = 
    replyError call (mkErrorName_ daemonInteractiveErrorName) [toVariant err]

-- | Handle page view key press event.
handlePageViewKeyPress :: Text -> SerializedEvent -> (Environment, Client) -> IO ()
handlePageViewKeyPress keystoke sEvent (env, client) = 
  getCurrentTab env >?>= \ Tab {tabProcessId = processId
                               ,tabPlugId    = plugId} -> 
    mkRenderSignal client processId PageViewKeyPress (PageViewKeyPressArgs plugId keystoke sEvent)

-- | View buffer directory.
-- If buffer path is not directory, view current directory.
viewBufferDirectory :: Environment -> IO ()
viewBufferDirectory env = do
  (bufferList, (tabbar, window)) <- envGet env
  getCurrentTab env 
    >?>= \ tab -> 
        tabbarGetPageModeName tabbar (windowGetId window) 
    ?>= \modeName ->
        bufferListGetBuffer bufferList modeName (tabPageId tab) 
    ?>= \buffer -> do
      let path = getUpperDirectory $ bufferPath buffer
      if not (null path) && directoryDoesExist (UTF8.fromString path)
          then newTab "PageFileManager" path [] env
          else do
            currentDir <- getCurrentDirectory
            newTab "PageFileManager" currentDir [] env

-- | Lock screen.
lockScreen :: Environment -> IO ()
lockScreen _ = do
  -- Don't burn LCD power.
  (runCommand_ "xset dpms force off && sleep 1")
  -- Lock screen.
  (runCommand_ "xtrlock")

-- | Play/Pause mplayer.
togglePlay :: Environment -> IO ()
togglePlay env = 
    mkGenericDaemonSignal (envDaemonClient env) "mplayer" Generic (GenericArgs "Pause" [])

