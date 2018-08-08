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
module Manatee.Core.Render where

import Control.Monad
import Control.Monad.State
import DBus.Client hiding (Signal)
import Data.Text.Lazy (unpack)
import GHC.Conc
import Graphics.UI.Gtk hiding (Window, windowNew, Frame, frameNew, Signal, Frame, Variant, Action, plugNew, plugGetId, get)
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.Debug
import Manatee.Core.Dynload
import Manatee.Core.Page
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.FilePath
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.Set hiding (mapM)
import Manatee.Toolkit.General.State
import Manatee.Toolkit.Gtk.Gtk
import Manatee.Toolkit.Widget.Plug
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Posix.Process
import System.Posix.Types (ProcessID)

import qualified Data.Map as M
import qualified Data.Set as Set

-- | Irc client render process.
startupRender :: PageBufferNewFun -> CustomizeNewFun -> IO ()
startupRender bufferNewFun customizeNewFun = do
  -- Get program arguments.
  args <- getArgs

  case args of
    [x] -> 
        case (read x :: SpawnProcessArgs) of
          arg@(SpawnRenderProcessArgs {}) -> renderMain arg bufferNewFun customizeNewFun
    _ -> return ()
    
-- | Render process main entry.
renderMain :: SpawnProcessArgs -> PageBufferNewFun -> CustomizeNewFun -> IO ()
renderMain spawnProcessArgs@(SpawnRenderProcessArgs pageId pType pagePath options sIds isRestore) 
           bufferNewFun customizeNewFun = do
  -- Init.
  unsafeInitGUIForThreadedRTS

  -- Create page list.
  pageList <- newTVarIO Set.empty 

  -- Get render process id.
  processId <- getProcessID

  -- Create client.
  let clientName = mkRenderClientName processId
  client <- mkSessionClientWithName clientName

  -- Load user's configure file.
  customizeWrap <- customizeNewFun

  -- Create page buffer.
  bufferWrap <- bufferNewFun pagePath options client pageId customizeWrap
  when isRestore (renderReadBufferState bufferWrap pageId)

  -- Build render client for listen dbus signal.
  pongStatusTVar <- newTVarIO GotPongMessage
  mkRenderClient client bufferWrap customizeWrap processId pageList pType pongStatusTVar spawnProcessArgs

  -- Send NewRenderProcessConfirm signal or create new page.
  if null sIds
     -- Send NewRenderProcessConfirm signal if haven't any page need create.
     then 
       case bufferWrap of
         (PageBufferWrap buffer) -> do
              let modeName = pageModeName $ pageBufferMode buffer
              path <- pageBufferGetName buffer
              mkDaemonSignal client 
                             NewRenderProcessConfirm
                             (NewRenderProcessConfirmArgs pageId pType processId modeName path)
     -- Otherwise, create new page.
     else
         forM_ (pairPred sIds) $ \ (sId, index) -> 
           renderPageNew client bufferWrap pageList processId (pageId, pType, sId) (index == 0) isRestore

  -- Send ping signal.
  renderSendPing client pongStatusTVar

  -- Loop.
  mainGUI

-- | Build render client for listen dbus signal.
mkRenderClient :: Client 
               -> PageBufferWrap 
               -> CustomizeWrap 
               -> ProcessID 
               -> TVar PageList 
               -> PageType 
               -> TVar PongStatus
               -> SpawnProcessArgs
               -> IO ()
mkRenderClient client bufferWrap customizeWrap processId pageList pType pongStatusTVar spawnProcessArgs = 
  -- Build match rule.
  mkRenderMatchRules client 
    [(CloneRenderPage,          renderHandleClonePage client bufferWrap pageList processId pType)
    ,(ReparentRenderPage,       renderHandleReparentPage client bufferWrap pageList processId)
    ,(FocusRenderPage,          renderHandleFocusPage pageList)
    ,(PageViewKeyPress,         renderHandleKeyPress pageList)
    ,(DestroyRenderPage,        renderHandleDestroyPage True pageList)
    ,(RestoreRenderPageState,   renderHandleRestorePageState pageList)
    ,(ExitRenderProcess,        renderHandleExitProcess client processId pageList bufferWrap spawnProcessArgs)
    ,(UpdateConfig,             renderHandleUpdateConfig customizeWrap)
    ,(InstallConfig,            renderHandleInstallConfig bufferWrap)
    ,(Pong,                     renderHandlePong pongStatusTVar)
    ]

-- | Handle clone render page signal.
renderHandleClonePage :: Client -> PageBufferWrap -> TVar PageList -> ProcessID -> PageType -> RenderSignalArgs -> IO ()
renderHandleClonePage client bufferWrap pageList processId pType (CloneRenderPageArgs pageId sId) = do
  debugDBusMessage $ "renderHandleClonePage: Catch CloneRenderPage signal. Box id: " ++ show sId

  renderPageNew client bufferWrap pageList processId (pageId, pType, sId) False False
  
  debugDBusMessage $ "renderHandleClonePage: Finish clone page. Box id : " ++ show sId

-- | Handle re-parent render page signal.
renderHandleReparentPage :: Client -> PageBufferWrap -> TVar PageList -> ProcessID -> RenderSignalArgs -> IO ()
renderHandleReparentPage client (PageBufferWrap buffer) pageList processId (ReparentRenderPageArgs pageId oldPlugId sId) = do
  debugDBusMessage $ "renderHandleReparentPage: Catch ReparentRenderPage signal. Box id: " ++ show sId

  -- Get old plug.
  pList <- readTVarIO pageList
  let oldPage = maybeFindMin pList (\x -> pagePlugId x == oldPlugId)

  -- Try to re-parent children from old plug.
  case oldPage of
    Just op@(Page {pageView = PageViewWrap oldView}) -> do
      -- Save state first.
      pageViewSaveState oldView
      
      -- Create new plug.
      newPlug <- plugNew Nothing
      let pId = plugId newPlug
          pType = pageType op 

      -- Reparent.
      writeTVarIO (pageApplyViewWrap op pageViewPlugId) pId -- update new page plug id
      widgetReparent (pageBox op) (plugBody newPlug)
      widgetShowAll $ plugBody newPlug

      -- New page.
      page <- pageNew pageId sId pType newPlug (pageView op)

      -- Add page to page list.
      runTVarStateT pageList $ put . Set.insert page

      -- Destroy old plug.
      renderHandleDestroyPage False pageList (DestroyRenderPageArgs oldPlugId)
  
      -- Send `NewRenderPageConfirm` signal.
      let modeName = pageModeName $ pageBufferMode buffer 
      path <- pageBufferGetName buffer
      mkDaemonSignal client 
                     NewRenderPageConfirm 
                     (NewRenderPageConfirmArgs pageId pType sId pId processId modeName path False False)

      debugDBusMessage $ "renderHandleReparentPage: Finish reparent page. Box id : " ++ show sId
    Nothing -> debugDBusMessage $ "Warning (renderHandleReparentPage): Cant find old plug " ++ show oldPlugId ++ " to reparent."

-- | Handle new render page signal.
renderPageNew :: Client -> PageBufferWrap -> TVar PageList -> ProcessID -> (PageId, PageType, SignalBoxId) 
              -> Bool -> Bool -> IO ()
renderPageNew client (PageBufferWrap buffer) pageList processId (pageId, pType, sId) isFirstPage isRestore = do
   -- Create page plug.
  plug <- plugNew Nothing
  let pId = plugId plug
  
  -- Create page.
  viewWrap <- pageBufferCreateView buffer pId
  page <- pageNew pageId sId pType plug viewWrap
  
  -- Create page and pageView.
  plugBody plug `containerAdd` pageBox page
  widgetShowAll $ plugBody plug
  
  -- Add page to page list.
  runTVarStateT pageList $ put .  Set.insert page
  
  -- Send `NewRenderPageConfirm` signal.
  let modeName = pageModeName $ pageBufferMode buffer 
  path <- pageBufferGetName buffer
  mkDaemonSignal client 
                 NewRenderPageConfirm 
                 (NewRenderPageConfirmArgs pageId pType sId pId processId modeName path isFirstPage isRestore)

-- | Handle focus render page signal.
renderHandleFocusPage :: TVar PageList -> RenderSignalArgs -> IO ()
renderHandleFocusPage pageList (FocusRenderPageArgs plugId) = do
  pl <- readTVarIO pageList
  maybeFindMin pl (\x -> pagePlugId x == plugId) 
     ?>= \ (Page {pageView = PageViewWrap view}) -> 
         ifM (pageViewIsFocusInteractivebar view)
             (pageViewFocusInteractivebar view)
             (pageViewFocus view) 

-- | Handle page view key press signal.
renderHandleKeyPress :: TVar PageList -> RenderSignalArgs -> IO ()
renderHandleKeyPress pageList (PageViewKeyPressArgs plugId keystoke sEvent) = do
  pl <- readTVarIO pageList
  maybeFindMin pl (\x -> pagePlugId x == plugId) 
     ?>= \ (Page {pageView = PageViewWrap view}) -> do
    isFocusKeymapWindow <- pageViewIsFocusKeymapWindow view
    isFocusInteractivebar <- pageViewIsFocusInteractivebar view
    if isFocusKeymapWindow
       then 
           M.lookup keystoke helperKeymap
                ?>= \ command -> 
                    M.lookup command helperCommandMap
                         ?>= \ action -> action view
       else if isFocusInteractivebar 
           -- Handle interactive keymap if focus on interactivebar.
           then 
               case M.lookup keystoke interactiveKeymap of
                 Just command ->
                     M.lookup command interactiveCommandMap 
                          ?>= \ action -> action view 
                 Nothing -> widgetPropagateEvent (pageViewEntry view) sEvent
           -- Otherwise handle page view keymap.
           else 
               case M.lookup keystoke pageViewKeymap of
                 Just command ->
                     M.lookup command pageViewCommandMap 
                          ?>= \ action -> action view
                 Nothing -> 
                   case M.lookup keystoke (pageViewLocalKeymap view) of
                     Just command -> 
                       case M.lookup command (pageViewLocalCommandMap view) of
                         Just action -> action view
                         Nothing -> do
                             pageViewShowOutputbar view ("Can't find command : " ++ unpack command ++ " propagate event") Nothing
                             widgetPropagateEvent (pageViewPropagateWidget view) sEvent
                     Nothing -> widgetPropagateEvent (pageViewPropagateWidget view) sEvent

-- | Handle destroy render page signal.
renderHandleDestroyPage :: Bool -> TVar PageList -> RenderSignalArgs -> IO ()
renderHandleDestroyPage saveState pageList (DestroyRenderPageArgs plugId) = 
    runTVarStateT pageList $ \pl -> 
      maybeFindMin pl (\x -> pagePlugId x == plugId) 
          ?>= \page@(Page {pageView = PageViewWrap view}) -> do
            lift $ debugDBusMessage $ "renderHandleDestroyPage: Catch DestroyRenderPage signal. Plug id : " ++ show plugId
            -- Save state first.
            when saveState $
                 lift $ pageViewSaveState view
            -- Delete page from page list.
            put (Set.delete page pl)                              
            -- Destroy page.
            lift $ plugDestroy $ pagePlug page

-- | Restore page state.
renderHandleRestorePageState :: TVar PageList -> RenderSignalArgs -> IO ()
renderHandleRestorePageState pageList (RestoreRenderPageStateArgs plugId isRestore) = do
  pl <- readTVarIO pageList
  maybeFindMin pl (\x -> pagePlugId x == plugId) 
     ?>= \ (Page {pageView        = PageViewWrap view
                 ,pageId          = pId
                 ,pageSignalBoxId = sId}) -> do
       if isRestore
          -- Restore from state file.
          then do 
            statePath <- renderGetViewStatePath pId sId
            isExist <- doesFileExist statePath
            if isExist 
               then 
                   pageViewReadState view statePath
               else do
                 pageViewRestoreState view
                 putStrLn $ "Can't found state path " ++ statePath ++ ", load default state."
          -- Or restore from last save state.
          else
              pageViewRestoreState view
                  
-- | Handle exit process signal.
renderHandleExitProcess :: Client -> ProcessID -> TVar PageList -> PageBufferWrap -> SpawnProcessArgs -> RenderSignalArgs -> IO ()
renderHandleExitProcess client 
                        processId 
                        pageList 
                        (PageBufferWrap buffer)
                        (SpawnRenderProcessArgs pId pType _ _ _ _)
                        (ExitRenderProcessArgs pageId saveState) = do
  -- Save state.
  when saveState $ do
       -- Get current buffer path.
       path <- pageBufferGetName buffer

       -- Get SignalBoxId from page list.
       pList <- fmap (Set.toList) $ readTVarIO pageList
       let sIds = map pageSignalBoxId pList

       -- Save startup state.
       progName <- getProgName
       savePath <- renderGetBufferStartupPath pageId
       createDirectoryIfMissing True (takeDirectory savePath)
       withFile savePath WriteMode $ \x -> 
         hPutStr x (show (progName, (SpawnRenderProcessArgs pId pType path [] sIds True)))

       -- Save buffer state.
       bStatePath <- renderGetBufferStatePath pageId
       pageBufferWriteState buffer bStatePath

       -- Save view state.
       forM_ pList $ \ (Page {pageView        = PageViewWrap view
                             ,pageId          = pId
                             ,pageSignalBoxId = sId}) -> do
            statePath <- renderGetViewStatePath pId sId
            pageViewWriteState view statePath

  -- Send RenderProcessExitConfirm signal to daemon process.
  mkDaemonSignal client RenderProcessExitConfirm (RenderProcessExitConfirmArgs pageId processId)

  -- Quit process.
  mainQuit

-- | Handle update config signal.
renderHandleUpdateConfig :: CustomizeWrap -> RenderSignalArgs -> IO ()
renderHandleUpdateConfig customizeWrap _ = 
  loadConfig customizeWrap True

-- | Handle install config signal.
renderHandleInstallConfig :: PageBufferWrap -> RenderSignalArgs -> IO ()
renderHandleInstallConfig (PageBufferWrap buffer) _ = do
  packageName <- pageBufferPackageName buffer
  runCommand_ ("cabal install " ++ packageName ++ " --reinstall")

-- | Handle pong signal.
renderHandlePong :: TVar PongStatus -> RenderSignalArgs -> IO ()
renderHandlePong pongStatusTVar _ = 
  writeTVarIO pongStatusTVar GotPongMessage
  
-- | Send ping signal.  
-- Render process will send PING message in delay time.
-- Quit current process if not receive PONG message in next delay time.
-- Use for test daemon process whether live to avoid immortal process.
renderSendPing :: Client -> TVar PongStatus -> IO ()
renderSendPing client pongStatusTVar = do
  -- Get current process id.
  processId <- getProcessID
  let sendDelay = 5000          -- delay time to send ping message

  -- Send ping message in delay time.
  timeoutAdd (do
               pongStatus <- readTVarIO pongStatusTVar
               case pongStatus of
                 GotPongMessage -> do
                   -- Send ping signal to test whether daemon process still live.
                   mkDaemonSignal client Ping (PingArgs processId)
                   -- Change pong status to WaitPongMessage. 
                   writeTVarIO pongStatusTVar WaitPongMessage
                 WaitPongMessage -> do
                   -- Quit process if lost ping message.
                   putStrLn $ "Lost pong message from daemon process, quit process (" ++ show processId ++ ")"
                   mainQuit
               return True)
              sendDelay
  
  return ()

-- | Get buffer state directory.
renderGetBufferStateDir :: PageId -> IO FilePath  
renderGetBufferStateDir pageId = do
  configDir <- getConfigDirectory
  expandFilePath $ configDir </> statePath </> show pageId

-- | Get buffer startup path.
renderGetBufferStartupPath :: PageId -> IO FilePath
renderGetBufferStartupPath pageId = do
  stateDir <- renderGetBufferStateDir pageId
  return $ stateDir </> subprocessStartupPath
  
-- | Get buffer state path.
renderGetBufferStatePath :: PageId -> IO FilePath  
renderGetBufferStatePath pageId = do
  stateDir <- renderGetBufferStateDir pageId
  return $ stateDir </> bufferStatePath

-- | Get view state path.
renderGetViewStatePath :: PageId -> SignalBoxId -> IO FilePath  
renderGetViewStatePath pageId signalBoxId = do
  stateDir <- renderGetBufferStateDir pageId
  return $ stateDir </> viewStatePath ++ "-" ++ show signalBoxId

-- | Read state.
renderReadBufferState :: PageBufferWrap -> PageId -> IO ()
renderReadBufferState (PageBufferWrap buffer) pageId = do
  -- Get buffer state path.
  path <- renderGetBufferStatePath pageId
  isExist <- doesFileExist path
  when isExist (pageBufferReadState buffer path)
