-- Author:     Andy Stewart <lazycat.manatee@gmail.com>
-- Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
-- 
-- Copyright (C) 2010 Andy Stewart, all rights reserved.
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

{-# LANGUAGE RankNTypes, OverloadedStrings #-}
module Manatee.Plugin.Anything.Main where

import Control.Monad.Trans
import DBus.Client hiding (Signal)
import Data.Text.Lazy (Text)
import GHC.Conc
import Graphics.UI.Gtk hiding (Window, windowNew, Frame, frameNew, Signal, Frame, Variant, Action, get)
import Manatee.Core.DBus
import Manatee.Core.Types 
import Manatee.Plugin.Anything.AnythingView
import Manatee.Plugin.Anything.Types
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Struct
import System.Posix.Process

import qualified Data.Map as M

-- | Anything main.
anythingMain :: AnythingInteractiveType -> [String] -> IO ()
anythingMain iType names = do
  -- Get render process id.
  processId <- getProcessID
  putStrLn $ "Anyting process (" ++ show processId ++ ") startup."

  -- Create Anything cache.
  anythingCache <- newTVarIO M.empty
  
  -- Make client name.
  let clientName = mkRenderClientName processId

  -- Create client.
  client <- mkSessionClientWithName clientName

  -- Create plug.
  plug   <- plugNew Nothing
  plugId <- plugGetId plug

  -- Create AnythingView.
  view <- anythingViewNew iType names
  plug `containerAdd` avNotebook view

  -- Terminate process when plug/socket delete from top-level window.
  plug `on` deleteEvent $ tryEvent $ liftIO $ do
    putStrLn $ "Anything plug disconnect from daemon process, exit process (" ++ show processId ++ ")."
    mainQuit

  -- Show plug.
  widgetShowAll plug

  -- Build anything match rule for listen dbus signal.
  mkAnythingClient client view 

  -- Build match rule to catch key press signal.
  mkRenderMatchRuleUnsafe client (AnythingViewKeyPress, anythingViewHandleKeyPress client view anythingCache)

  -- Send `NewRenderProcessConfirm` signal when create render process.
  mkDaemonSignal client NewAnythingProcessConfirm (NewAnythingProcessConfirmArgs (GWindowId plugId) processId)

  -- Init search when process startup instead show blank page to user, more friendly. :)
  anythingViewCompletion client anythingCache view "" 0

  -- Loop
  mainGUI

-- | Build anything match rule for listen dbus signal.
mkAnythingClient :: Client -> AnythingView -> IO ()
mkAnythingClient client view = 
  -- Build match rule.
  mkRenderMatchRules client
      [(AnythingViewChangeCandidate,            anythingViewHandleChangeCandidate view)
      ,(AnythingViewChangeInteractiveType,      anythingViewHandleChangeInteractiveType view)]

-- | Handle change interactive type.
anythingViewHandleChangeInteractiveType :: AnythingView -> RenderSignalArgs -> IO ()
anythingViewHandleChangeInteractiveType view (AnythingViewChangeInteractiveTypeArgs iType) = 
  writeTVarIO (avInteractiveType view) iType

-- | Handle change interactive candidate.
anythingViewHandleChangeCandidate :: AnythingView -> RenderSignalArgs -> IO ()
anythingViewHandleChangeCandidate view (AnythingViewChangeCandidateArgs names) = do
  candidateTab <- anythingCandidateTabNew names
  writeTVarIO (avCandidateTab view) candidateTab

-- | Handle key press event.
anythingViewHandleKeyPress :: Client -> AnythingView -> TVar AnythingSearchCache -> RenderSignalArgs -> IO ()
anythingViewHandleKeyPress client view anythingCache 
                               (AnythingViewKeyPressArgs 
                                keystoke 
                                allText 
                                unselectText
                                keyPressId
                                isChanged
                               ) = do
  -- Handle event.
  let input = AnythingViewInput allText unselectText
      env   = AnythingViewEnvironment view client anythingCache input keyPressId
      anythingAction keymap = 
       case M.lookup keystoke keymap of
         -- Execute anything action when found match keystroke.
         Just action -> 
             -- Use 'postGUIAsync' protect GTK+ thread won't crash.
             postGUIAsync $ anythingViewRunAction env action
         Nothing -> 
             if isChanged 
                -- Completion when input content changed.
                then anythingViewCompletion client anythingCache view unselectText keyPressId
                -- Otherwise, return same completion as input.
                else postGUIAsync $ do
                  boxHeight <- anythingViewGetBoxHeight view 
                  mkDaemonSignal client AnythingViewOutput (AnythingViewOutputArgs 
                                                            unselectText 
                                                            (drop (length unselectText) allText)
                                                            boxHeight 
                                                            keyPressId)

  focusCandidateView <- anythingViewIsFocusCandidateTab view
  anythingAction $ if focusCandidateView
                      then anythingCandidateViewKeymap
                      else anythingCommandViewKeymap

-- | Run action.
anythingViewRunAction :: AnythingViewEnvironment -> AnythingViewAction -> IO ()
anythingViewRunAction env (AnythingViewAction {anythingViewActionFun = fun}) =
  anythingViewExecute env >>= fun

-- | Keymap for anything candidate view.
anythingCandidateViewKeymap :: AnythingViewKeymap
anythingCandidateViewKeymap = 
    M.fromList
         ["M-j"     <=> anythingViewFocusNextCandidate
         ,"M-k"     <=> anythingViewFocusPrevCandidate
         ,"Down"    <=> anythingViewFocusNextCandidate
         ,"Up"      <=> anythingViewFocusPrevCandidate
         ,"M-J"     <=> anythingViewFocusNextCandidateView
         ,"M-K"     <=> anythingViewFocusPrevCandidateView
         ,"M-m"     <=> anythingViewRunFirstCommand
         ,"Return"  <=> anythingViewRunFirstCommand
         ,"M-/"     <=> anythingViewToggleTab
         ,"M-N"     <=> anythingViewExpandSelectedCandidate
         ]

-- | Keymap for anything command view. 
anythingCommandViewKeymap :: AnythingViewKeymap
anythingCommandViewKeymap = 
    M.fromList
         ["M-j"     <=> anythingViewFocusNextCommand
         ,"M-k"     <=> anythingViewFocusPrevCommand
         ,"Down"    <=> anythingViewFocusNextCommand
         ,"Up"      <=> anythingViewFocusPrevCommand
         ,"M-m"     <=> anythingViewRunSelectedCommand
         ,"Return"  <=> anythingViewRunSelectedCommand
         ,"M-/"     <=> anythingViewToggleTab
         ,"M-N"     <=> anythingViewExpandSelectedCandidate
         ]

-- | Sugar to connect Key and Action.
(<=>) :: forall a . AnythingViewActionArgs a => Text -> (a -> IO ()) -> (Text, AnythingViewAction)
key <=> command = (key, AnythingViewAction command)
