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

{-# LANGUAGE ExistentialQuantification, RankNTypes, DeriveDataTypeable, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Manatee.Core.PageView where

import Control.Concurrent.STM 
import Control.Exception
import Control.Monad
import Data.Map (Map)
import Data.Text.Lazy (Text)
import DBus.Client hiding (Signal)
import DBus.Message 
import DBus.Types
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew)
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.PageFrame
import Manatee.Core.Types
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Editable
import Manatee.Toolkit.Gtk.Gtk
import Manatee.Toolkit.Gtk.ScrolledWindow
import Manatee.Toolkit.Widget.Interactivebar
import Manatee.Toolkit.Widget.CompletionWindow
import Manatee.Toolkit.Widget.KeymapWindow
import Manatee.Toolkit.Widget.Tooltip
import System.Directory
import System.FilePath
import System.Time

import qualified Data.Map as M
import qualified Control.Exception as Exc

-- | Helper keymap.
helperKeymap :: Map Text Text
helperKeymap =
    M.fromList
     [("M-u",        "Scroll step up")
     ,("M-i",        "Scroll step down")
     ,("M-U",        "Scroll page up")
     ,("M-I",        "Scroll page down")
     ,("C-I",        "Scroll to top")
     ,("C-U",        "Scroll to bottom")
     ,("PageDown",   "Scroll step up")
     ,("PageUp",     "Scroll step down")
     ,("Home",       "Scroll page up")
     ,("End",        "Scroll page down")
     ,("C-/",        "Show/Hide local keymap")
     ,("C-?",        "Show global keymap")
     ]

-- | Helper keymap.
helperCommandMap :: PageViewKeymap
helperCommandMap =
    M.fromList
     [("Scroll step up",                scrolledWindowScrollVerticalStep True . pageViewKeymapScrolledWindow)
     ,("Scroll step down",              scrolledWindowScrollVerticalStep False . pageViewKeymapScrolledWindow)
     ,("Scroll page up",                scrolledWindowScrollVerticalPage True . pageViewKeymapScrolledWindow)
     ,("Scroll page down",              scrolledWindowScrollVerticalPage False . pageViewKeymapScrolledWindow)
     ,("Scroll to top",                 scrolledWindowScrollToTop . pageViewKeymapScrolledWindow)
     ,("Scroll to bottom",              scrolledWindowScrollToBottom . pageViewKeymapScrolledWindow)
     ,("Show/Hide local keymap",        pageViewShowLocalKeymap)
     ,("Show global keymap",            pageViewShowGlobalKeymap)
     ]

-- | PageViewKeymap.
pageViewKeymap :: Map Text Text
pageViewKeymap = 
    M.fromList
         [("M-u",        "Scroll step up")
         ,("M-i",        "Scroll step down")
         ,("M-U",        "Scroll page up")
         ,("M-I",        "Scroll page down")
         ,("C-I",        "Scroll to top")
         ,("C-U",        "Scroll to bottom")
         ,("PageDown",   "Scroll page up")
         ,("PageUp",     "Scroll page down")
         ,("Home",       "Scroll to top")
         ,("End",        "Scroll to bottom")
         ,("M-y",        "Scroll step right")
         ,("M-o",        "Scroll step left")
         ,("M-Y",        "Scroll page right")
         ,("M-O",        "Scroll page left")
         ,("C-Y",        "Scroll to left")
         ,("C-O",        "Scroll to right")
         ,("M-x",        "Cut")
         ,("M-c",        "Copy")
         ,("M-v",        "Paste")
         ,("C-F1",       "Save snapshot")
         ,("M-b",        "Focus interactive bar")
         ,("M-g",        "Exit interactive bar")
         ,("C-/",        "Show/Hide local keymap")
         ,("C-?",        "Show/Hide global keyamp")
         ]

-- | PageViewKeymap.
pageViewCommandMap :: PageViewKeymap
pageViewCommandMap = 
    M.fromList
         [("Scroll step up",            pageViewScrollStepUp)
         ,("Scroll step down",          pageViewScrollStepDown)
         ,("Scroll page up",            pageViewScrollPageUp)
         ,("Scroll page down",          pageViewScrollPageDown)
         ,("Scroll to top",             pageViewScrollToTop)
         ,("Scroll to bottom",          pageViewScrollToBottom)
         ,("Scroll step right",         pageViewScrollStepRight)
         ,("Scroll step left",          pageViewScrollStepLeft)
         ,("Scroll page right",         pageViewScrollPageRight)
         ,("Scroll page left",          pageViewScrollPageLeft)
         ,("Scroll to left",            pageViewScrollToLeft)
         ,("Scroll to right",           pageViewScrollToRight)
         ,("Cut",                       pageViewCutAction)
         ,("Copy",                      pageViewCopyAction)
         ,("Paste",                     pageViewPasteAction)
         ,("Save snapshot",             pageViewSaveSnapshot)
         ,("Focus interactive bar",     pageViewFocusInteractivebar)
         ,("Exit interactive bar",      interactiveExit)
         ,("Show/Hide local keymap",    pageViewShowLocalKeymap)
         ,("Show/Hide global keyamp",   pageViewShowGlobalKeymap)
         ]

-- | Interactive keymap.
interactiveKeymap :: Map Text Text
interactiveKeymap = 
    M.fromList $
         [("C-/",       "Show/Hide local keymap")
         ,("C-?",       "Show/Hide global keymap")
         ,("M-g",       "Exit interactivebar")
         ,("M-m",       "Return interactivebar")
         ,("Return",    "Return interactivebar")
         ,("Tab",       "Switch auto completion")
         ,("M-d",       "Delete all")
         ,("M-,",       "Delete backward char")
         ,("M-.",       "Delete forward char")
         ,("M-<",       "Delete backward word")
         ,("M->",       "Delete forward word")
         ,("C-M-,",     "Delete to start")
         ,("C-M-.",     "Delete to end")
         ,("M-a",       "Select all")
         ,("M-b",       "Focus")
         ,("M-x",       "Cut")
         ,("M-c",       "Copy")
         ,("M-v",       "Paste")
         ,("M-h",       "Backward char")
         ,("M-l",       "Forward char")
         ,("M-H",       "Backward word")
         ,("M-L",       "Forward word")
         ,("M-P-h",     "Move to start")
         ,("M-P-l",     "Move to end")]

-- | Interactive keymap.
interactiveCommandMap :: InteractiveKeymap
interactiveCommandMap = 
    M.fromList $
         [("Show/Hide local keymap",         pageViewShowLocalKeymap)
         ,("Show/Hide global keymap",        pageViewShowGlobalKeymap)
         ,("Exit interactivebar",            interactiveExit)
         ,("Return interactivebar",          interactiveReturn)
         ,("Switch auto completion",         interactiveSwitchAutoCompletion)
         ]
      ++ map (\ (k, f) -> (k, f . pageViewEntry))
         [("Delete all",                editableDeleteAllText)
         ,("Delete backward char",      editableDeleteBackwardChar)
         ,("Delete forward char",       editableDeleteForwardChar)
         ,("Delete backward word",      editableDeleteBackwardWord)
         ,("Delete forward word",       editableDeleteForwardWord)
         ,("Delete to start",           editableDeleteToStart)
         ,("Delete to end",             editableDeleteToEnd)
         ,("Select all",                editableSelectAll)
         ,("Focus",                     editableFocus)
         ,("Cut",                       editableCutClipboard)
         ,("Copy",                      editableCopyClipboard)
         ,("Paste",                     editablePasteClipboard)
         ,("Backward char",             editableBackwardChar)
         ,("Forward char",              editableForwardChar)
         ,("Backward word",             editableBackwardWord)
         ,("Forward word",              editableForwardWord)
         ,("Move to start",             editableMoveToStart)
         ,("Move to end",               editableMoveToEnd)
         ]

-- | Page View box.
pageViewBox :: forall a . PageView a => a -> VBox
pageViewBox = 
    pageFrameBox . pageViewFrame

-- | Page View scrolled window.
pageViewScrolledWindow :: forall a . PageView a => a -> ScrolledWindow
pageViewScrolledWindow = 
    pageFrameScrolledWindow . pageViewFrame

-- | Basic scroll action.
pageViewScrollStepUp, pageViewScrollStepDown, pageViewScrollPageUp, pageViewScrollPageDown :: PageView a => a -> IO ()
pageViewScrollStepUp   = pageViewScrollVerticalStep True 
pageViewScrollStepDown = pageViewScrollVerticalStep False 
pageViewScrollPageUp   = pageViewScrollVerticalPage True 
pageViewScrollPageDown = pageViewScrollVerticalPage False 

pageViewScrollStepRight, pageViewScrollStepLeft, pageViewScrollPageRight, pageViewScrollPageLeft :: PageView a => a -> IO ()
pageViewScrollStepRight = pageViewScrollHorizontalStep False
pageViewScrollStepLeft  = pageViewScrollHorizontalStep True
pageViewScrollPageRight = pageViewScrollHorizontalPage False
pageViewScrollPageLeft  = pageViewScrollHorizontalPage True

-- | Get plug id of page view.
pageViewGetPlugId :: forall a . PageView a => a -> IO PagePlugId
pageViewGetPlugId = readTVarIO . pageViewPlugId

-- | Get dbus client.
pageViewClient :: forall a . PageView a => a -> Client
pageViewClient view = 
    case pageViewBuffer view of
      (PageBufferWrap b) -> pageBufferClient b

-- | Cut action.
pageViewCutAction :: forall a . PageView a => a -> IO ()
pageViewCutAction view = 
    unlessM (pageViewCut view) $
            putStrLn "pageViewCutAction : Haven't implement pageViewCut."

-- | Copy action.
pageViewCopyAction :: forall a . PageView a => a -> IO ()
pageViewCopyAction view = 
    unlessM (pageViewCopy view) $
            putStrLn "pageViewCopyAction : Haven't implement pageViewCopy."

-- | Paste action.
pageViewPasteAction :: forall a . PageView a => a -> IO ()
pageViewPasteAction view = 
    unlessM (pageViewPaste view) $
            putStrLn "pageViewPasteAction : Haven't implement pageViewPaste."

-- | Apply with page view rectangle.
pageViewApplyRectangle :: forall a . PageView a => a -> (Rectangle -> IO ()) -> IO ()
pageViewApplyRectangle view func = do
    pagePlugId <- pageViewGetPlugId view
    callDaemonMethodAsync
      (pageViewClient view)
      "GetWindowAllocation"
      [toVariant pagePlugId]
      (\ err -> pageViewShowOutputbar view (show err) Nothing)               
      (\ methodReturn -> do
         let variants = messageBody methodReturn
         unless (null variants) $
                (fromVariant (head variants) :: Maybe Rectangle)
                ?>= func)

-- | Is focus on keymap window.
pageViewIsFocusKeymapWindow :: forall a . PageView a => a -> IO Bool
pageViewIsFocusKeymapWindow =
    keymapWindowIsVisible . pageFrameKeymapWindow . pageViewFrame

-- | Page view keymap scrolled window.
pageViewKeymapScrolledWindow :: forall a . PageView a => a -> ScrolledWindow
pageViewKeymapScrolledWindow =
    kwScrolledWindow . pageFrameKeymapWindow . pageViewFrame

-- | Show/Hide local keymap.
pageViewShowLocalKeymap :: forall a . PageView a => a -> IO ()
pageViewShowLocalKeymap view = do
  let keymapWin = pageFrameKeymapWindow $ pageViewFrame view
      frame = pageFrameFrame $ pageViewFrame view
  isVisible <- keymapWindowIsVisible keymapWin
  if isVisible 
     then keymapWindowHide keymapWin
     else 
         pageViewApplyRectangle view $ \ (Rectangle x y w h) -> 
             postGUIAsync $ do
               let rw = floor (fromIntegral w * 2 / 3)
                   rh = floor (fromIntegral h * 2 / 3)
                   rx = (x + floor (fromIntegral w / 6))
                   ry = (y + floor (fromIntegral h / 6))
               keymapWindowShow keymapWin 
                                frame
                                (Rectangle rx ry rw rh)
                                [("Local keymap", M.toList (pageViewLocalKeymap view))]

-- | Show/Hide global keymap.
pageViewShowGlobalKeymap :: forall a . PageView a => a -> IO ()
pageViewShowGlobalKeymap view = do
  let keymapWin = pageFrameKeymapWindow $ pageViewFrame view
      frame = pageFrameFrame $ pageViewFrame view
  isVisible <- keymapWindowIsVisible keymapWin
  if isVisible 
     then keymapWindowHide keymapWin
     else do
       pagePlugId <- pageViewGetPlugId view
       callDaemonMethodAsync
          (pageViewClient view)
          "GetGlobalKeymap"
          [toVariant pagePlugId]
          (\ err -> pageViewShowOutputbar view (show err) Nothing)               
          (\ methodReturn -> do
             let variants = messageBody methodReturn
             unless (null variants) $
                        (fromVariant (head variants) :: Maybe (Rectangle, [(Text, Text)]))
                        ?>= \ ((Rectangle x y w h), list) ->
                            postGUIAsync $ do
                                  let rw = floor (fromIntegral w * 2 / 3)
                                      rh = floor (fromIntegral h * 2 / 3)
                                      rx = (x + floor (fromIntegral w / 6))
                                      ry = (y + floor (fromIntegral h / 6))
                                  keymapWindowShow keymapWin 
                                                   frame
                                                   (Rectangle rx ry rw rh)
                                                   [("Global keymap", list)
                                                   ,("Page view keymap", M.toList pageViewKeymap)
                                                   ,("Interactivebar keymap", M.toList interactiveKeymap)
                                                   ,("Helper keymap", M.toList helperKeymap)])

-- | Save page view snapshot.
pageViewSaveSnapshot :: forall a . PageView a => a -> IO ()
pageViewSaveSnapshot view = 
  widgetGetSnapshotPixbuf (pageViewScrolledWindow view) >?>= \pixbuf -> do
    -- Get snapshot save path.
    configDir <- getConfigDirectory
    time <- getClockTime
    let snapshotDir = configDir </> "snapshot"
        snapshotPath = snapshotDir </> (show time ++ "_snapshot.png")

    -- Create it directory if haven't create.
    createDirectoryIfMissing True snapshotDir

    -- Save snapshot.
    pixbufSave pixbuf snapshotPath "png" []
    pageViewShowOutputbar view ("Has save snapshot at : " ++ snapshotPath) Nothing

-- | Hide completion window.
pageViewHideCompletionWindow :: forall a . PageView a => a -> IO ()
pageViewHideCompletionWindow =
    completionWindowHide . pageFrameCompletionWindow . pageViewFrame 

-- | Show outputbar.
pageViewShowOutputbar :: forall a . PageView a => a -> String -> Maybe Int -> IO ()
pageViewShowOutputbar =
    pageFrameShowOutputbar . pageViewFrame

-- | Update Statusbar.
pageViewUpdateStatusbar :: forall a . PageView a => a -> String -> String -> IO ()
pageViewUpdateStatusbar =
    pageFrameUpdateStatusbar . pageViewFrame

-- | Update progress.
pageViewUpdateProgress :: forall a . PageView a => a -> Double -> IO ()
pageViewUpdateProgress =
    pageFrameUpdateProgress . pageViewFrame

-- | Page view entry.
pageViewEntry :: forall a . PageView a => a -> Entry
pageViewEntry = 
  interactivebarEntry . pageFrameInteractivebar . pageViewFrame

-- | Is interactivebar is focus.
pageViewIsFocusInteractivebar :: forall a . PageView a => a -> IO Bool
pageViewIsFocusInteractivebar = 
  widgetGetIsFocus . pageViewEntry

-- | Focus interactivebar.
pageViewFocusInteractivebar :: forall a . PageView a => a -> IO ()
pageViewFocusInteractivebar = 
  editableFocus . pageViewEntry

-- | Interactive.
interactive :: forall a . PageView a => a -> [(InteractiveType, String, String)] -> ([String] -> IO ()) -> IO ()
interactive view [] _ = pageViewShowOutputbar view "Interactive arguments is empty." Nothing
interactive view ((typ, title, content):rest) func = do
  -- Init.
  let pageFrame = pageViewFrame view 
  
  -- Show interactivebar first.
  pageFrameShowInteractivebar pageFrame
    
  -- Set interactive data.
  writeTVarIO (pageFrameInteractiveArgs pageFrame) rest
  writeTVarIO (pageFrameInteractiveResult pageFrame) []
  writeTVarIO (pageFrameInteractiveFun pageFrame) func

  -- Prompt first one.
  writeTVarIO (pageFrameInteractiveType pageFrame) typ
  interactivebarSetTitle (pageFrameInteractivebar pageFrame) title
  interactivebarSetContent (pageFrameInteractivebar pageFrame) content

-- | Interactive return.
interactiveReturn :: forall a . PageView a => a -> IO ()
interactiveReturn view = do
  -- Init.
  let pageFrame = pageViewFrame view 

  -- Hide completion window first.
  pageViewHideCompletionWindow view

  -- Get input.
  status <- interactivebarGetStatus (pageFrameInteractivebar pageFrame) 
  case status of
    Nothing -> pageViewShowOutputbar view "Interactive is invisible." Nothing
    Just (_, (arg, _)) -> do
        -- Store input.
        modifyTVarIO (pageFrameInteractiveResult pageFrame) (\ list -> list ++ [arg])

        args <- readTVarIO $ pageFrameInteractiveArgs pageFrame
        case args of
          [] -> do
                -- Read result and function.
                result <- readTVarIO $ pageFrameInteractiveResult pageFrame
                fun    <- readTVarIO $ pageFrameInteractiveFun pageFrame

                -- Exit interactivebar first.
                interactiveExit view

                -- Execute command.
                Exc.catch 
                       (fun result)
                       -- Show error if catch any exception when do command.
                       (\ (e :: SomeException) -> 
                          pageViewShowOutputbar view (show e) Nothing)
          -- Otherwise continue read argument from user.
          ((typ, title, content):rest) -> do
                -- Popup top element from arguments.
                writeTVarIO (pageFrameInteractiveArgs pageFrame) rest

                -- Prompt next argument.
                writeTVarIO (pageFrameInteractiveType pageFrame) typ
                interactivebarSetTitle (pageFrameInteractivebar pageFrame) title
                interactivebarSetContent (pageFrameInteractivebar pageFrame) content
       
-- | Close interactivebar.
interactiveExit :: forall a . PageView a => a -> IO ()
interactiveExit view = do
  -- Close interactive first.
  pageFrameCloseInteractivebar $ pageViewFrame view
  pageViewHideCompletionWindow view

-- | Switch auto completion.
interactiveSwitchAutoCompletion :: forall a . PageView a => a -> IO ()
interactiveSwitchAutoCompletion view = 
  pageFrameSwitchAutoCompletion (pageViewFrame view)

-- | Interactive complete.
interactiveCompletion :: forall a . PageView a => a -> IO ()
interactiveCompletion view = do
  -- Init.
  let pageFrame = pageViewFrame view
      window = pageFrameCompletionWindow pageFrame
      entry = pageViewEntry view

  -- completion <- interactiveGetCompletion view
  -- case completion of

  case [1] of

    [] -> return ()
    _ -> 
        pageViewApplyRectangle view $ \ (Rectangle wx wy _ _) -> 
            postGUIAsync $ do
              -- Get coordinate.
              (Rectangle x y w h) <- widgetGetAllocation entry
      
              -- Show completion window.
              completionWindowShow window (pageViewEntry view) (Rectangle (wx + x) (wy + y + h) w 100)

-- -- | Get completion.
-- interactiveGetCompletion :: forall a . PageView a => a -> IO [String]
-- interactiveGetCompletion view = do
--   let pageFrame = pageViewFrame view
--   iType <- readTVarIO $ pageFrameInteractiveType pageFrame 
--   case iType of
--     IFile -> do
--       status <- interactivebarGetStatus (pageFrameInteractivebar pageFrame) 
--       case status of
--         Just (_, (input, _)) -> do
--            home <- getEnv "HOME"
--            let -- Expand '~' with HOME value. 
--                path = if ("~" :: String) `isPrefixOf` filepath
--                           then home ++ drop (length ("~" :: String)) filepath
--                           else filepath
--            -- Try to get upper directory of current file path.
--            upperDir | hasTrailingPathSeparator path
--                         = fromString path
--                     | otherwise
--                         = fromString $ getUpperDirectory path
--            if directoryDoesExist upperDir
--               then do
--                 fileInfos <- directoryGetFileInfos upperDir
--                 let infos = map (\info -> ((fileInfoGetDisplayNameWithType info), (fileInfoGetFileType info))) fileInfos
--                     files = sortBy (\ a b -> 
--                                         compareFileWithType 
--                                         (fileInfoDisplayName a, fileInfoFileType a)
--                                         (fileInfoDisplayName b, fileInfoFileType b)
--                                    ) infos
--                 return $ map fst files
--               else return []
--         Nothing -> return []
--     _ -> return [] 

-- | Show tooltip.
pageViewShowTooltip :: forall a . PageView a => a -> String -> Maybe Point -> IO ()
pageViewShowTooltip view text Nothing = 
  tooltipNew (pageViewPropagateWidget view)
             text
             Nothing
             Nothing Nothing Nothing
  >> return ()
pageViewShowTooltip view text (Just (px, py)) = 
    pageViewApplyRectangle view $ \ (Rectangle x y _ _) -> postGUIAsync $ 
        tooltipNew (pageViewPropagateWidget view)
                   text
                   (Just (x + px, y + py))
                   Nothing Nothing Nothing
        >> return ()
        
