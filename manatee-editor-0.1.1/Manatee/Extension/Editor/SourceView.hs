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

{-# LANGUAGE ExistentialQuantification, RankNTypes, DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Manatee.Extension.Editor.SourceView where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Graphics.UI.Gtk.SourceView.SourceLanguage
import Graphics.UI.Gtk.SourceView.SourceLanguageManager
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.PageFrame
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Extension.Editor.SourceBuffer
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Gtk
import Manatee.Toolkit.Gtk.Multiline
import Paths_manatee_editor
import System.FilePath
import Text.Printf

import qualified Data.Map as M
import qualified Graphics.UI.Gtk.SourceView.SourceBuffer as SB
import qualified Graphics.UI.Gtk.SourceView.SourceView as SV

data SourceView =
    SourceView {sourceViewPlugId          :: TVar PagePlugId
               ,sourceViewFrame           :: PageFrame
               ,sourceViewView            :: SV.SourceView
               ,sourceViewBuffer          :: SourceBuffer
               }
    deriving Typeable

instance PageBuffer SourceBuffer where
    pageBufferGetName           = readTVarIO . sourceBufferFilePath
    pageBufferSetName a         = writeTVarIO (sourceBufferFilePath a)
    pageBufferClient            = sourceBufferClient
    pageBufferCreateView a pId  = PageViewWrap <$> sourceViewNew a pId
    pageBufferMode              = sourceBufferMode
    pageBufferPackageName _     = fmap takeFileName getDataDir
    pageBufferWriteState        = sourceBufferWriteState
    pageBufferReadState         = sourceBufferReadState

instance PageView SourceView where
    pageViewBuffer               = PageBufferWrap . sourceViewBuffer
    pageViewPlugId               = sourceViewPlugId
    pageViewFrame                = sourceViewFrame
    pageViewCut                  = sourceViewCut
    pageViewCopy                 = sourceViewCopy
    pageViewPaste                = sourceViewPaste
    pageViewLocalKeymap _        = sourceViewLocalKeymap
    pageViewLocalCommandMap _    = sourceViewLocalCommandMap
    pageViewFocus                = widgetGrabFocus . sourceViewView
    pageViewPropagateWidget      = castToWidget . sourceViewView
    pageViewSaveState view       = sourceViewSaveState view Nothing
    pageViewRestoreState view    = sourceViewRestoreState view Nothing
    pageViewWriteState view path = sourceViewSaveState view (Just path)
    pageViewReadState view path  = sourceViewRestoreState view (Just path)
    pageViewScrollToTop          = sourceViewScrollToTop
    pageViewScrollToBottom       = sourceViewScrollToBottom
    pageViewScrollVerticalPage   = sourceViewScrollVerticalPage
    pageViewScrollVerticalStep   = sourceViewScrollVerticalStep

-- | Internal function for create string buffer.
sourceViewNew :: SourceBuffer -> PagePlugId -> IO SourceView
sourceViewNew sb plugId = do
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ sourceBufferMode sb)

  sourceView <- SV.sourceViewNewWithBuffer (sourceBufferBuffer sb)
  pageFrameAddChild pFrame sourceView

  -- Set default font of source view.
  fontDescr <- fontDescriptionFromString "Monospace"
  widgetModifyFont sourceView (Just fontDescr)

  let sv = SourceView pId pFrame sourceView sb

  -- Load syntax highlight.
  sourceViewSyntaxHighlight sv

  -- Customize setup.
  SV.sourceViewSetHighlightCurrentLine sourceView True      -- highlight current line.
  SV.sourceViewSetInsertSpacesInsteadOfTabs sourceView True -- use space instead tabs
  SV.sourceViewSetShowLineNumbers sourceView True           -- show line number
  textViewSetCursorVisible sourceView True                  -- make cursor visible

  -- Update status after expose.
  sourceViewView sv `afterExposeRect` \ _ -> do
    sourceViewDisplayPositionStatus sv -- display position
    sourceViewDisplayPercentStatus sv  -- display percent

  -- Handle selection mark.
  sourceViewHandleSelectionMark sv

  return sv

-- | Get text buffer.
sourceViewGetTextBuffer :: SourceView -> IO TextBuffer 
sourceViewGetTextBuffer = textViewGetBuffer . sourceViewView

-- | Get source buffer.
sourceViewGetSourceBuffer :: SourceView -> IO SB.SourceBuffer
sourceViewGetSourceBuffer sb = 
    SB.castToSourceBuffer <$> sourceViewGetTextBuffer sb

-- | Get buffer content.
sourceViewGetText :: SourceView -> IO String
sourceViewGetText = textViewGetText . sourceViewView

-- | Get current line.
sourceViewGetLine :: SourceView -> IO Int
sourceViewGetLine = textViewGetLine . sourceViewView

-- | Get line count.
sourceViewGetLineCount :: SourceView -> IO Int
sourceViewGetLineCount = textViewGetLineCount . sourceViewView

-- | Get current column.
sourceViewGetColumn :: SourceView -> IO Int
sourceViewGetColumn = textViewGetColumn . sourceViewView

-- | Display position information.
sourceViewDisplayPositionStatus :: SourceView -> IO ()
sourceViewDisplayPositionStatus sb = 
  liftM2 (printf "Pos (%d, %d)") (sourceViewGetLine sb) (sourceViewGetColumn sb)
         >>= pageViewUpdateStatusbar sb "Pos"

-- | Display percent information.
sourceViewDisplayPercentStatus :: SourceView -> IO ()
sourceViewDisplayPercentStatus sv = do
  line <- sourceViewGetLine sv
  lineCount <- sourceViewGetLineCount sv
  pageViewUpdateStatusbar sv "Percent" ("(" ++ show (floor (i2d (line * 100) / i2d lineCount)) ++ "%)")
  
-- | Handle selection mark.
sourceViewHandleSelectionMark :: SourceView -> IO ()  
sourceViewHandleSelectionMark sv = do
  -- Get source view.
  let sourceView = sourceViewView sv 

  -- Cancel selection when button press.
  onButtonPress sourceView (\_ -> textViewCancelSelectionMark sourceView >> return False)

  -- Update selection iter after buffer changed.
  buffer <- textViewGetBuffer sourceView
  onBufferChanged buffer (textBufferUpdateSelectionIter buffer)

  -- Apply selection iter when focus in.
  onFocusIn sourceView (\_ -> sourceViewApplySelectionMark sv >> return False)

  return ()

-- | Get string buffer language.
sourceViewGetLanguage :: SourceView -> IO (Maybe SourceLanguage)
sourceViewGetLanguage buffer = 
    SB.sourceBufferGetLanguage
        =<< sourceViewGetSourceBuffer buffer

-- | Set string buffer language.
sourceViewSetLanguage :: SourceView -> SourceLanguage -> IO ()
sourceViewSetLanguage buffer language = 
    (<=<) (`SB.sourceBufferSetLanguage` Just language) sourceViewGetSourceBuffer buffer

-- | Syntax highlight with file name.
sourceViewSyntaxHighlight :: SourceView -> IO ()
sourceViewSyntaxHighlight view = do
  lm <- sourceLanguageManagerNew
  name <- sourceViewName view
  (_, lang) <- sourceLanguageForFilename lm (Just name)
  lang ?>= sourceViewSetLanguage view

-- | Save buffer.
sourceViewSave :: SourceView -> IO ()
sourceViewSave a = do
  filepath <- sourceViewName a
  string <- sourceViewGetText a
  writeFile filepath string
  name <- sourceViewName a
  pageViewShowOutputbar a ("Save " ++ name) Nothing

-- | Get buffer name.
sourceViewName :: SourceView -> IO String
sourceViewName = 
  pageBufferGetName . sourceViewBuffer

-- | Undo.
sourceViewUndo :: SourceView -> IO ()
sourceViewUndo a = do
  sb <- sourceViewGetSourceBuffer a
  ifM (SB.sourceBufferGetCanUndo sb)
          (do
            SB.sourceBufferUndo sb
            pageViewShowOutputbar a "Undo!" Nothing)
          (pageViewShowOutputbar a "No further undo information." Nothing)

-- | Redo.
sourceViewRedo :: SourceView -> IO ()
sourceViewRedo a = do
  sb <- sourceViewGetSourceBuffer a
  ifM (SB.sourceBufferGetCanRedo sb)
          (do
            SB.sourceBufferRedo sb
            pageViewShowOutputbar a "Redo!" Nothing)
          (pageViewShowOutputbar a "No further redo information." Nothing)

-- | String view wrap user action for undo/redo.
sourceViewWrapAction :: SourceView -> IO () -> IO ()  
sourceViewWrapAction  = textViewWrapAction . sourceViewView

-- | Newline.
sourceViewNewline :: SourceView -> IO ()  
sourceViewNewline = textViewNewLine . sourceViewView

-- | Open newline below.
sourceViewOpenNewlineBelow :: SourceView -> IO ()
sourceViewOpenNewlineBelow a = 
    textViewOpenNewlineBelow (sourceViewView a) (sourceViewScrolledWindow a)

-- | Open newline above.
sourceViewOpenNewlineAbove :: SourceView -> IO ()
sourceViewOpenNewlineAbove a = 
    textViewOpenNewlineAbove (sourceViewView a) (sourceViewScrolledWindow a)

-- | Select all.
sourceViewSelectAll :: SourceView -> IO ()
sourceViewSelectAll = textViewSelectAll . sourceViewView

-- | Delete.
sourceViewDelete :: SourceView -> IO ()
sourceViewDelete view = 
  textViewDelete (sourceViewView view) True True  >> return ()

-- | Cut.
sourceViewCut :: SourceView -> IO Bool
sourceViewCut view = do
  textViewCut $ sourceViewView view
  return True

-- | Copy.
sourceViewCopy :: SourceView -> IO Bool
sourceViewCopy view = do
  textViewCopy $ sourceViewView view
  return True

-- | Paste.
sourceViewPaste :: SourceView -> IO Bool
sourceViewPaste view = do
  textViewPaste $ sourceViewView view
  return True

-- | Forward line.
sourceViewForwardLine :: SourceView -> IO ()
sourceViewForwardLine a = do
    textViewForwardLine (sourceViewView a) (sourceViewScrolledWindow a)
    sourceViewApplySelectionMark a

-- | Backward line.
sourceViewBackwardLine :: SourceView -> IO ()
sourceViewBackwardLine a = do
    textViewBackwardLine (sourceViewView a) (sourceViewScrolledWindow a)
    sourceViewApplySelectionMark a

-- | Forward char.
sourceViewForwardChar :: SourceView -> IO ()
sourceViewForwardChar a = do
    textViewForwardChar (sourceViewView a) (sourceViewScrolledWindow a)
    sourceViewApplySelectionMark a

-- | Backward char.
sourceViewBackwardChar :: SourceView -> IO ()
sourceViewBackwardChar a = do
    textViewBackwardChar (sourceViewView a) (sourceViewScrolledWindow a)
    sourceViewApplySelectionMark a

-- | Forward word.
sourceViewForwardWord :: SourceView -> IO ()
sourceViewForwardWord a = do
    textViewForwardWord (sourceViewView a) (sourceViewScrolledWindow a)
    sourceViewApplySelectionMark a

-- | Backward word.
sourceViewBackwardWord :: SourceView -> IO ()
sourceViewBackwardWord a = do
    textViewBackwardWord (sourceViewView a) (sourceViewScrolledWindow a)
    sourceViewApplySelectionMark a

-- | Begin.
sourceViewScrollToTop :: SourceView -> IO ()
sourceViewScrollToTop a = do 
  textViewBegin (sourceViewView a) (sourceViewScrolledWindow a)
  sourceViewApplySelectionMark a

-- | End.
sourceViewScrollToBottom :: SourceView -> IO ()
sourceViewScrollToBottom a = do 
  textViewEnd (sourceViewView a) (sourceViewScrolledWindow a)
  sourceViewApplySelectionMark a

-- | Smart home.
sourceViewSmartHome :: SourceView -> IO ()
sourceViewSmartHome a = do
    textViewSmartHome $ sourceViewView a
    sourceViewApplySelectionMark a

-- | Smart end.
sourceViewSmartEnd :: SourceView -> IO ()
sourceViewSmartEnd a = do
    textViewSmartEnd $ sourceViewView a
    sourceViewApplySelectionMark a

-- | Delete forward char.
sourceViewDeleteForwardChar :: SourceView -> IO ()
sourceViewDeleteForwardChar view = 
  textViewDeleteForwardChar (sourceViewView view) False >> return ()

-- | Backward char.
sourceViewDeleteBackwardChar :: SourceView -> IO ()
sourceViewDeleteBackwardChar view = 
  textViewDeleteBackwardChar (sourceViewView view) False >> return ()

-- | Forward word.
sourceViewDeleteForwardWord :: SourceView -> IO ()
sourceViewDeleteForwardWord view = 
  textViewDeleteForwardWord (sourceViewView view) False >> return ()

-- | Backward word.
sourceViewDeleteBackwardWord :: SourceView -> IO ()
sourceViewDeleteBackwardWord view = 
  textViewDeleteBackwardWord (sourceViewView view) False >> return ()

-- | Delete to line end.
sourceViewDeleteToLineEnd :: SourceView -> IO ()
sourceViewDeleteToLineEnd view = 
  textViewDeleteToLineEnd (sourceViewView view) False >> return ()

-- | Delete to line start.
sourceViewDeleteToLineStart :: SourceView -> IO ()
sourceViewDeleteToLineStart view = 
  textViewDeleteToLineStart (sourceViewView view) False >> return ()

-- | Duplicate lines.
sourceViewDupLinesBelow, sourceViewDupLinesAbove :: SourceView -> IO ()
sourceViewDupLinesBelow = textViewDupLinesBelow . sourceViewView
sourceViewDupLinesAbove = textViewDupLinesAbove . sourceViewView

-- | Delete lines.
sourceViewDelLines :: SourceView -> IO ()
sourceViewDelLines view = 
  textViewDelLines (sourceViewView view) >> return ()

-- | Transposes lines.
sourceViewTraLinesBelow, sourceViewTraLinesAbove :: SourceView -> IO ()
sourceViewTraLinesBelow = textViewTraLinesBelow . sourceViewView
sourceViewTraLinesAbove = textViewTraLinesAbove . sourceViewView

-- | Reload file.
sourceViewReload :: SourceView -> IO ()
sourceViewReload sv = do
  -- Reload file.
  name <- sourceViewName sv
  textViewLoadFile (sourceViewView sv) name

  -- Move to view begin.
  sourceViewScrollToTop sv

-- | Set text.
sourceViewSetText :: SourceView -> String -> IO ()
sourceViewSetText = textViewSetText . sourceViewView

-- | Local keymap.
sourceViewLocalKeymap :: Map Text Text
sourceViewLocalKeymap = 
    M.fromList
         [("M-a",    "Select all")
         ,("M-s",    "Save")
         ,("M-d",    "Delete lines")
         ,("M-D",    "Delete")
         ,("M-/",    "Undo")
         ,("M-?",    "Redo")
         ,("M-r",    "Reload")
         ,("M-,",    "Backward delete char")
         ,("M-.",    "Forward delete char")
         ,("M-<",    "Backward delete word")
         ,("M->",    "Forward delete word")
         ,("C-M-,",  "Delete to line start")
         ,("C-M-.",  "Delete to line end")
         ,("M-j",    "Forward line")
         ,("M-k",    "Backward line")
         ,("M-l",    "Forward char")
         ,("M-h",    "Backward char")
         ,("M-m",    "New line")
         ,("Down",   "Forward line")
         ,("Up",     "Backward line")
         ,("Left",   "Forward char")
         ,("Right",  "Backward char")
         ,("Return", "New line")
         ,("M-L",    "Forward word")
         ,("M-H",    "Backward word")
         ,("M-P-h",  "Smart home")
         ,("M-P-l",  "Smart end")
         ,("M-N",    "Open new line below")
         ,("M-P",    "Open new line above")
         ,("M-w",    "Duplicate lines below")
         ,("M-W",    "Duplicate lines above")
         ,("M-e",    "Transposes line below")
         ,("M-E",    "Transposes line above")
         ,("C-c",    "Toggle selection mark")
         ,("C-C",    "Exchange selection mark")
         ,("C-o",    "Open file")
         ,("C-g",    "Goto line")
         ,("C-G",    "Goto column")
         ]

-- | String buffer keymap.
sourceViewLocalCommandMap :: Map Text (SourceView -> IO ())
sourceViewLocalCommandMap = 
    M.fromList 
         [("Select all",                sourceViewSelectAll)
         ,("Save",                      sourceViewSave)
         ,("Delete lines",              sourceViewDelLines)
         ,("Delete",                    sourceViewDelete)
         ,("Undo",                      sourceViewUndo)
         ,("Redo",                      sourceViewRedo)
         ,("Reload",                    sourceViewReload)
         ,("Backward delete char",      sourceViewDeleteBackwardChar)
         ,("Forward delete char",       sourceViewDeleteForwardChar)
         ,("Backward delete word",      sourceViewDeleteBackwardWord)
         ,("Forward delete word",       sourceViewDeleteForwardWord)
         ,("Delete to line start",      sourceViewDeleteToLineStart)
         ,("Delete to line end",        sourceViewDeleteToLineEnd)
         ,("Forward line",              sourceViewForwardLine)
         ,("Backward line",             sourceViewBackwardLine)
         ,("Forward char",              sourceViewForwardChar)
         ,("Backward char",             sourceViewBackwardChar)
         ,("New line",                  sourceViewNewline)
         ,("Forward word",              sourceViewForwardWord)
         ,("Backward word",             sourceViewBackwardWord)
         ,("Smart home",                sourceViewSmartHome)
         ,("Smart end",                 sourceViewSmartEnd)
         ,("Open new line below",       sourceViewOpenNewlineBelow)
         ,("Open new line above",       sourceViewOpenNewlineAbove)
         ,("Duplicate lines below",     sourceViewDupLinesBelow)
         ,("Duplicate lines above",     sourceViewDupLinesAbove)
         ,("Transposes line below",     sourceViewTraLinesBelow)
         ,("Transposes line above",     sourceViewTraLinesAbove)
         ,("Toggle selection mark",     sourceViewToggleSelectionMark)
         ,("Exchange selection mark",   sourceViewExchangeSelectionMark)
         ,("Open file",                 sourceViewOpenFile)
         ,("Goto line",                 sourceViewGotoLine)
         ,("Goto column",               sourceViewGotoColumn)
         ]

-- | Open file.
sourceViewOpenFile :: SourceView -> IO ()
sourceViewOpenFile view = 
  interactive view [(IFile, "Open file : ", "")] $ \ [path] ->                       
      mkDaemonSignal (pageViewClient view) NewTab (NewTabArgs "PageEditor" path [])

-- | Goto column.
sourceViewGotoColumn :: SourceView -> IO ()          
sourceViewGotoColumn view@SourceView {sourceViewView = sourceView} = 
  interactive view [(INum, "Column : ", "")] $ \ [column] -> do
      let number = read column :: Int
      textViewGotoColumn sourceView number
      pageViewShowOutputbar view (show [column]) Nothing
  
-- | Goto row.  
sourceViewGotoLine :: SourceView -> IO ()
sourceViewGotoLine view@SourceView {sourceViewView   = sourceView
                                   ,sourceViewBuffer = sourceBuffer} = do
  let buffer = sourceBufferBuffer sourceBuffer
  lines <- textBufferGetLineCount buffer
  interactive view [(INum, "Line (1 - " ++ show lines ++ ") : ", "")] $ \ [line] -> do
      let number = read line :: Int
      textViewGotoLine sourceView number

-- | Set selection mark.
sourceViewToggleSelectionMark :: SourceView -> IO ()
sourceViewToggleSelectionMark view = 
  ifM (textViewToggleSelectionMark $ sourceViewView view )
      (pageViewUpdateStatusbar view "Selection" "Selection (Active)")
      (pageViewUpdateStatusbar view "Selection" "Selection (Inactive)")

-- | Exchange selection mark.
sourceViewExchangeSelectionMark :: SourceView -> IO ()
sourceViewExchangeSelectionMark = textViewExchangeSelectionMark . sourceViewView 

-- | Show selection mark.
sourceViewApplySelectionMark :: SourceView -> IO ()
sourceViewApplySelectionMark = textViewApplySelectionMark . sourceViewView

-- | Scroll page vertically.
sourceViewScrollVerticalPage :: Bool -> SourceView -> IO ()
sourceViewScrollVerticalPage isDown a = do
  let sw = sourceViewScrolledWindow a
      tv = sourceViewView a
  pageInc <- (<=<) adjustmentGetPageIncrement scrolledWindowGetVAdjustment sw
  textViewScrollVertical tv sw (if isDown then pageInc else (- pageInc))
  sourceViewApplySelectionMark a

-- | Scroll step vertically.
sourceViewScrollVerticalStep :: Bool -> SourceView -> IO ()
sourceViewScrollVerticalStep isDown a = do
  let sw = sourceViewScrolledWindow a
      tv = sourceViewView a
  ti <- textViewGetTextIter tv
  (_, lineHeight) <- textViewGetLineYrange tv ti
  let stepInc = i2d lineHeight
  textViewScrollVertical tv sw (if isDown then stepInc else (- stepInc))
  sourceViewApplySelectionMark a

-- | Source view scrolled window.
sourceViewScrolledWindow :: SourceView -> ScrolledWindow    
sourceViewScrolledWindow =
  pageFrameScrolledWindow . sourceViewFrame

-- | Save state.
sourceViewSaveState :: SourceView -> Maybe FilePath -> IO ()
sourceViewSaveState view@(SourceView {sourceViewBuffer  = buffer}) 
                    statePath = do
  -- Get cursor.
  line <- textBufferGetLine (sourceBufferBuffer buffer)
  column <- textBufferGetLineOffset (sourceBufferBuffer buffer)

  -- Get scroll position.
  scrolledWindowPosition <- textViewGetCursorAlign (sourceViewView view)

  -- Save state.
  let state = SourceState (Just (line, column)) scrolledWindowPosition 
  case statePath of
    Nothing   -> writeTVarIO (sourceBufferState buffer) state
    Just path -> writeConfigPath path state

-- | Restore state.
sourceViewRestoreState :: SourceView -> Maybe FilePath -> IO ()
sourceViewRestoreState SourceView {sourceViewBuffer  = buffer
                                  ,sourceViewView    = textView} 
                       statePath = do
  bufferState <- readTVarIO (sourceBufferState buffer)
  (SourceState cursor scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore cursor.
  cursor ?>= \ (line, column) -> do
    textViewGotoLine textView line
    textViewGotoColumn textView column

  -- Restore cursor alignment.
  -- TODO, we need find a way to restore TextView scroll position.
  -- textViewSetCursorAlign can't work if cursor not in visible area.
  textViewSetCursorAlign textView scrolledWindowPosition
