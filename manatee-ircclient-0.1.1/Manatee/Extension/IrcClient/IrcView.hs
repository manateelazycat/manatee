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
module Manatee.Extension.IrcClient.IrcView where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get, Language)
import Graphics.UI.Gtk.SourceView
import Language.Translate.Google
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.PageFrame
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Extension.IrcClient.DBus
import Manatee.Extension.IrcClient.HighlightNick
import Manatee.Extension.IrcClient.IrcBuffer
import Manatee.Extension.IrcClient.Types
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.ByteString
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.String
import Manatee.Toolkit.Gtk.Concurrent
import Manatee.Toolkit.Gtk.Multiline
import Network.FastIRC.Messages
import Paths_manatee_ircclient
import System.FilePath
import Text.Morse

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.UTF8 as UTF8

data IrcView =
    IrcView {ircViewPlugId            :: TVar PagePlugId
            ,ircViewFrame             :: PageFrame
            ,ircViewView              :: SourceView
            ,ircViewBuffer            :: IrcBuffer
            ,ircViewBroadcastChannel  :: ViewChannel IrcBufferSignal
            }
    deriving Typeable

instance PageBuffer IrcBuffer where
    pageBufferGetName           = return . ircBufferChannel
    pageBufferSetName _ _       = return ()
    pageBufferClient            = ircBufferClient
    pageBufferCreateView a pId  = PageViewWrap <$> ircViewNew a pId
    pageBufferMode              = ircBufferMode
    pageBufferPackageName _     = fmap takeFileName getDataDir

instance PageView IrcView where
    pageViewBuffer               = PageBufferWrap . ircViewBuffer
    pageViewPlugId               = ircViewPlugId
    pageViewFrame                = ircViewFrame
    pageViewLocalKeymap _        = ircViewLocalKeymap
    pageViewLocalCommandMap _    = ircViewLocalCommandMap
    pageViewFocus                = widgetGrabFocus . ircViewView
    pageViewPropagateWidget      = castToWidget . ircViewView
    pageViewSaveState view       = ircViewSaveState view Nothing
    pageViewRestoreState view    = ircViewRestoreState view Nothing
    pageViewWriteState view path = ircViewSaveState view (Just path)
    pageViewReadState view path  = ircViewRestoreState view (Just path)
    pageViewCut                  = ircViewCut
    pageViewCopy                 = ircViewCopy
    pageViewPaste                = ircViewPaste
    pageViewScrollToTop          = ircViewScrollToTop
    pageViewScrollToBottom       = ircViewScrollToBottom
    pageViewScrollVerticalPage   = ircViewScrollVerticalPage
    pageViewScrollVerticalStep   = ircViewScrollVerticalStep

-- | Create new irc view.
ircViewNew :: IrcBuffer -> PagePlugId -> IO IrcView
ircViewNew sb plugId = do
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ ircBufferMode sb)

  -- Create source view.
  ircView <- sourceViewNewWithBuffer (ircBufferBuffer sb)
  pageFrameAddChild pFrame ircView
  forM_ [StateNormal, StateActive, StatePrelight, StateSelected, StateInsensitive] 
    $ \state -> widgetModifyBg ircView state (nickColorToColor backgroundColor)

  -- Broadcast channel.
  channel <- createViewChannel (ircBufferBroadcastChannel sb) ircView

  let sv = IrcView pId pFrame ircView sb channel

  -- Customize setup.
  sourceViewSetHighlightCurrentLine ircView True            -- highlight current line.
  sourceViewSetInsertSpacesInsteadOfTabs ircView True       -- use space instead tabs
  set ircView                                               -- show line number
    [sourceViewShowLineNumbers :=> readTVarIO (ircCustomizeShowLineNumber $ ircBufferCustomize sb)]
  textViewSetCursorVisible ircView True                     -- make cursor visible
  textViewSetWrapMode ircView WrapWord                      -- auto wrap text

  -- Set fixed-width font, otherwise wrap mode can't work.
  fontDescr <- fontDescriptionFromString "Monospace"
  widgetModifyFont ircView (Just fontDescr)

  -- Update time stamp at right.
  timeStampPosition <- readTVarIO (ircCustomizeTimeStampPosition $ ircBufferCustomize sb)
  gutter <- sourceViewGetGutter ircView timeStampPosition
  cell   <- cellRendererTextNew
  sourceGutterInsert gutter cell 0

  -- Set gutter data.
  sourceGutterSetCellDataFunc gutter cell $ \ c l _ -> do
         -- Display time stamp.
         timeStampMap <- readTVarIO $ ircBufferTimeStamp sb
         let (timeStamp, timeColor) = 
                 case findMinMatch timeStampMap (\ line _ -> line == l) of
                   Just x  -> snd x
                   Nothing -> (defaultTimeStamp, Color 0 0 0)
         set (castToCellRendererText c) [cellText := timeStamp]
         set (castToCellRendererText c) [cellTextForegroundColor := timeColor]

  -- Set gutter size.
  sourceGutterSetCellSizeFunc gutter cell $ \ c -> 
      -- -1 mean cell renderer will adjust width with chars dynamically.
      set (castToCellRendererText c) [cellTextWidthChars := (-1)]

  -- Read channel.
  ircViewListenChannel sv

  return sv

-- | Listen broadcast channel for draw view synchronous.
ircViewListenChannel :: IrcView -> IO ()
ircViewListenChannel view = 
  listenViewChannel (ircViewBroadcastChannel view) $ \ signal -> 
      case signal of
        SwitchTranslateLanguage -> do
          -- Update language status
          lang <- readTVarIO $ ircBufferTranslateLanguage $ ircViewBuffer view
          pageViewUpdateStatusbar view "Translate" ("Translate to (" ++ show lang ++ ")")
        BufferChanged -> do
          -- Scroll screen if prompt mark at left of insert mark.
          -- Otherwise don't screen, because user screen download to read old messages.
          let buffer = ircViewBuffer view
              textBuffer = ircBufferBuffer buffer
              ircView = ircViewView view
          readTVarIO (ircBufferScrollMark buffer)
              >?>= \ mark -> do
                  iter   <- textBufferGetIterAtMark textBuffer mark
                  (y, _) <- textViewGetLineYrange ircView iter
                  (Rectangle _ vy _ vh) <- textViewGetVisibleRect ircView
                  when (y >= vy + vh) $ 
                       textViewScrollMarkOnscreen ircView mark

-- | Swtich translate language.
ircViewSwitchTranslateLanguage :: IrcView -> IO ()
ircViewSwitchTranslateLanguage IrcView {ircViewBuffer           = buffer
                                       ,ircViewBroadcastChannel = channel} = do
  let customize = ircBufferCustomize buffer 
  targetLanguage <- readTVarIO (ircCustomizeTargetLanguage customize)
  sourceLanguage <- readTVarIO (ircCustomizeSourceLanguage customize)
  modifyTVarIO (ircBufferTranslateLanguage buffer) 
      $ \lang -> if lang == targetLanguage
                    then sourceLanguage
                    else targetLanguage
  writeTChanIO (viewChannel channel) SwitchTranslateLanguage

-- | Begin.
ircViewScrollToTop :: IrcView -> IO ()
ircViewScrollToTop a = do
  textViewBegin (ircViewView a) (ircViewScrolledWindow a)
  ircViewApplySelectionMark a

-- | End.
ircViewScrollToBottom :: IrcView -> IO ()
ircViewScrollToBottom a = do
  textViewEnd (ircViewView a) (ircViewScrolledWindow a)
  ircViewApplySelectionMark a

-- | Scroll page vertically.
ircViewScrollVerticalPage :: Bool -> IrcView -> IO ()
ircViewScrollVerticalPage isDown a = do
  let sw = ircViewScrolledWindow a
      tv = ircViewView a
  pageInc <- (<=<) adjustmentGetPageIncrement scrolledWindowGetVAdjustment sw
  textViewScrollVertical tv sw (if isDown then pageInc else (- pageInc))
  ircViewApplySelectionMark a

-- | Scroll step vertically.
ircViewScrollVerticalStep :: Bool -> IrcView -> IO ()
ircViewScrollVerticalStep isDown a = do
  let sw = ircViewScrolledWindow a
      tv = ircViewView a
  ti <- textViewGetTextIter tv
  (_, lineHeight) <- textViewGetLineYrange tv ti
  let stepInc = i2d lineHeight
  textViewScrollVertical tv sw (if isDown then stepInc else (- stepInc))
  ircViewApplySelectionMark a

-- | Send morse code.
ircViewSendMorse :: IrcView -> IO ()
ircViewSendMorse view@(IrcView {ircViewBuffer = buffer
                               ,ircViewView   = vView}) = do
  -- Scroll mark on screen visible area.
  let textBuffer = ircBufferBuffer buffer
      promptMark = ircBufferPromptMark buffer
  textViewScrollMarkOnscreen vView promptMark

  -- Get input string.
  startIter <- textBufferGetIterAtMark textBuffer promptMark
  endIter   <- textBufferGetEndIter textBuffer
  text      <- textBufferGetText textBuffer startIter endIter True

  -- Clean input.
  ircViewCleanInput view 

  if isBlankString text
     -- Avoid send blank string, server will ignore blank message.
     then pageViewShowOutputbar view "Ignored blank ..." Nothing
     -- Otherwise send message to server.
     else ircViewSend view (UTF8.fromString $ encodeMorse text)

-- | Send irc message.
ircViewSendMessage :: Bool -> IrcView -> IO ()
ircViewSendMessage isTranslate
                   view@(IrcView {ircViewBuffer = buffer
                                 ,ircViewView   = vView}) = do
  -- Scroll mark on screen visible area.
  let promptMark = ircBufferPromptMark buffer
  textViewScrollMarkOnscreen vView promptMark

  -- Get input string.
  text <- ircViewGetInput view

  -- Clean input.
  ircViewCleanInput view

  if isBlankByteString text
     -- Avoid send blank string, server will ignore blank message.
     then pageViewShowOutputbar view "Ignored blank ..." Nothing
     -- Otherwise send message to server.
     else 
       if isTranslate 
          -- Try get translation.
          then 
            forkGuiIO_ (do
                         pageViewShowOutputbar view "Translation ..." Nothing
                         lang <- readTVarIO $ ircBufferTranslateLanguage buffer
                         translate text Nothing lang)
                       $ \result -> 
                           case result of
                             -- Send original message if translate failed.
                             Left _ -> do
                               pageViewShowOutputbar view "Get translation failed, send original message." Nothing
                               ircViewSend view text
                             -- Otherwise send translation.
                             Right translation -> do
                                 pageViewShowOutputbar view "Translation ... completed." Nothing
                                 ircViewSend view translation
          -- Send original message.
          else ircViewSend view text

-- | Select all.
ircViewSelectAll :: IrcView -> IO ()
ircViewSelectAll = textViewSelectAll . ircViewView

-- | Wrap delete action.
ircViewWrapDeleteAction :: IrcView -> IO Bool -> IO ()
ircViewWrapDeleteAction view action =
  unlessM action $ 
    pageViewShowOutputbar view "Can't delete uneditable area." Nothing

-- | Delete lines.
ircViewDelLines :: IrcView -> IO ()
ircViewDelLines view =
  ircViewWrapDeleteAction view (textViewDelLines (ircViewView view))

-- | Delete.
ircViewDelete :: IrcView -> IO ()
ircViewDelete view = 
  textViewDelete (ircViewView view) True True >> return ()

-- | Delete forward char.
ircViewDeleteForwardChar :: IrcView -> IO ()
ircViewDeleteForwardChar view = 
  ircViewWrapDeleteAction view (textViewDeleteForwardChar (ircViewView view) True)

-- | Backward char.
ircViewDeleteBackwardChar :: IrcView -> IO ()
ircViewDeleteBackwardChar view = 
  ircViewWrapDeleteAction view (textViewDeleteBackwardChar (ircViewView view) True)

-- | Forward word.
ircViewDeleteForwardWord :: IrcView -> IO ()
ircViewDeleteForwardWord view = 
  ircViewWrapDeleteAction view (textViewDeleteForwardWord (ircViewView view) True)

-- | Backward word.
ircViewDeleteBackwardWord :: IrcView -> IO ()
ircViewDeleteBackwardWord view = 
  ircViewWrapDeleteAction view (textViewDeleteBackwardWord (ircViewView view) True)

-- | Delete to line end.
ircViewDeleteToLineEnd :: IrcView -> IO ()
ircViewDeleteToLineEnd view = 
  ircViewWrapDeleteAction view (textViewDeleteToLineEnd (ircViewView view) True)

-- | Delete to line start.
ircViewDeleteToLineStart :: IrcView -> IO ()
ircViewDeleteToLineStart view = 
  ircViewWrapDeleteAction view (textViewDeleteToLineStart (ircViewView view) True)

-- | Cut.
ircViewCut :: IrcView -> IO Bool
ircViewCut view = do
  textViewCut $ ircViewView view
  return True

-- | Copy.
ircViewCopy :: IrcView -> IO Bool
ircViewCopy view = do
  textViewCopy $ ircViewView view
  return True

-- | Paste.
ircViewPaste :: IrcView -> IO Bool
ircViewPaste view = do
  textViewPaste $ ircViewView view
  return True

-- | Forward line.
ircViewForwardLine :: IrcView -> IO ()
ircViewForwardLine a = do
    textViewForwardLine (ircViewView a) (ircViewScrolledWindow a)
    ircViewApplySelectionMark a

-- | Backward line.
ircViewBackwardLine :: IrcView -> IO ()
ircViewBackwardLine a = do
    textViewBackwardLine (ircViewView a) (ircViewScrolledWindow a)
    ircViewApplySelectionMark a

-- | Forward char.
ircViewForwardChar :: IrcView -> IO ()
ircViewForwardChar a = do
    textViewForwardChar (ircViewView a) (ircViewScrolledWindow a)
    ircViewApplySelectionMark a

-- | Backward char.
ircViewBackwardChar :: IrcView -> IO ()
ircViewBackwardChar a = do
    textViewBackwardChar (ircViewView a) (ircViewScrolledWindow a)
    ircViewApplySelectionMark a

-- | Forward word.
ircViewForwardWord :: IrcView -> IO ()
ircViewForwardWord a = do
    textViewForwardWord (ircViewView a) (ircViewScrolledWindow a)
    ircViewApplySelectionMark a

-- | Backward word.
ircViewBackwardWord :: IrcView -> IO ()
ircViewBackwardWord a = do
    textViewBackwardWord (ircViewView a) (ircViewScrolledWindow a)
    ircViewApplySelectionMark a

-- | Smart home.
ircViewSmartHome :: IrcView -> IO ()
ircViewSmartHome a = do
    textViewSmartHome $ ircViewView a
    ircViewApplySelectionMark a

-- | Smart end.
ircViewSmartEnd :: IrcView -> IO ()
ircViewSmartEnd a = do
    textViewSmartEnd $ ircViewView a
    ircViewApplySelectionMark a

-- | Set selection mark.
ircViewToggleSelectionMark :: IrcView -> IO ()
ircViewToggleSelectionMark view = 
  ifM (textViewToggleSelectionMark $ ircViewView view)
      (pageViewUpdateStatusbar view "Selection" "Selection (Active)")
      (pageViewUpdateStatusbar view "Selection" "Selection (Inactive)")

-- | Exchange selection mark.
ircViewExchangeSelectionMark :: IrcView -> IO ()
ircViewExchangeSelectionMark = textViewExchangeSelectionMark . ircViewView 

-- | Show selection mark.
ircViewApplySelectionMark :: IrcView -> IO ()
ircViewApplySelectionMark = textViewApplySelectionMark . ircViewView

-- | Newline.
ircViewNewline :: IrcView -> IO ()  
ircViewNewline = textViewNewLine . ircViewView

-- | Get text buffer.
ircViewGetTextBuffer :: IrcView -> IO TextBuffer 
ircViewGetTextBuffer = textViewGetBuffer . ircViewView

-- | Get source buffer.
ircViewGetSourceBuffer :: IrcView -> IO SourceBuffer
ircViewGetSourceBuffer sb = 
    castToSourceBuffer <$> ircViewGetTextBuffer sb

-- | Move to prompt position.
ircViewMoveToPrompt :: IrcView -> IO ()
ircViewMoveToPrompt IrcView {ircViewBuffer = buffer
                            ,ircViewView   = view} = 
  textViewPlaceCursorWithMark view (ircBufferPromptMark buffer)

-- | Clean input.
ircViewCleanInput :: IrcView -> IO ()
ircViewCleanInput IrcView {ircViewBuffer = buffer} = do
  -- Get iter range.
  let textBuffer = ircBufferBuffer buffer
      promptMark = ircBufferPromptMark buffer
  promptIter <- textBufferGetIterAtMark textBuffer promptMark 
  endIter    <- textBufferGetEndIter textBuffer

  -- Clean input.
  textBufferDelete textBuffer promptIter endIter

-- | Get input message.
ircViewGetInput :: IrcView -> IO ByteString
ircViewGetInput IrcView {ircViewBuffer = buffer} = do
  -- Get iter range.
  let textBuffer = ircBufferBuffer buffer
      promptMark = ircBufferPromptMark buffer
  promptIter <- textBufferGetIterAtMark textBuffer promptMark 
  endIter    <- textBufferGetEndIter textBuffer

  -- Return input message.
  textBufferGetByteString textBuffer promptIter endIter True

-- | Send message.
ircViewSend :: IrcView -> ByteString -> IO ()
ircViewSend view@(IrcView {ircViewBuffer = buffer}) 
            message = do
    nick <- readTVarIO $ ircBufferNick buffer
    -- Push message to buffer.
    ircBufferReceivePrivate buffer (B.pack nick) message
    -- Send message to server.
    let msg = showCommand $ PrivMsgCmd (S.singleton (B.pack $ ircBufferChannel buffer)) message
    mkIrcDaemonSignal (pageViewClient view) SendMessage (SendMessageArgs (ircBufferServer buffer) msg)

-- | Open url around pointer.
ircViewOpenUrl :: IrcView -> IO ()
ircViewOpenUrl view@(IrcView {ircViewBuffer = buffer}) = 
  textBufferGetTagText (ircBufferBuffer buffer) (ircCustomizeUrlColorTag $ ircBufferCustomize buffer)
     >?>= \ url ->        
         mkDaemonSignal (pageViewClient view) NewTab (NewTabArgs "PageBrowser" url [])

-- | Translate message around pointer.
ircViewTranslateMessage :: IrcView -> IO ()
ircViewTranslateMessage view@(IrcView {ircViewBuffer = 
                                           ircBuffer@(IrcBuffer {ircBufferBuffer             = buffer
                                                                ,ircBufferMessageTag         = messageTag
                                                                ,ircBufferTranslateLanguage  = language})}) = do
  -- Use end line iter to match messageTag content (skip message head).
  iter <- textBufferGetCurrentLineEndIter_ buffer
  textIterBackwardChar iter
  textBufferGetTagByteStringWithIter buffer iter messageTag
     >?>= \ text -> 
         forkGuiIO_ (do
                      pageViewShowOutputbar view "Fetch translation ..." Nothing
                      -- Translate reverse. :)
                      lang <- readTVarIO language
                      let customize = ircBufferCustomize ircBuffer
                      targetLanguage <- readTVarIO (ircCustomizeTargetLanguage customize)
                      sourceLanguage <- readTVarIO (ircCustomizeSourceLanguage customize)
                      translate text Nothing (if lang == targetLanguage 
                                              then sourceLanguage
                                              else targetLanguage))
                     $ \result -> 
                         case result of
                           Left _ -> 
                               pageViewShowOutputbar view "Fetch translation ... failed." Nothing
                           Right translation -> do
                               pageViewShowOutputbar view "Fetch translation ... completed." Nothing
                               point <- ircViewGetTooltipPoint view
                               pageViewShowTooltip view (UTF8.toString translation) (Just point)

-- | Translate morse code.
ircViewTranslateMorse :: IrcView -> IO ()
ircViewTranslateMorse view@(IrcView {ircViewBuffer = 
                                                 IrcBuffer {ircBufferBuffer     = buffer
                                                           ,ircBufferMessageTag = messageTag}}) = do
  -- Use end line iter to match messageTag content (skip message head).
  iter <- textBufferGetCurrentLineEndIter_ buffer 
  textIterBackwardChar iter
  textBufferGetTagTextWithIter buffer iter messageTag
       >?>= \ text -> do
           point <- ircViewGetTooltipPoint view
           pageViewShowTooltip view (decodeMorse text) (Just point)
                          
-- | Read message around pointer.
ircViewReadMessage :: IrcView -> IO ()
ircViewReadMessage view@(IrcView {ircViewBuffer = 
                                      IrcBuffer {ircBufferBuffer     = buffer
                                                ,ircBufferMessageTag = messageTag}}) = do
  -- Use end line iter to match messageTag content (skip message head).
  iter <- textBufferGetCurrentLineEndIter_ buffer 
  textIterBackwardChar iter
  textBufferGetTagTextWithIter buffer iter messageTag
       >?>= \ text -> ircViewReadText view text

-- | Read irc message.
ircViewReadText :: IrcView -> String -> IO ()
ircViewReadText view text = 
  execute "festival" ("echo " ++ show text ++ " | %s --tts") True
          >?>= \ failedReason ->
              pageViewShowOutputbar view failedReason Nothing

-- | Get tooltip coordinate.
ircViewGetTooltipPoint :: IrcView -> IO Point
ircViewGetTooltipPoint IrcView {ircViewView = textView} = do
  textIter <- textViewGetTextIter textView
  (Rectangle x y width height) <- textViewGetIterLocation textView textIter
  (wx, wy) <- textViewBufferToWindowCoords textView TextWindowWidget (x, y)
  return (wx + width, wy + height)

-- | Scrolled window.
ircViewScrolledWindow :: IrcView -> ScrolledWindow  
ircViewScrolledWindow =
  pageFrameScrolledWindow . ircViewFrame

-- | Join channel.
ircViewJoinChannel :: IrcView -> IO ()
ircViewJoinChannel view = 
  interactive view [(IString, "Server : ", "irc.freenode.net")
                   ,(INum, "Port : ", "6667")
                   ,(IString, "Channel : ", "#")
                   ,(IString, "Nickname : ", "")] $ \ [server, port, channel, user] -> do
    let info = "irc://" ++ user ++ "@" ++ server ++ ":" ++ port ++ "/" ++ channel
    mkDaemonSignal (pageViewClient view) NewTab (NewTabArgs "PageIrc" info [])

-- | Keymap.
ircViewLocalKeymap :: Map Text Text
ircViewLocalKeymap = 
    M.fromList [("Return",   "Send original message")
               ,("M-m",      "Send original message")
               ,("C-m",      "Send translate message")
               ,("C-n",      "Switch translate language")
               ,("C-N",      "Send morse code")
               ,("M-M",      "Move to prompt")
               ,("M-N",      "Clean input")
               ,("M-a",      "Select all")
               ,("M-d",      "Delete lines")
               ,("M-D",      "Delete")
               ,("M-,",      "Delete backward char")
               ,("M-.",      "Delete forward char")
               ,("M-<",      "Delete backward word")
               ,("M->",      "Delete forward word")
               ,("M-C-,",    "Delete to line start")
               ,("M-C-.",    "Delete to line end")
               ,("M-j",      "Forward line")
               ,("M-k",      "Backward line")
               ,("M-l",      "Forward char")
               ,("M-h",      "Backward char")
               ,("Down",     "Forward line")
               ,("Up",       "Backward line")
               ,("Right",    "Forward char")
               ,("Left",     "Backward char")
               ,("S-Return", "New line")
               ,("M-L",      "Forward word")
               ,("M-H",      "Backward word")
               ,("M-P-h",    "Smart home")
               ,("M-P-l",    "Smart end")
               ,("C-c",      "Toggle selection mark")
               ,("C-C",      "Exchange selection mark")
               ,("C-o",      "Open url around point")
               ,("C-,",      "Translate message")
               ,("C-.",      "Translate morse")
               ,("C-k",      "Read message")
               ,("C-j",      "Join channel")
               ]

-- | Keymap.
ircViewLocalCommandMap :: Map Text (IrcView -> IO ())
ircViewLocalCommandMap = 
    M.fromList [("Send original message",       ircViewSendMessage False)
               ,("Send translate message",      ircViewSendMessage True)
               ,("Switch translate language",   ircViewSwitchTranslateLanguage)
               ,("Send morse code",             ircViewSendMorse)
               ,("Move to prompt",              ircViewMoveToPrompt)
               ,("Clean input",                 ircViewCleanInput)
               ,("Select all",                  ircViewSelectAll)
               ,("Delete lines",                ircViewDelLines)
               ,("Delete",                      ircViewDelete)
               ,("Delete backward char",        ircViewDeleteBackwardChar)
               ,("Delete forward char",         ircViewDeleteForwardChar)
               ,("Delete backward word",        ircViewDeleteBackwardWord)
               ,("Delete forward word",         ircViewDeleteForwardWord)
               ,("Delete to line start",        ircViewDeleteToLineStart)
               ,("Delete to line end",          ircViewDeleteToLineEnd)
               ,("Forward line",                ircViewForwardLine)
               ,("Backward line",               ircViewBackwardLine)
               ,("Forward char",                ircViewForwardChar)
               ,("Backward char",               ircViewBackwardChar)
               ,("New line",                    ircViewNewline)
               ,("Forward word",                ircViewForwardWord)
               ,("Backward word",               ircViewBackwardWord)
               ,("Smart home",                  ircViewSmartHome)
               ,("Smart end",                   ircViewSmartEnd)
               ,("Toggle selection mark",       ircViewToggleSelectionMark)
               ,("Exchange selection mark",     ircViewExchangeSelectionMark)
               ,("Open url around point",       ircViewOpenUrl)
               ,("Translate message",           ircViewTranslateMessage)
               ,("Translate morse",             ircViewTranslateMorse)
               ,("Read message",                ircViewReadMessage)
               ,("Join channel",                ircViewJoinChannel)
               ]

-- | Save state.
ircViewSaveState :: IrcView -> Maybe FilePath -> IO ()
ircViewSaveState view@(IrcView {ircViewBuffer  = buffer})  
                 statePath = do
  -- Get scroll position.
  scrolledWindowPosition <- textViewGetCursorAlign (ircViewView view)

  -- Save state.
  let state = IrcState Nothing scrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (ircBufferState buffer) state
    Just path -> writeConfigPath path state

-- | Restore state.
ircViewRestoreState :: IrcView -> Maybe FilePath -> IO ()
ircViewRestoreState IrcView {ircViewBuffer  = buffer
                            ,ircViewView    = textView} 
                    statePath = do
  bufferState <- readTVarIO (ircBufferState buffer)
  (IrcState cursor scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore cursor.
  cursor ?>= \ (line, column) -> do
    textViewGotoLine textView line
    textViewGotoColumn textView column

  -- Restore cursor alignment.
  textViewSetCursorAlign textView scrolledWindowPosition

