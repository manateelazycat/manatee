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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Extension.IrcClient.IrcBuffer where

import Config.Import
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import DBus.Client hiding (Signal)
import Data.Array
import Data.Binary
import Data.DeriveTH
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Typeable
import Graphics.UI.Gtk hiding (Language, eventButton, eventClick, get)
import Graphics.UI.Gtk.Gdk.Events
import Graphics.UI.Gtk.SourceView.SourceBuffer
import Language.Translate.Google
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.Dynload
import Manatee.Core.Types
import Manatee.Extension.IrcClient.DBus
import Manatee.Extension.IrcClient.HighlightNick
import Manatee.Extension.IrcClient.PageMode
import Manatee.Extension.IrcClient.Smile
import Manatee.Extension.IrcClient.Types
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.ByteString
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.Set
import Manatee.Toolkit.General.Time
import Manatee.Toolkit.Gtk.Multiline
import Network.FastIRC.Types
import Network.URI
import System.Posix.Process
import Text.Regex.Posix hiding (after)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Graphics.UI.Gtk as Gtk

data IrcBuffer = 
    IrcBuffer {ircBufferServer            :: Server
              ,ircBufferPort              :: Port
              ,ircBufferChannel           :: Channel
              ,ircBufferClient            :: Client
              ,ircBufferPageId            :: PageId
              ,ircBufferMode              :: PageMode
              ,ircBufferBuffer            :: SourceBuffer
              ,ircBufferInsertMark        :: TextMark
              ,ircBufferPromptMark        :: TextMark
              ,ircBufferScrollMark        :: TVar (Maybe TextMark)
              ,ircBufferEditableTag       :: TextTag
              ,ircBufferMessageTag        :: TextTag
              ,ircBufferTimeStamp         :: TVar (Map Int (String, Color))
              ,ircBufferNick              :: TVar Nick
              ,ircBufferNickSet           :: TVar (Set NickName)
              ,ircBufferNickColorMap      :: TVar (Map NickName TextTag)
              ,ircBufferTranslateLanguage :: TVar Language
              ,ircBufferBroadcastChannel  :: TChan IrcBufferSignal
              ,ircBufferSmilePixbufs      :: Map String Pixbuf
              ,ircBufferCustomize         :: IrcCustomize
              ,ircBufferState             :: TVar IrcState
              }
    deriving Typeable

data IrcBufferSignal = SwitchTranslateLanguage 
                     | BufferChanged
                       deriving (Show, Eq, Ord)

data IrcState =
    IrcState {ircStateCursor              :: Maybe (Int, Int)
             ,ircStateScrolledPosition    :: (Double, Double)}

-- | Init state.
ircInitState :: IrcState
ircInitState =
    IrcState (Just (1, 0)) (0, 0)

-- | Create buffer new.
ircBufferNew :: String -> [String] -> Client -> PageId -> CustomizeWrap -> IO IrcBuffer
ircBufferNew info _ client pageId c = do
  -- Get customize option.
  let customize = castCustomize c

  -- Get server port channel information.
  (mynick, server, port, channel) <- ircParseInfo info customize
  putStrLn ("Buffer create (server, port, channel) : " ++ show (server, port, channel))

  -- Create source buffer and text mark.
  sourceBuffer <- sourceBufferNew Nothing
  ircBufferInsertPromptStr sourceBuffer
  insertMark <- ircBufferCreateInsertMark sourceBuffer
  promptMark <- ircBufferCreatePromptMark sourceBuffer
  scrollMark <- newTVarIO Nothing

  -- Create editable tag.
  textTag <- textTagNew Nothing
  set textTag [textTagEditable := False]
  tagTable <- textBufferGetTagTable sourceBuffer
  textTagTableAdd tagTable textTag

  -- Create message tag.
  messageTag <- textTagNew Nothing
  set messageTag [textTagEditable := False]
  textTagTableAdd tagTable messageTag

  -- Create color tag.
  ircBufferAddColorTag sourceBuffer (ircCustomizeServerColorTag customize)
  ircBufferAddColorTag sourceBuffer (ircCustomizeSelfColorTag customize)
  ircBufferAddColorTag sourceBuffer (ircCustomizeOtherColorTag customize)
  ircBufferAddColorTag sourceBuffer (ircCustomizeJoinColorTag customize)
  ircBufferAddColorTag sourceBuffer (ircCustomizeQuitColorTag customize)
  ircBufferAddColorTag sourceBuffer (ircCustomizePartColorTag customize)
  ircBufferAddColorTag sourceBuffer (ircCustomizeActionColorTag customize)
  ircBufferAddColorTag sourceBuffer (ircCustomizeUrlColorTag customize)
  let urlColorTag = ircCustomizeUrlColorTag customize
  set urlColorTag [textTagUnderline := UnderlineSingle]

  -- Open url when click.
  urlColorTag `onTextTagEvent` \ event iter -> 
      case event of
        Button {eventClick  = click
               ,eventButton = button} -> 
          when (button == LeftButton && click == SingleClick) $ 
            textBufferGetTagTextWithIter sourceBuffer iter urlColorTag 
              >?>= \ url ->
                mkDaemonSignal client NewTab (NewTabArgs "PageBrowser" url [])
        _ -> return ()

  -- Apply text tag.
  startIter <- textBufferGetStartIter sourceBuffer
  endIter   <- textBufferGetIterAtMark sourceBuffer promptMark
  textBufferApplyTag sourceBuffer textTag startIter endIter

  -- Time stamp.
  timeStamp <- newTVarIO M.empty

  -- Nick set.
  nick <- newTVarIO mynick

  -- Nick set.
  nickSet <- newTVarIO S.empty

  -- Nick color map.
  nickColorMap <- newTVarIO M.empty

  -- Target language.
  targetLanguage <- readTVarIO (ircCustomizeTargetLanguage customize)
  lang <- newTVarIO targetLanguage

  -- Broadcast channel.
  broadcastChannel <- newTChanIO :: IO (TChan IrcBufferSignal)

  -- Get smile pixbufs.
  smilePixbufs <- createSmilePixbufs

  -- Init state.
  state <- newTVarIO ircInitState

  -- Create buffer.
  let buffer = IrcBuffer server port channel
                         client pageId ircMode
                         sourceBuffer insertMark promptMark scrollMark 
                         textTag messageTag 
                         timeStamp nick nickSet nickColorMap lang
                         broadcastChannel smilePixbufs customize state

  -- Add self in nick map.
  ircBufferAddNick buffer (B.pack mynick)


  -- Build DBus match rule.
  processId <- getProcessID
  mkIrcClientMatchRules client
    [(ReceivePrivate,
      \ (ReceivePrivateArgs nick msg) -> 
          ircBufferReceivePrivate buffer nick msg)
    ,(ReceiveJoin,
     \ (ReceiveJoinArgs nick user host) ->
          ircBufferReceiveJoin buffer nick user host)
    ,(ReceiveTopicReply,
      \ (ReceiveTopicReplyArgs msg) ->
          ircBufferReceiveTopicReply buffer msg)
    ,(ReceiveTopicWhoTime,
      \ (ReceiveTopicWhoTimeArgs nick seconds) ->
          ircBufferReceiveTopicWhoTime buffer nick seconds)
    ,(ReceiveChannelUrl,
      \ (ReceiveChannelUrlArgs url) ->
          ircBufferReceiveChannelUrl buffer url)
    ,(ReceiveNames,
      \ (ReceiveNamesArgs nicks) -> 
          ircBufferReceiveNames buffer nicks)
    ,(ReceiveQuit,
      \ (ReceiveQuitArgs nick user host reason) ->
          ircBufferReceiveQuit buffer nick user host reason)
    ,(ReceivePart,
      \ (ReceivePartArgs nick user host reason) ->
          ircBufferReceivePart buffer nick user host reason)
    ,(DaemonProcessStartup,
     \_ -> mkIrcDaemonSignal client Join (JoinArgs server port channel mynick processId))]
    
  -- Check whether daemon process has startup.
  ifM (isBusNameExist ircDaemonBusName)
      -- Send join signal if daemon process has startup.
      (mkIrcDaemonSignal client Join (JoinArgs server port channel mynick processId))
      -- Otherwise startup daemon process, send join signal after daemon process startup complete.
      (do
         putStrLn "No irc daemon process, starting one."
         runProcess_ "manatee-irc-daemon" [show processId])

  -- Add function when main loop left.
  -- Send part signal to daemon process if tab closed.
  quitAdd 0 (do
              putStrLn $ "Irc process "  ++ show processId ++ " quit."
              mkIrcDaemonSignal client Part (PartArgs server channel processId)
              return False)

  -- Record scroll mark before change buffer.
  -- Then we can restore scroll mark position after buffer changed.
  sourceBuffer `on` bufferChanged $ do
    cursorMark <- textBufferGetInsert sourceBuffer
    promptIter <- textBufferGetIterAtMark sourceBuffer promptMark
    insertIter <- textBufferGetInsertIter sourceBuffer
    order <- textIterCompare insertIter promptIter
    case order of
      GT -> writeTVarIO scrollMark (Just cursorMark)
      EQ -> writeTVarIO scrollMark (Just cursorMark)
      _  -> writeTVarIO scrollMark Nothing

  -- Broadcast signal when buffer changed.
  sourceBuffer `after` bufferChanged $
     writeTChanIO broadcastChannel BufferChanged 


  return buffer

-- | Parse "irc:[//[user@][server[:port]][/]][#channel]".
ircParseInfo :: String -> IrcCustomize -> IO (Nick, Server, Port, Channel)
ircParseInfo str (IrcCustomize {ircCustomizeDefaultNick         = nickTVar
                               ,ircCustomizeDefaultServer       = serverTVar
                               ,ircCustomizeDefaultPort         = portTVar
                               ,ircCustomizeDefaultChannel      = channelTVar
                               }) = do
    defaultNick     <- readTVarIO nickTVar
    defaultServer   <- readTVarIO serverTVar
    defaultPort     <- readTVarIO portTVar
    defaultChannel  <- readTVarIO channelTVar
    return $ 
           if null matchTextList
              then (defaultNick, defaultServer, defaultPort, defaultChannel)
              else 
                  let (_ : (nickStr, _) : (serverStr, _) : (portStr, _) : (channelStr, _) : _) = elems $ head matchTextList
                      nick = if null nickStr then defaultNick else init nickStr
                      server = if null serverStr then defaultServer else serverStr
                      port = if null portStr then defaultPort else read $ tail portStr :: Int
                      channel = (\x -> case x of
                                        ('/':c) -> c 
                                        _ -> x
                                ) $ if null channelStr then defaultChannel else channelStr
                  in (nick, server, port, channel)
    where matchTextList =
              matchAllText 
                  (makeRegex ("irc://([^<> \t\n'@:]+@)*([a-zA-Z.]+)*(:[0-9]+)*(/*[$#+!].[^ \t\n,]+)*" :: String) :: Regex) 
                  str
          

-- | Receive private message.
ircBufferReceivePrivate :: IrcBuffer -> NickName -> CommandArg -> IO ()
ircBufferReceivePrivate buffer@(IrcBuffer {ircBufferCustomize = 
                                            IrcCustomize 
                                            {ircCustomizeSelfColorTag     = selfColorTag
                                            ,ircCustomizeOtherColorTag    = otherColorTag
                                            ,ircCustomizeActionColorTag   = actionColorTag}
                                          ,ircBufferNick = mynick}) 
                        nick content = do
  -- Format private message.
  ((msg, msgHeadLen), isAction) <-
        if B.pack "\SOHACTION " `B.isPrefixOf` content 
               && B.pack "\SOH" `B.isSuffixOf` content
           then do
               let prefixLen  = B.length (B.pack "\SOHACTION ")
                   suffixLen  = B.length (B.pack "\SOH")
                   takeLen    = B.length content - prefixLen - suffixLen
                   newContent = B.take takeLen $ B.drop prefixLen content
               args <- ircBufferIndentMessage buffer newContent (B.concat ["* ", nick, " "])
               return (args, True)
           else do 
               args <- ircBufferIndentMessage buffer content (B.concat ["<", nick, "> "]) 
               return (args, False)

  -- Get color with the type of private message.
  mynickname <- readTVarIO mynick
  let messageColor 
          -- Use selfColorTag when it's self message.
          | B.unpack nick == mynickname
              = selfColorTag
          -- User actionColorTag when print action message.
          | isAction 
              = actionColorTag
          -- Otherwise use otherColorTag.
          | otherwise 
              = otherColorTag

  -- Insert private message.
  ircBufferReceiveMsg buffer msg msgHeadLen messageColor

-- | Receive quit message.
ircBufferReceiveQuit :: IrcBuffer -> NickName -> UserName -> HostName -> B.ByteString -> IO ()
ircBufferReceiveQuit buffer@(IrcBuffer {ircBufferNickSet = nickSet
                                       ,ircBufferCustomize =
                                         IrcCustomize 
                                         {ircCustomizeQuitColorTag = quitColorTag}}) 
                     nick user host reason = do
  -- Quit nick map.
  set <- readTVarIO nickSet
  case maybeFindMin set (== nick) of
    Just _ -> do
      -- Remove from set.
      modifyTVarIO nickSet $ \ set ->
          S.delete nick set

      -- Format quit message.
      let adjustMsg = B.pack "*** "
          content   = B.concat [nick, " (", user, "@", host, ") has quit: ", reason]
      (msg, msgHeadLen) <- ircBufferIndentMessage buffer content adjustMsg
      
      -- Insert quit message.
      ircBufferReceiveMsg buffer msg msgHeadLen quitColorTag
    Nothing -> return ()

-- | Receive part message.
ircBufferReceivePart :: IrcBuffer -> NickName -> UserName -> HostName -> B.ByteString -> IO ()
ircBufferReceivePart buffer@(IrcBuffer {ircBufferNickSet        = nickSet
                                       ,ircBufferCustomize =
                                         IrcCustomize 
                                         {ircCustomizePartColorTag   = partColorTag}}) 
                     nick user host reason = do
  -- Remove from set.
  modifyTVarIO nickSet $ \ set ->
      S.delete nick set

  -- Format part message.
  let adjustMsg = B.pack "*** "
      content   = B.concat [nick, " (", user, "@", host, ") has part: ", reason]
  (msg, msgHeadLen) <- ircBufferIndentMessage buffer content adjustMsg
  
  -- Insert part message.
  ircBufferReceiveMsg buffer msg msgHeadLen partColorTag

-- | Receive topic replay message.
ircBufferReceiveNames :: IrcBuffer -> B.ByteString -> IO ()
ircBufferReceiveNames buffer nicks = 
  -- Add nick to color map.
  forM_ (B.words nicks) $ \nick -> 
    ircBufferAddNick buffer nick

-- | Receive topic replay message.
ircBufferReceiveChannelUrl :: IrcBuffer -> B.ByteString -> IO ()
ircBufferReceiveChannelUrl buffer@(IrcBuffer {ircBufferChannel          = channel
                                             ,ircBufferCustomize =
                                               IrcCustomize 
                                               {ircCustomizeServerColorTag   = serverColorTag}})
                           url = do
  -- Format channel url message.
  let adjustMsg = B.pack "*** "
      content   = B.concat [B.pack channel, " URL: ", url]
  (msg, msgHeadLen) <- ircBufferIndentMessage buffer content adjustMsg
      
  -- Insert channel url message.
  ircBufferReceiveMsg buffer msg msgHeadLen serverColorTag

-- | Receive topic replay message.
ircBufferReceiveTopicReply :: IrcBuffer -> CommandArg -> IO ()
ircBufferReceiveTopicReply buffer@(IrcBuffer {ircBufferChannel          = channel
                                             ,ircBufferCustomize =
                                               IrcCustomize 
                                               {ircCustomizeServerColorTag   = serverColorTag}})
                           message = do
  -- Format topic replay message.
  let adjustMsg = B.pack "*** "
      content   = B.concat ["Topic for ", B.pack channel, ": ", message]
  (msg, msgHeadLen) <- ircBufferIndentMessage buffer content adjustMsg
      
  -- Insert topic replay message.
  ircBufferReceiveMsg buffer msg msgHeadLen serverColorTag

-- | Receive topic replay message.
ircBufferReceiveTopicWhoTime :: IrcBuffer -> NickName -> Integer -> IO ()
ircBufferReceiveTopicWhoTime buffer@(IrcBuffer {ircBufferChannel        = channel
                                               ,ircBufferCustomize =
                                                 IrcCustomize 
                                                 {ircCustomizeServerColorTag = serverColorTag}})
                                nick seconds = do
  -- Format topic who time message.
  time <- getSecondsTimeStamp seconds "%H:%M:%S %Y/%m/%d"
  let adjustMsg = B.pack "*** "
      content   = B.concat [B.pack channel, ": topic set by ", nick, ", ", B.pack time]
  (msg, msgHeadLen) <- ircBufferIndentMessage buffer content adjustMsg
      
  -- Insert topic who time message.
  ircBufferReceiveMsg buffer msg msgHeadLen serverColorTag

-- | Receive join message.
ircBufferReceiveJoin :: IrcBuffer -> NickName -> UserName -> HostName -> IO ()
ircBufferReceiveJoin buffer@(IrcBuffer {ircBufferChannel        = channel
                                       ,ircBufferNick           = mynick
                                       ,ircBufferCustomize =
                                         IrcCustomize 
                                         {ircCustomizeJoinColorTag   = joinColorTag}
                                       }) 
                        nick user host = do
  nickname <- readTVarIO mynick
  -- Don't print self join message.
  when (B.unpack nick /= nickname) $ do
    -- Add nick to set.
    ircBufferAddNick buffer nick

    -- Format join message.
    let adjustMsg = B.pack "*** " 
        content   = B.concat [nick, " (", user, "@", host, ") has joined channel ", B.pack channel]
    (msg, msgHeadLen) <- ircBufferIndentMessage buffer content adjustMsg 

    -- Insert join message.
    ircBufferReceiveMsg buffer msg msgHeadLen joinColorTag
  
-- | Receive irc message.
ircBufferReceiveMsg :: IrcBuffer -> B.ByteString -> Int -> TextTag -> IO ()
ircBufferReceiveMsg IrcBuffer {ircBufferBuffer       = buffer
                              ,ircBufferInsertMark   = insertMark
                              ,ircBufferPromptMark   = promptMark
                              ,ircBufferEditableTag  = tag
                              ,ircBufferMessageTag   = messageTag
                              ,ircBufferTimeStamp    = stamp
                              ,ircBufferNickColorMap = nickColorMap
                              ,ircBufferSmilePixbufs = smilePixbufs
                              ,ircBufferCustomize =
                                IrcCustomize 
                                {ircCustomizeUrlColorTag  = urlColorTag}
                              } 
                    message msgHeadLen colorTag = do
  -- Make editable tag can edit temporary for insert irc message.
  set tag [textTagEditable := True]

  -- Update time stamp before insert message.
  insertIter<- textBufferGetIterAtMark buffer insertMark
  line      <- textIterGetLine insertIter
  timeStamp <- getTimeStamp "[%H:%M:%S]"
  color <- Gtk.get colorTag textTagForegroundGdk
  modifyTVarIO stamp $ \s -> 
      M.insert line (timeStamp, color) s

  -- Create new mark to save insert iter position.
  -- To apply color tag after insert message.
  saveMark <- textBufferCreateMark buffer Nothing insertIter True

  -- Filter smile string from message.
  (msg, smileIndex) <- smileMessage message smilePixbufs

  -- Insert irc message before insertMark.
  textBufferInsertByteString buffer insertIter msg

  -- Apply base color tag.
  saveIter   <- textBufferGetIterAtMark buffer saveMark
  insertIter <- textBufferGetIterAtMark buffer insertMark
  textBufferApplyTag buffer colorTag saveIter insertIter

  -- Apply nick color tag.
  colorMap <- readTVarIO nickColorMap
  unless (null $ map fst $ M.toList colorMap) $ 
    -- Regex "[^<> \t\n\'@,.:]+" works fine,
    -- don't build regex match all nickname, 
    -- too big regex will make matchAllText *failed*.
    forM_ (matchAllText (makeRegex (B.pack "[^<> \t\n\'@,.:]+") :: Regex) msg)
        $ \ x -> do
          let ((word, (matchOffset, matchLength)) : _) = elems x
          case findMinMatch colorMap (\ nickName _ -> nickName == word) of
            Just (_, colorTag) -> do
                  matchStartIter <- textIterCopy saveIter
                  matchEndIter   <- textIterCopy saveIter
                  textIterForwardChars matchStartIter matchOffset
                  textIterForwardChars matchEndIter (matchOffset + matchLength)
                  textBufferApplyTag buffer colorTag matchStartIter matchEndIter
            Nothing -> return ()

  -- Apply URL color tag.
  forM_ (matchAllText (makeRegex (B.pack "[^]\\ \t\n`\\[\"^]+") :: Regex) msg)
      $ \ x -> do
        let ((word, (matchOffset, matchLength)) : _) = elems x
        parseURI (UTF8.toString word) ?>= \ uri -> 
          when (isJust $ uriAuthority uri) $ do 
            matchStartIter <- textIterCopy saveIter
            matchEndIter   <- textIterCopy saveIter
            textIterForwardChars matchStartIter matchOffset
            textIterForwardChars matchEndIter (matchOffset + matchLength)
            textBufferApplyTag buffer urlColorTag matchStartIter matchEndIter

  -- Insert smile pixbuf.
  forM_ smileIndex $ \ (offset, pixbuf) -> do
    iter <- textBufferGetIterAtMark buffer saveMark
    textIterForwardChars iter offset
    textBufferInsertPixbuf buffer iter pixbuf

  -- We should apply message tag after all INSERT action.
  -- Otherwise will get rang bound.
  msgStartIter <- textBufferGetIterAtMark buffer saveMark
  msgEndIter   <- textBufferGetIterAtMark buffer insertMark
  textIterForwardChars msgStartIter msgHeadLen -- skip message head (e.g. nick name)
  textIterBackwardChar msgEndIter              -- skip '\n' at last
  textBufferApplyTag buffer messageTag msgStartIter msgEndIter

  -- Apply editable tag to new rang (from start to insertMark).
  startIter <- textBufferGetStartIter buffer
  endIter   <- textBufferGetIterAtMark buffer promptMark
  textBufferApplyTag buffer tag startIter endIter

  -- Make irc message read-only again.
  set tag [textTagEditable := False]

-- | Add nick to nick set.
ircBufferAddNick :: IrcBuffer -> NickName -> IO ()
ircBufferAddNick buffer@(IrcBuffer {ircBufferNickSet = nickSet})
                 nick = do
    -- Add nick to list.
    set <- readTVarIO nickSet
    case maybeFindMin set (== nick) of
      Just _  -> 
          -- Debug.
          -- putStrLn $ "Nick " ++ B.unpack nick ++ " already in nick set."
          return ()
      Nothing -> 
        -- Debug.
        -- putStrLn $ "Nick " ++ B.unpack nick ++ " added to nick set."
        modifyTVarIO nickSet $ \ set -> 
            S.insert nick set

    -- Update nick color.
    ircBufferUpdateNickColor buffer nick

-- | Update nick color to  map.
ircBufferUpdateNickColor :: IrcBuffer -> NickName -> IO ()
ircBufferUpdateNickColor (IrcBuffer {ircBufferBuffer       = buffer
                                    ,ircBufferNickColorMap = nickColorMap})
                         nick = do
  colorMap <- readTVarIO nickColorMap
  case findMinMatch colorMap (\ nickName _ -> nickName == nick) of
    Just _  -> 
      -- Debug
      -- putStrLn $ "Have build color for nick : " ++ B.unpack nick
      return ()
    Nothing -> do
      -- Get color nick.
      let color = nickColorToColor $ nickColor nick

      -- Create color tag.
      textTag <- textTagNew Nothing
      set textTag [textTagForegroundGdk := color
                  ,textTagWeight        := fromEnum WeightBold]

      -- Add color tag to table.
      tagTable <- textBufferGetTagTable buffer
      textTagTableAdd tagTable textTag

      -- Update color map.
      modifyTVarIO nickColorMap $ \map ->
          M.insert nick textTag map

-- | Add color tag.
ircBufferAddColorTag :: TextBufferClass buffer => buffer -> TextTag -> IO ()
ircBufferAddColorTag buffer colorTag = do
  tagTable <- textBufferGetTagTable buffer
  textTagTableAdd tagTable colorTag
      
-- | Insert prompt string.
ircBufferInsertPromptStr :: TextBufferClass buffer => buffer -> IO ()
ircBufferInsertPromptStr buffer = 
    textBufferSetText buffer promptStr

-- | Create insert mark.
ircBufferCreateInsertMark :: TextBufferClass buffer => buffer -> IO TextMark
ircBufferCreateInsertMark buffer = do
  textIter <- textBufferGetStartIter buffer
  textBufferCreateMark buffer Nothing textIter False

-- | Create prompt mark.
ircBufferCreatePromptMark :: TextBufferClass buffer => buffer -> IO TextMark
ircBufferCreatePromptMark buffer = do
  textIter <- textBufferGetIterAtOffset buffer (length promptStr)
  textBufferCreateMark buffer Nothing textIter True
          
-- | Indent message.
ircBufferIndentMessage :: IrcBuffer -> B.ByteString -> B.ByteString -> IO (B.ByteString, Int)
ircBufferIndentMessage buffer content adjustMsg = do
  wrapColumn <- readTVarIO (ircCustomizeWrapColumn $ ircBufferCustomize buffer)                          
  let indentMsg = B.replicate (B.length adjustMsg) ' '
      msgLines  = map B.unwords $ wrapLine (wrapColumn - B.length adjustMsg) (splitWords content)
      concatMsg []     = B.concat [adjustMsg, "\n"]
      concatMsg [x]    = B.concat [adjustMsg, x, "\n"]
      concatMsg (x:xs) = B.concat [B.concat [adjustMsg, x, "\n"]
                                  ,B.concat $ map (\ xx -> B.concat [indentMsg, xx, "\n"]) xs]

  return (concatMsg msgLines, B.length adjustMsg)

-- | Irc customize new.
ircCustomizeNew :: IO CustomizeWrap
ircCustomizeNew =
  fmap CustomizeWrap $ 
       IrcCustomize <$> newTVarIO selfMsgColor
                    <*> newTVarIO serverMsgColor
                    <*> newTVarIO otherMsgColor
                    <*> newTVarIO joinMsgColor
                    <*> newTVarIO quitMsgColor
                    <*> newTVarIO partMsgColor
                    <*> newTVarIO actionMsgColor
                    <*> newTVarIO urlColor
                    <*> newTVarIO wrapColumn
                    <*> ircColorTagNew serverMsgColor
                    <*> ircColorTagNew selfMsgColor
                    <*> ircColorTagNew otherMsgColor
                    <*> ircColorTagNew joinMsgColor
                    <*> ircColorTagNew quitMsgColor
                    <*> ircColorTagNew partMsgColor
                    <*> ircColorTagNew actionMsgColor
                    <*> ircColorTagNew urlColor
                    <*> newTVarIO timeStampPosition
                    <*> newTVarIO sourceLanguage
                    <*> newTVarIO targetLanguage
                    <*> newTVarIO showLineNumber
                    <*> newTVarIO defaultNick
                    <*> newTVarIO defaultServer
                    <*> newTVarIO defaultPort
                    <*> newTVarIO defaultChannel

-- | New color tag.
ircColorTagNew :: Color -> IO TextTag
ircColorTagNew color = do
  colorTag <- textTagNew Nothing
  set colorTag [textTagForegroundGdk := color]
  return colorTag

-- | Write state.
ircBufferWriteState :: IrcBuffer -> FilePath -> IO ()
ircBufferWriteState buffer path = do
  state <- readTVarIO $ ircBufferState buffer
  writeConfigPath path state

-- | Read state.
ircBufferReadState :: IrcBuffer -> FilePath -> IO ()  
ircBufferReadState buffer path = do
  state <- readConfigPath path ircInitState
  writeTVarIO (ircBufferState buffer) state
  
$(derive makeBinary ''IrcState)

