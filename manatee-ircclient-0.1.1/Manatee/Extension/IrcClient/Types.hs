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
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
module Manatee.Extension.IrcClient.Types where

import Control.Concurrent.STM
import Data.Typeable
import GHC
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get, Language)
import Language.Translate.Google
import Manatee.Core.Types
import Manatee.Toolkit.General.STM
import Unsafe.Coerce

import qualified Data.ByteString.Char8 as B

type Server     = String
type Channel    = String
type Port       = Int
type Nick       = String
type Password   = String

data IrcCustomize = 
  IrcCustomize {ircCustomizeSelfMsgColor        :: TVar Color
               ,ircCustomizeServerMsgColor      :: TVar Color
               ,ircCustomizeOtherMsgColor       :: TVar Color
               ,ircCustomizeJoinMsgColor        :: TVar Color
               ,ircCustomizeQuitMsgColor        :: TVar Color
               ,ircCustomizePartMsgColor        :: TVar Color
               ,ircCustomizeActionMsgColor      :: TVar Color
               ,ircCustomizeUrlColor            :: TVar Color
               ,ircCustomizeWrapColumn          :: TVar Int
               ,ircCustomizeServerColorTag      :: TextTag
               ,ircCustomizeSelfColorTag        :: TextTag
               ,ircCustomizeOtherColorTag       :: TextTag
               ,ircCustomizeJoinColorTag        :: TextTag
               ,ircCustomizeQuitColorTag        :: TextTag
               ,ircCustomizePartColorTag        :: TextTag
               ,ircCustomizeActionColorTag      :: TextTag
               ,ircCustomizeUrlColorTag         :: TextTag
               ,ircCustomizeTimeStampPosition   :: TVar TextWindowType
               ,ircCustomizeSourceLanguage      :: TVar Language
               ,ircCustomizeTargetLanguage      :: TVar Language
               ,ircCustomizeShowLineNumber      :: TVar Bool
               ,ircCustomizeDefaultNick         :: TVar String
               ,ircCustomizeDefaultServer       :: TVar String
               ,ircCustomizeDefaultPort         :: TVar Int
               ,ircCustomizeDefaultChannel      :: TVar String
               }
  deriving Typeable

instance Customize IrcCustomize where
  customizeConfigFile _ = "IrcClient.hs"
  customizeLoad a       = 
      [("selfMsgColor"
        ,\ v -> ircUpdateColorTag v (ircCustomizeSelfMsgColor a) (ircCustomizeSelfColorTag a))
      ,("serverMsgColor"
        ,\ v -> ircUpdateColorTag v (ircCustomizeServerMsgColor a) (ircCustomizeServerColorTag a))
      ,("otherMsgColor"
        ,\ v -> ircUpdateColorTag v (ircCustomizeOtherMsgColor a) (ircCustomizeOtherColorTag a))
      ,("joinMsgColor"
        ,\ v -> ircUpdateColorTag v (ircCustomizeJoinMsgColor a) (ircCustomizeJoinColorTag a))
      ,("quitMsgColor"
        ,\ v -> ircUpdateColorTag v (ircCustomizeQuitMsgColor a) (ircCustomizeQuitColorTag a))
      ,("partMsgColor"
        ,\ v -> ircUpdateColorTag v (ircCustomizePartMsgColor a) (ircCustomizePartColorTag a))
      ,("actionMsgColor"
        ,\ v -> ircUpdateColorTag v (ircCustomizeActionMsgColor a) (ircCustomizeActionColorTag a))
      ,("urlColor"
        ,\ v -> ircUpdateColorTag v (ircCustomizeUrlColor a) (ircCustomizeUrlColorTag a))
      ,("wrapColumn"
        ,\ v -> writeTVarIO (ircCustomizeWrapColumn a) (unsafeCoerce v :: Int))
      ,("timeStampPosition"
        ,\ v -> writeTVarIO (ircCustomizeTimeStampPosition a) (unsafeCoerce v :: TextWindowType))
      ,("sourceLanguage"
        ,\ v -> writeTVarIO (ircCustomizeSourceLanguage a) (unsafeCoerce v :: Language))
      ,("targetLanguage"
        ,\ v -> writeTVarIO (ircCustomizeTargetLanguage a) (unsafeCoerce v :: Language))
      ,("showLineNumber"
        ,\ v -> writeTVarIO (ircCustomizeShowLineNumber a) (unsafeCoerce v :: Bool))
      ,("defaultNick"
        ,\ v -> writeTVarIO (ircCustomizeDefaultNick a) (unsafeCoerce v :: String))
      ,("defaultServer"
        ,\ v -> writeTVarIO (ircCustomizeDefaultServer a) (unsafeCoerce v :: String))
      ,("defaultPort"
        ,\ v -> writeTVarIO (ircCustomizeDefaultPort a) (unsafeCoerce v :: Int))
      ,("defaultChannel"
        ,\ v -> writeTVarIO (ircCustomizeDefaultChannel a) (unsafeCoerce v :: String))
      ]

-- | Update color tag.
ircUpdateColorTag :: HValue -> TVar Color -> TextTag -> IO ()
ircUpdateColorTag hvalue colorTVar colorTag = do
  let color = unsafeCoerce hvalue :: Color
  writeTVarIO colorTVar color
  set colorTag [textTagForegroundGdk := color]

-- | Temporary user name for test.
tempUser :: B.ByteString
tempUser = "Irc client for Manatee ( http://www.flickr.com/photos/48809572@N02/ )."

-- | Temporary real name for test.
tempRealname :: B.ByteString
tempRealname = "Manatee User"

-- | The prompt string.
promptStr :: String
promptStr = "> "

-- | The default time stamp : [00:00:00]
defaultTimeStamp :: String
defaultTimeStamp = 
    "          "

