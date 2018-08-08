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

module Config.Default where

import Graphics.UI.Gtk.Gdk.GC (Color (..))
import Graphics.UI.Gtk.General.Enums (TextWindowType (..))
import Language.Translate.Google

-- | Self message color.
selfMsgColor :: Color
selfMsgColor = Color 7710 37008 65535

-- | Server message color.
serverMsgColor :: Color
serverMsgColor = Color 30000 30000 0

-- | Other message color.
otherMsgColor :: Color
otherMsgColor = Color 0 0 0

-- | Join message color.
joinMsgColor :: Color
joinMsgColor = Color 12850 52685 12850 

-- | Quit message color.
quitMsgColor :: Color
quitMsgColor = Color 52685 14135 0

-- | Part message color.
partMsgColor :: Color
partMsgColor = Color 52685 14135 0

-- | Action message color.
actionMsgColor :: Color
actionMsgColor = Color 41120 8224 61680

-- | Url color.
urlColor :: Color
urlColor = Color 38036 0 54227

-- | The column limit to wrap irc message.
wrapColumn :: Int
wrapColumn = 100

-- | Time stamp position.
timeStampPosition :: TextWindowType
timeStampPosition = TextWindowLeft

-- | Target language to translate.
targetLanguage :: Language
targetLanguage = English

-- | Source language.
sourceLanguage :: Language
sourceLanguage = ChineseSimplified

-- | Show line number.
showLineNumber :: Bool
showLineNumber = False

-- | Default user nick.
defaultNick :: String
defaultNick = "manateeUser"

-- | Default server.
defaultServer :: String
defaultServer = "irc.freenode.net"

-- | Default port.
defaultPort :: Int
defaultPort = 6667

-- | Default channel
defaultChannel :: String
defaultChannel = "#haskell"
