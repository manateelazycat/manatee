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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Manatee.Extension.IrcClient.DBus where

import DBus.Client hiding (Signal)
import DBus.MatchRule
import DBus.Message (Signal, signalBody)
import DBus.Types
import Data.Text.Lazy (Text, empty)
import Graphics.UI.Gtk.General.General
import Language.Haskell.TH
import Manatee.Core.DBus
import Manatee.Core.TH
import Manatee.Extension.IrcClient.Types
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Misc
import Network.FastIRC.Types
import System.Posix.Process 
import System.Posix.Types (ProcessID)

import qualified Data.ByteString.Char8 as B
import qualified Control.Exception as Exc

data IrcDaemonMember = Join
                     | Part
                     | SendMessage
                       deriving (Show, Eq, Ord)

data IrcDaemonSignalArgs = JoinArgs Server Port Channel Nick ProcessID
                         | PartArgs Server Channel ProcessID
                         | SendMessageArgs Server B.ByteString
                           deriving (Show, Eq, Ord)

data IrcClientMember = ReceivePrivate
                     | ReceiveJoin
                     | ReceiveTopicReply
                     | ReceiveTopicWhoTime
                     | ReceiveChannelUrl
                     | ReceiveNames
                     | ReceiveQuit
                     | ReceivePart
                     | DaemonProcessStartup
                       deriving (Show, Eq, Ord)

data IrcClientSignalArgs = ReceivePrivateArgs NickName CommandArg
                         | ReceiveJoinArgs NickName UserName HostName
                         | ReceiveTopicReplyArgs CommandArg
                         | ReceiveTopicWhoTimeArgs NickName Integer
                         | ReceiveChannelUrlArgs B.ByteString
                         | ReceiveNamesArgs B.ByteString
                         | ReceiveQuitArgs NickName UserName HostName CommandArg
                         | ReceivePartArgs NickName UserName HostName CommandArg
                         | DaemonProcessStartupArgs
                           deriving (Show, Eq, Ord)

-- | Irc daemon bus name.
ircDaemonBusName :: Text
ircDaemonBusName = "org.manatee.extension.irc.daemon"

ircDaemonInterfaceName :: Text 
ircDaemonInterfaceName = "org.manatee.daemon.interface"

-- | The daemon path name.
ircDaemonPathName :: Text 
ircDaemonPathName = "/path"

-- | Check daemon signal argument.
-- Return False if mismatch.
mkFunDec "checkIrcDaemonSignalArgs" (checkSignalArgs ''IrcDaemonMember ''IrcDaemonSignalArgs)

-- | Unpack daemon signal from Variant list.
-- unpackIrcDaemonSignalArgs :: IrcDaemonMember -> [Variant] -> Maybe IrcDaemonSignalArgs                 
mkFunDec "unpackIrcDaemonSignalArgs" (unpackVariantList ''IrcDaemonMember ''IrcDaemonSignalArgs)

-- | Pack daemon signal argument to Variant list.
-- packIrcDaemonSignalArgs :: IrcDaemonSignalArgs -> [Variant]
$(packVariantList "packIrcDaemonSignalArgs" ''IrcDaemonSignalArgs)

-- | Check client signal argument.
-- Return False if mismatch.
mkFunDec "checkIrcClientSignalArgs" (checkSignalArgs ''IrcClientMember ''IrcClientSignalArgs)

-- | Unpack client signal from Variant list.
-- unpackIrcClientSignalArgs :: IrcClientMember -> [Variant] -> Maybe IrcClientSignalArgs                 
mkFunDec "unpackIrcClientSignalArgs" (unpackVariantList ''IrcClientMember ''IrcClientSignalArgs)

-- | Pack client signal argument to Variant list.
-- packIrcClientSignalArgs :: IrcClientSignalArgs -> [Variant]
$(packVariantList "packIrcClientSignalArgs" ''IrcClientSignalArgs)

-- | Build daemon signal.
-- If signal argument not match daemon member name.
mkIrcDaemonSignal :: Client -> IrcDaemonMember -> IrcDaemonSignalArgs -> IO ()
mkIrcDaemonSignal client memberName args 
    | checkIrcDaemonSignalArgs memberName args -- check signal argument before emit signal.
        = emitSignal client signal
    | otherwise
        = putStrLn $ "mkIrcDaemonSignal CRITICAL: Invalid argument for dbus daemon member: " ++ show memberName
          where signal = mkMessageSignal 
                         ircDaemonPathName
                         (showText memberName)
                         ircDaemonInterfaceName
                         ircDaemonBusName
                         (packIrcDaemonSignalArgs args)

-- | Build daemon match rule.
mkIrcDaemonMatchRule :: Client -> (IrcDaemonMember, IrcDaemonSignalArgs -> IO ()) -> IO ()
mkIrcDaemonMatchRule client (member, fun) = 
    onSignal client matchRule $ \_ signal -> 
        pickIrcDaemonSignalArgs member signal >?>= fun
        where matchRule = mkMatchRule
                          (Just Signal) 
                          empty 
                          ircDaemonInterfaceName
                          (showText member)
                          ircDaemonPathName
                          ircDaemonBusName
                          []

-- | Build daemon match rule list.
mkIrcDaemonMatchRules :: Client -> [(IrcDaemonMember, IrcDaemonSignalArgs -> IO ())] -> IO ()              
mkIrcDaemonMatchRules client = mapM_ (mkIrcDaemonMatchRule client)

-- | Pick IrcDaemonSignalArgs.
pickIrcDaemonSignalArgs :: IrcDaemonMember -> Signal -> IO (Maybe IrcDaemonSignalArgs)          
pickIrcDaemonSignalArgs member signal = 
    Exc.catch
        (return $ unpackIrcDaemonSignalArgs member $ signalBody signal)
        (\ (_ :: Exc.SomeException) -> 
             printDBusMessageError "pickIrcDaemonSignalArgs" (show member)
             >> return Nothing)

-- | Pick IrcClientSignalArgs.
pickIrcClientSignalArgs :: IrcClientMember -> Signal -> IO (Maybe IrcClientSignalArgs)          
pickIrcClientSignalArgs member signal = 
    Exc.catch
        (return $ unpackIrcClientSignalArgs member $ signalBody signal)
        (\ (_ :: Exc.SomeException) ->
             printDBusMessageError "pickIrcClientSignalArgs" (show member)
             >> return Nothing)

-- | Build render signal.
-- If signal argument not match render member name.
mkIrcClientSignal :: Client -> ProcessID -> IrcClientMember -> IrcClientSignalArgs -> IO ()
mkIrcClientSignal client processId memberName args 
    | checkIrcClientSignalArgs memberName args -- check signal argument before emit signal.
        = emitSignal client signal
    | otherwise
        = putStrLn $ "mkIrcClientSignal CRITICAL: Invalid argument for dbus render member: " ++ show memberName
          where signal = mkMessageSignal 
                         renderPathName
                         (showText memberName)
                         renderInterfaceName
                         (mkRenderClientName processId)
                         (packIrcClientSignalArgs args)

-- | Build render process match rule for catch signal.
mkIrcClientMatchRule :: Client -> (IrcClientMember, IrcClientSignalArgs -> IO ()) -> IO ()
mkIrcClientMatchRule client (member, fun) = do
    processId <- getProcessID
    let matchRule = mkMatchRule 
                          (Just Signal) 
                          empty 
                          renderInterfaceName 
                          (showText member)
                          renderPathName
                          (mkRenderClientName processId)
                          []
    -- Use postGUIAsync wrap DBus action to protect gtk+ main thread. 
    onSignal client matchRule $ \_ signal -> 
        pickIrcClientSignalArgs member signal >?>= (postGUIAsync . fun)

-- | Build irc client match rule for catch signal.
mkIrcClientMatchRules :: Client -> [(IrcClientMember, IrcClientSignalArgs -> IO ())] -> IO ()
mkIrcClientMatchRules client = mapM_ (mkIrcClientMatchRule client)

deriveVariable (conT ''Integer)
