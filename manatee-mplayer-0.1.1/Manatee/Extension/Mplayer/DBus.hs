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
module Manatee.Extension.Mplayer.DBus where

import DBus.Client hiding (Signal)
import DBus.MatchRule
import DBus.Message (Signal, signalBody)
import Data.Text.Lazy (empty)
import Graphics.UI.Gtk.General.General
import Manatee.Core.DBus
import Manatee.Core.TH
import Manatee.Core.Types
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Misc
import System.Posix.Process
import System.Posix.Types (ProcessID)

import qualified Control.Exception as Exc

data MplayerClientMember = PlayFinished
                         | DaemonProcessStartup
                           deriving (Show, Eq, Ord)

data MplayerClientSignalArgs = PlayFinishedArgs 
                             | DaemonProcessStartupArgs
                             deriving (Show, Eq, Ord)

-- | Check client signal argument.
-- Return False if mismatch.
mkFunDec "checkMplayerClientSignalArgs" (checkSignalArgs ''MplayerClientMember ''MplayerClientSignalArgs)

-- | Unpack client signal from Variant list.
-- unpackMplayerClientSignalArgs :: MplayerClientMember -> [Variant] -> Maybe MplayerClientSignalArgs                 
mkFunDec "unpackMplayerClientSignalArgs" (unpackVariantList ''MplayerClientMember ''MplayerClientSignalArgs)

-- | Pack client signal argument to Variant list.
-- packMplayerClientSignalArgs :: MplayerClientSignalArgs -> [Variant]
$(packVariantList "packMplayerClientSignalArgs" ''MplayerClientSignalArgs)

-- | Pick MplayerClientSignalArgs.
pickMplayerClientSignalArgs :: MplayerClientMember -> Signal -> IO (Maybe MplayerClientSignalArgs)          
pickMplayerClientSignalArgs member signal = 
    Exc.catch
        (return $ unpackMplayerClientSignalArgs member $ signalBody signal)
        (\ (_ :: Exc.SomeException) -> 
             printDBusMessageError "pickMplayerClientSignalArgs" (show member)
             >> return Nothing)

-- | Build render signal.
-- If signal argument not match render member name.
mkMplayerClientSignal :: Client -> ProcessID -> MplayerClientMember -> MplayerClientSignalArgs -> IO ()
mkMplayerClientSignal client processId memberName args 
    | checkMplayerClientSignalArgs memberName args -- check signal argument before emit signal.
        = emitSignal client signal
    | otherwise
        = putStrLn $ "mkMplayerClientSignal CRITICAL: Invalid argument for dbus render member: " ++ show memberName
          where signal = mkMessageSignal 
                         renderPathName
                         (showText memberName)
                         renderInterfaceName
                         (mkRenderClientName processId)
                         (packMplayerClientSignalArgs args)

-- | Build render process match rule for catch signal.
mkMplayerClientMatchRule :: Client -> (MplayerClientMember, MplayerClientSignalArgs -> IO ()) -> IO ()
mkMplayerClientMatchRule client (member, fun) = do
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
        pickMplayerClientSignalArgs member signal >?>= (postGUIAsync . fun)

-- | Build daemon signal.
mkMplayerDaemonSignal :: Client -> String -> [String] -> IO ()
mkMplayerDaemonSignal client command options = 
    mkGenericDaemonSignal client "mplayer" Generic (GenericArgs command options)
