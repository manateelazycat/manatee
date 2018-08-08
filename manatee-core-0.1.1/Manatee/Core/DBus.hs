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
module Manatee.Core.DBus where

import Control.Concurrent.MVar
import Control.Monad
import DBus.Client hiding (Signal)
import DBus.MatchRule
import DBus.Message (Signal, signalBody, MethodReturn, Error)
import DBus.Types
import Data.Text.Lazy (Text)
import Graphics.UI.Gtk.General.General
import Manatee.Core.TH
import Manatee.Core.Types
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Misc
import System.Posix.Process
import System.Posix.Types (ProcessID)

import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Control.Exception as Exc

-- | The daemon bus name.
daemonBusName :: Text 
daemonBusName = "org.manatee.daemon"

-- | The daemon interface name.
daemonInterfaceName :: Text 
daemonInterfaceName = "org.manatee.daemon.interface"

-- | The daemon path name.
daemonPathName :: Text 
daemonPathName = "/path"

-- | The damon broadcast interface name.
daemonBroadcastInterfaceName :: Text
daemonBroadcastInterfaceName = "org.manatee.daemon.broadcast.interface"

-- | The damon broadcast path name.
daemonBroadcastPathName :: Text
daemonBroadcastPathName = "/broadcastpath"

-- | The daemon interactive error name.
daemonInteractiveErrorName :: Text
daemonInteractiveErrorName = "org.manatee.daemon.interactive.error"

-- | The render bus name.
-- This name just template name,
-- The real render name should add render process id behind.
renderBusName :: Text
renderBusName = "org.manatee.render"

-- | The render interface name.
renderInterfaceName :: Text 
renderInterfaceName = "org.manatee.daemon.interface"

-- | The render path name.
renderPathName :: Text 
renderPathName = "/path"

-- | Generic daemon bus name.
genericDaemonBusName :: Text
genericDaemonBusName = "org.manatee.extension.generic"

genericDaemonInterfaceName :: Text 
genericDaemonInterfaceName = "org.manatee.daemon.interface"

-- | The daemon path name.
genericDaemonPathName :: Text 
genericDaemonPathName = "/path"

-- | DBus message error.
printDBusMessageError :: String -> String -> IO ()
printDBusMessageError funName member = 
    putStrLn (funName 
              ++ ": Invalid DBus message " 
              ++ member
              ++ "\nOne possible is extension package older than manatee-core, please make sure all pacages is newest."
              ++ "\nPlease report bug to author if still have this problem after update."     )

-- | Build render client name.
-- Concat renderBusName and render process id. 
mkRenderClientName :: ProcessID -> Text 
mkRenderClientName processId = TL.append renderBusName $ showText processId

-- | Unpack daemon signal from Variant list.
-- unpackDaemonSignalArgs :: DaemonMember -> [Variant] -> Maybe DaemonSignalArgs                                      
mkFunDec "unpackDaemonSignalArgs" (unpackVariantList ''DaemonMember ''DaemonSignalArgs)

-- | Unpack daemon signal from Variant list.
-- unpackDaemonBroadcastSignalArgs :: DaemonBroadcastMember -> [Variant] -> Maybe DaemonBroadcastSignalArgs
mkFunDec "unpackDaemonBroadcastSignalArgs" (unpackVariantList ''DaemonBroadcastMember ''DaemonBroadcastSignalArgs)

-- | Unpack render signal arguments from Variant list.
-- unpackRenderSignalArgs :: RenderMember -> [Variant] -> Maybe RenderSignalArgs
mkFunDec "unpackRenderSignalArgs" (unpackVariantList ''RenderMember ''RenderSignalArgs)

-- | Pack daemon signal argument to Variant list.
-- packDaemonSignalArgs :: DaemonSignalArgs -> [Variant]
$(packVariantList "packDaemonSignalArgs" ''DaemonSignalArgs)

-- | Pack render signal arguments to Variant list.
$(packVariantList "packRenderSignalArgs" ''RenderSignalArgs)

-- | Pack daemon broadcast signal arguments to Variant list.
-- packDaemonBroadcastSignalArgs :: DaemonBroadcastSignalArgs -> [Variant]
$(packVariantList "packDaemonBroadcastSignalArgs" ''DaemonBroadcastSignalArgs)

-- | Check daemon signal argument.
-- Return False if mismatch.
mkFunDec "checkDaemonSignalArgs" (checkSignalArgs ''DaemonMember ''DaemonSignalArgs)

-- | Check render signal arguments.
-- Return False is mismatch.
mkFunDec "checkRenderSignalArgs" (checkSignalArgs ''RenderMember ''RenderSignalArgs)

-- | Check daemon broadcast signal argument.
-- Return False if mismatch.
mkFunDec "checkDaemonBroadcastSignalArgs" (checkSignalArgs ''DaemonBroadcastMember ''DaemonBroadcastSignalArgs)

-- | Build daemon broadcast signal.
mkDaemonBroadcastSignal :: Client -> DaemonBroadcastMember -> DaemonBroadcastSignalArgs -> IO ()
mkDaemonBroadcastSignal client memberName args 
    | checkDaemonBroadcastSignalArgs memberName args -- check signal argument before emit signal.
        = emitSignal client signal
    | otherwise
        = putStrLn $ "mkDaemonBroadcastSignal CRITICAL: Invalid argument for dbus daemon broadcast member: " ++ show memberName
          where signal = mkMessageSignal 
                         daemonBroadcastPathName
                         (showText memberName)
                         daemonBroadcastInterfaceName
                         TL.empty  -- send to any client that interested this signal
                         (packDaemonBroadcastSignalArgs args)

-- | Build daemon broadcast match rule.
mkDaemonBroadcastMatchRule :: Client -> (DaemonBroadcastMember, DaemonBroadcastSignalArgs -> IO ()) -> IO ()
mkDaemonBroadcastMatchRule client (member, fun) =
    onSignal client matchRule $ \_ signal -> 
        pickDaemonBroadcastSignalArgs member signal >?>= fun
        where matchRule = mkMatchRule
                          (Just Signal) 
                          TL.empty
                          daemonBroadcastInterfaceName
                          (showText member)
                          daemonBroadcastPathName
                          TL.empty
                          []

-- | Build daemon signal.
-- If signal argument not match daemon member name.
mkDaemonSignal :: Client -> DaemonMember -> DaemonSignalArgs -> IO ()
mkDaemonSignal client memberName args 
    | checkDaemonSignalArgs memberName args -- check signal argument before emit signal.
        = emitSignal client signal
    | otherwise
        = putStrLn $ "mkDaemonSignal CRITICAL: Invalid argument for dbus daemon member: " ++ show memberName
          where signal = mkMessageSignal 
                         daemonPathName
                         (showText memberName)
                         daemonInterfaceName
                         daemonBusName
                         (packDaemonSignalArgs args)

-- | Build daemon match rule.
-- Use 'postGUIAsync' wrap all signal handler.
mkDaemonMatchRule :: Client -> (DaemonMember, DaemonSignalArgs -> IO ()) -> IO ()
mkDaemonMatchRule client (member, fun) = 
    -- Use postGUIAsync wrap DBus action to protect gtk+ main thread. 
    onSignal client matchRule $ \_ signal -> 
        pickDaemonSignalArgs member signal >?>= (postGUIAsync . fun)
        where matchRule = mkMatchRule
                          (Just Signal) 
                          TL.empty 
                          daemonInterfaceName
                          (showText member)
                          daemonPathName
                          daemonBusName
                          []

-- | Build daemon match rule list.
mkDaemonMatchRules :: Client -> [(DaemonMember, DaemonSignalArgs -> IO ())] -> IO ()              
mkDaemonMatchRules client = mapM_ (mkDaemonMatchRule client)

-- | Pick DaemonSignalArgs.
pickDaemonSignalArgs :: DaemonMember -> Signal -> IO (Maybe DaemonSignalArgs)          
pickDaemonSignalArgs member signal = 
  Exc.catch 
     (return $ unpackDaemonSignalArgs member $ signalBody signal)
     (\ (_ :: Exc.SomeException) -> 
        printDBusMessageError "pickDaemonSignalArgs" (show member)
        >> return Nothing)

-- | Pick DaemonBroadcastSignalArgs.
pickDaemonBroadcastSignalArgs :: DaemonBroadcastMember -> Signal -> IO (Maybe DaemonBroadcastSignalArgs)          
pickDaemonBroadcastSignalArgs member signal = 
    Exc.catch
       (return $ unpackDaemonBroadcastSignalArgs member $ signalBody signal)
       (\ (_ :: Exc.SomeException) -> 
          printDBusMessageError "pickDaemonBroadcastSignalArgs" (show member)
          >> return Nothing)

-- | Build render signal.
-- If signal argument not match render member name.
mkRenderSignal :: Client -> ProcessID -> RenderMember -> RenderSignalArgs -> IO ()
mkRenderSignal client processId memberName args 
    | checkRenderSignalArgs memberName args -- check signal argument before emit signal.
        = emitSignal client signal
    | otherwise
        = putStrLn $ "mkRenderSignal CRITICAL: Invalid argument for dbus render member: " ++ show memberName
          where signal = mkMessageSignal 
                         renderPathName
                         (showText memberName)
                         renderInterfaceName
                         (mkRenderClientName processId)
                         (packRenderSignalArgs args)

-- | Build render process match rule for catch signal.
-- Use 'postGUIAsync' wrap all signal handler.
-- If signal handler need long time calculation, 
-- please use 'mkRenderMatchRuleUnsafe' for better performance.
mkRenderMatchRule :: Client -> (RenderMember, RenderSignalArgs -> IO ()) -> IO ()
mkRenderMatchRule client (member, fun) = do
    processId <- getProcessID
    let matchRule = mkMatchRule 
                        (Just Signal) 
                        TL.empty 
                        renderInterfaceName 
                        (showText member)
                        renderPathName
                        (mkRenderClientName processId)
                        []
    -- Use postGUIAsync wrap DBus action to protect gtk+ main thread. 
    onSignal client matchRule $ \_ signal -> 
        pickRenderSignalArgs member signal >?>= (postGUIAsync . fun)

-- | Build render process match rule for catch signal.
-- Simliar 'mkRenderMatchRule', but don't use 'postGUIAsync' wrap all signal handler.
-- This function design for signal handler that need long time calculation.
-- So you need use 'postGUIAsync' wrap GTK+ code explicitly to protect GTK+ main thread won't crash.
mkRenderMatchRuleUnsafe :: Client -> (RenderMember, RenderSignalArgs -> IO ()) -> IO ()
mkRenderMatchRuleUnsafe client (member, fun) = do
    processId <- getProcessID
    let matchRule = mkMatchRule 
                        (Just Signal) 
                        TL.empty 
                        renderInterfaceName 
                        (showText member)
                        renderPathName
                        (mkRenderClientName processId)
                        []
    onSignal client matchRule $ \_ signal -> 
        pickRenderSignalArgs member signal >?>= fun

-- | Build render math rule list.
mkRenderMatchRules :: Client -> [(RenderMember, RenderSignalArgs -> IO ())] -> IO ()
mkRenderMatchRules client = mapM_ (mkRenderMatchRule client)

-- | Pick RenderSignalArgs.
pickRenderSignalArgs :: RenderMember -> Signal -> IO (Maybe RenderSignalArgs)
pickRenderSignalArgs member signal = 
    Exc.catch
       (return $ unpackRenderSignalArgs member $ signalBody signal)
       (\ (_ :: Exc.SomeException) -> 
          printDBusMessageError "pickRenderSignalArgs" (show member)
          >> return Nothing)

-- | Wait broadcast exit signal.
-- This function is useful for listen exit status of main process of manatee.
-- Most for manatee extension daemon process.
waitBroadcastExitSignal :: Client -> IO ()
waitBroadcastExitSignal client = do
  -- Quit process when received exit signal from manatee daemon process.
  exitSignal :: MVar String <- newEmptyMVar
  mkDaemonBroadcastMatchRule client (ExitDaemonProcess, \_ -> putMVar exitSignal "Exit")
  readMVar exitSignal           -- read exit signal to exit
  return ()

-- | Build daemon local object.
mkDaemonMethods :: [(Text, Member)] -> IO ()
mkDaemonMethods methods = 
    forM_ methods $ \ (name, member) -> do
      let object = LocalObject $ M.fromList [(mkInterfaceName_ daemonInterfaceName
                                             ,Interface $ M.fromList [(mkMemberName_ name, member)])] 
      client <- mkSessionClientWithName (TL.append daemonBusName name)
      export client (mkObjectPath_ daemonPathName) object

-- | Call daemon method.
callDaemonMethod :: Client -> Text -> [Variant] -> IO (Either Error MethodReturn)
callDaemonMethod client methodName =
  callProxyBlocking client (Proxy (RemoteObject 
                                    (mkBusName_ (TL.append daemonBusName methodName)) 
                                    (mkObjectPath_ daemonPathName)) 
                             (mkInterfaceName_ daemonInterfaceName))
                     (mkMemberName_ methodName) []

-- | Call daemon method asynchronously.
callDaemonMethodAsync :: Client -> Text -> [Variant] -> (Error -> IO ()) -> (MethodReturn -> IO ()) -> IO ()
callDaemonMethodAsync client methodName = 
  callProxy client (Proxy (RemoteObject 
                           (mkBusName_ (TL.append daemonBusName methodName)) 
                           (mkObjectPath_ daemonPathName)) 
                    (mkInterfaceName_ daemonInterfaceName))
                   (mkMemberName_ methodName) []

-- | Check daemon signal argument.
-- Return False if mismatch.
mkFunDec "checkGenericDaemonSignalArgs" (checkSignalArgs ''GenericDaemonMember ''GenericDaemonSignalArgs)

-- | Unpack daemon signal from Variant list.
-- unpackGenericDaemonSignalArgs :: GenericDaemonMember -> [Variant] -> Maybe GenericDaemonSignalArgs                 
mkFunDec "unpackGenericDaemonSignalArgs" (unpackVariantList ''GenericDaemonMember ''GenericDaemonSignalArgs)

-- | Pack daemon signal argument to Variant list.
-- packGenericDaemonSignalArgs :: GenericDaemonSignalArgs -> [Variant]
$(packVariantList "packGenericDaemonSignalArgs" ''GenericDaemonSignalArgs)

-- | Build daemon signal.
-- If signal argument not match daemon member name.
mkGenericDaemonSignal :: Client -> Text -> GenericDaemonMember -> GenericDaemonSignalArgs -> IO ()
mkGenericDaemonSignal client busName memberName args 
    | checkGenericDaemonSignalArgs memberName args -- check signal argument before emit signal.
        = emitSignal client signal
    | otherwise
        = putStrLn $ "mkGenericDaemonSignal CRITICAL: Invalid argument for dbus daemon member: " ++ show memberName
          where signal = mkMessageSignal 
                         genericDaemonPathName
                         (showText memberName)
                         genericDaemonInterfaceName
                         (packGenericBusName busName)
                         (packGenericDaemonSignalArgs args)

-- | Build daemon match rule.
mkGenericDaemonMatchRule :: Client -> Text -> (GenericDaemonMember, GenericDaemonSignalArgs -> IO ()) -> IO ()
mkGenericDaemonMatchRule client busName (member, fun) = 
    onSignal client matchRule $ \_ signal -> 
        pickGenericDaemonSignalArgs member signal >?>= fun
        where matchRule = mkMatchRule
                          (Just Signal) 
                          TL.empty 
                          genericDaemonInterfaceName
                          (showText member)
                          genericDaemonPathName
                          (packGenericBusName busName)
                          []

-- | Pick GenericDaemonSignalArgs.
pickGenericDaemonSignalArgs :: GenericDaemonMember -> Signal -> IO (Maybe GenericDaemonSignalArgs)          
pickGenericDaemonSignalArgs member signal = 
  Exc.catch 
      (return $ unpackGenericDaemonSignalArgs member $ signalBody signal)
      (\ (_ :: Exc.SomeException) -> 
         printDBusMessageError "pickGenericDaemonSignalArgs" (show member)
         >> return Nothing)

-- | Pack generic bus name.
packGenericBusName :: Text -> Text
packGenericBusName name =
    TL.concat [genericDaemonBusName, ".", name]

-- | Apply generic action.
parseGenericSignalArgs :: GenericDaemonSignalArgs -> ([String] -> a) -> (a -> IO ()) -> IO ()
parseGenericSignalArgs args@(GenericArgs _ options) paser action = 
  Exc.catch (action $ paser options) 
            $ \ (_ :: Exc.SomeException) -> 
                putStrLn $ "Parse " ++ show args ++ " failed, please make sure transfer valid DBus arguments."
