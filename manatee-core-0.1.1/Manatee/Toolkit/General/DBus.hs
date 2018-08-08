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
module Manatee.Toolkit.General.DBus where

import Control.Arrow (first)
import DBus.Bus
import DBus.Client
import DBus.MatchRule
import DBus.Message (messageBody)
import DBus.Types
import Data.Maybe (fromMaybe)
import Data.Text.Lazy hiding (zip, map, head)

import qualified DBus.Client as DC
import qualified DBus.Message as DM
import qualified DBus.NameReservation as DNR
import qualified Data.Map as M

type ObjectPathText    = Text
type MemberNameText    = Text
type InterfaceNameText = Text
type BusNameText       = Text
type SignatureText     = Text

-- | Member name prefix for some name that beginning with digit.
memberNamePrefix :: Text
memberNamePrefix = "member_"

-- | Handy function for build DBus message signal.
mkMessageSignal :: ObjectPathText 
                -> MemberNameText 
                -> InterfaceNameText 
                -> BusNameText 
                -> [Variant] 
                -> DM.Signal
mkMessageSignal oPath mName iName bName =
  DM.Signal (mkObjectPath_ oPath)
            (mkMemberName_ mName)
            (mkInterfaceName_ iName)
            (mkBusName bName)

-- | Build system bus client.
mkSystemClient :: IO Client                      
mkSystemClient = mkClient =<< getSystemBus

-- | Build system bus client with name.
mkSystemClientWithName :: Text -> IO Client
mkSystemClientWithName clientName = do
  client <- mkSystemClient
  requestName' client clientName []
  return client

-- | Build session bus client.
mkSessionClient :: IO Client                      
mkSessionClient = mkClient =<< getSessionBus

-- | Build session bus client with name.
mkSessionClientWithName :: Text -> IO Client
mkSessionClientWithName clientName = do
  client <- mkSessionClient
  requestName' client clientName []
  return client

-- | Build starter bus client.
mkStarterClient :: IO Client                      
mkStarterClient = mkClient =<< getStarterBus

-- | Request name.
requestName' :: Client -> BusNameText -> [DNR.RequestNameFlag] -> IO DNR.RequestNameReply
requestName' client name = requestName client (mkBusName_ name)

-- | Build LoaclObject with Text information, handy!.
mkLocalObject :: [(InterfaceNameText, [(MemberNameText, Member)])] -> LocalObject
mkLocalObject list = LocalObject $ M.fromList $ zip interfaceName interface
    where interfaceNameList = fst $ unzip list                       -- [InterfaceNameText]
          interfaceList     = snd $ unzip list                       -- [[(MemberNameText, Member)]]
          interfaceName     = map mkInterfaceName_ interfaceNameList -- [interfaceName]
          interface         = map (Interface . M.fromList . map (first mkMemberName_)) interfaceList -- [Interface]

-- | Like `export`, but instead ObjectPath with ObjectPathText.
export' :: Client -> ObjectPathText -> LocalObject -> IO ()
export' client path = export client (mkObjectPath_ path)

-- | Export LocalObject.
exportLocalObject :: Client -> ObjectPathText -> [(InterfaceNameText, [(MemberNameText, Member)])] -> IO ()
exportLocalObject client path info = export' client path (mkLocalObject info)

-- | Build MatchRule.
mkMatchRule :: Maybe MessageType 
            -> BusNameText 
            -> InterfaceNameText 
            -> MemberNameText 
            -> ObjectPathText 
            -> BusNameText 
            -> [ParameterValue] 
            -> MatchRule 
mkMatchRule mType sender interface member path dest =
  MatchRule mType 
            (mkBusName sender)
            (mkInterfaceName interface)
            (mkMemberName member)
            (mkObjectPath path)
            (mkBusName dest)

-- | Create Signal Memeber.
mkSignalMember :: SignatureText -> Member            
mkSignalMember = 
  DC.Signal . mkSignature_

-- | Create Method Member.
mkMethodMember :: SignatureText -> SignatureText -> (MethodCall -> IO ()) -> Member
mkMethodMember input output = 
    DC.Method (mkSignature_ input) (mkSignature_ output)

-- | Pack digit member name for protocol.
packDigitMemberName :: Text -> Text
packDigitMemberName name = 
  memberNamePrefix `append` name

-- | Unpack digit member name for protocol.
unpackDigitMemberName :: Text -> Text
unpackDigitMemberName = 
  replace memberNamePrefix empty

-- | Check specify bus name whether exist.
-- This function very useful to check dbus process whether exist 
-- by check bus name in dbus list.
isBusNameExist :: Text -> IO Bool
isBusNameExist busName = do
  -- Request a list of connected clients from the bus
  client <- mkSessionClient
  reply  <- callProxyBlocking_ client dbus "ListNames" [] []
	
  -- Get bus name list.
  let names = fromMaybe [] (fromArray =<< fromVariant (head $ messageBody reply))

  -- Whether dbus name exist?
  return (busName `elem` names)
      where 
        dbus :: Proxy
        dbus = Proxy (RemoteObject "org.freedesktop.DBus" "/org/freedesktop/DBus") "org.freedesktop.DBus"
