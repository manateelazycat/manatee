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

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.MVar
import Control.Monad (forever)
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Extension.Mplayer.DBus
import Manatee.Extension.Mplayer.Daemon
import Manatee.Toolkit.General.DBus
import System.Environment
import System.Posix.Types

main :: IO ()
main = do
  args <- getArgs

  -- Init mplayer status. 
  status <- mplayerInitStatus

  -- Build match rule listen dbus message.
  client <- mkSessionClientWithName (packGenericBusName "mplayer")
  mkGenericDaemonMatchRule 
    client "mplayer"
    (Generic, \ args@(GenericArgs command options) ->
         case command of
           "Play"       -> 
             parseGenericSignalArgs 
                 args 
                 (\ [pathArg, processIdArg] -> 
                      (pathArg, read processIdArg :: ProcessID))
                 (mplayerPlay status)
           "Pause"      -> mplayerPause status
           "Stop"       -> mplayerStop status
           "Forward"    -> 
               parseGenericSignalArgs
                   args
                   (\ [step] -> read step :: Int)
                   (mplayerForward status)
           "Backward"   -> 
               parseGenericSignalArgs
                   args
                   (\ [step] -> read step :: Int)
                   (mplayerBackward status) 
           "VolumeInc"  -> 
               parseGenericSignalArgs
                   args
                   (\ [step] -> read step :: Int)
                   (mplayerVolumeInc status)
           "VolumeDec"  -> 
               parseGenericSignalArgs
                   args
                   (\ [step] -> read step :: Int)
                   (mplayerVolumeDec status)
           _ -> putStrLn $ "Invalid command : " ++ command) 

  case args of
    [] -> putStrLn "PlaylistBuffer should call 'manatee-mplayer-daemon processId', and not 'manatee-mplayer-daemon'."
    [x] -> do
      let processId = read x :: ProcessID
      mkMplayerClientSignal client processId DaemonProcessStartup DaemonProcessStartupArgs

  -- Quit process when received exit signal from manatee daemon process.
  waitBroadcastExitSignal client