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
import Manatee.Extension.IrcClient.DBus
import Manatee.Extension.IrcClient.Daemon
import Manatee.Toolkit.General.DBus
import System.Environment
import System.Posix.Types

-- | Startup daemon process to listen irc signal.
main :: IO ()
main = do
  args <- getArgs

  -- Init irc status. 
  status <- ircInitStatus

  -- Build match rule listen dbus message.
  client <- mkSessionClientWithName ircDaemonBusName
  mkIrcDaemonMatchRules client 
       [(Join,          ircJoin status) 
       ,(Part,          ircPart status)
       ,(SendMessage,   ircSendMessage status)
       ]

  case args of
    [] -> putStrLn "IrcBuffer should call 'manatee-irc-daemon processId', and not 'manatee-irc-daemon'."
    [x] -> do
      let processId = read x :: ProcessID
      mkIrcClientSignal client processId DaemonProcessStartup DaemonProcessStartupArgs

  -- Quit process when received exit signal from manatee daemon process.
  let exitSignal = ircExitSignal status
  mkDaemonBroadcastMatchRule client (ExitDaemonProcess, \_ -> putMVar exitSignal "Exit")
  readMVar exitSignal           -- read exit signal to exit
  putStrLn "Exit irc daemon process."

