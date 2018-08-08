-- Author:     Andy Stewart <lazycat.manatee@gmail.com>
-- Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
-- 
-- Copyright (C) 2010 Andy Stewart, all rights reserved.
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

{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, DeriveDataTypeable, RankNTypes #-}
module Manatee.Plugin.Anything.AnythingExecuteFile where

import Control.Monad
import DBus.Client hiding (Signal)
import Data.ByteString.UTF8
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.Gio.Gio

-- | Search all execute files that can find in PATH.
anythingExecuteFile :: Anything
anythingExecuteFile = 
  Anything {anythingColumnTitle         = ["ExecuteFile"]
           ,anythingColumnFun           = [anythingCandidateUnpack]
           ,anythingSearch              = AnythingSearch anythingExecuteFileSearch
           ,anythingFilterRule          = anythingFuzzyFilterRule False
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingExecuteFileCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search all execute files.
anythingExecuteFileSearch :: AnythingInput -> Client -> IO [String]
anythingExecuteFileSearch _ _ = 
    liftM (map toString) getAllExecuteFiles

-- | Function for generate command list.
anythingExecuteFileCommandFun :: AnythingCommandFun
anythingExecuteFileCommandFun _ (AnythingCandidateWrap a) _ = do
  let command = candidateCommandName a
  return [("Execute",                   anythingExecuteFileActionExecute command)
         ,("Running in terminal",       anythingExecuteFileActionRunInTerminal command)]

-- | Run command.
anythingExecuteFileActionExecute :: String -> Client -> IO ()
anythingExecuteFileActionExecute command _ =
  runExternalCommand command

-- | Run command in terminal.
anythingExecuteFileActionRunInTerminal :: String -> Client -> IO ()
anythingExecuteFileActionRunInTerminal command _ = 
  launchCommandInTerminal command


