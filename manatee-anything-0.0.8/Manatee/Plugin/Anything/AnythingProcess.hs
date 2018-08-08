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
module Manatee.Plugin.Anything.AnythingProcess where

import DBus.Client hiding (Signal)
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types
import Manatee.Toolkit.General.Process
import System.Linux.Proc

instance AnythingCandidate ProcStatus where
    candidateCommandName        = show . psProcessId
    candidateFilterName a       = psCommand a ++ " " ++ show (psProcessId a)
    candidateCompletionName a _ = psCommand a
    candidateExpandName a _     = return $ psCommand a

-- | Search all running processes.
anythingProcess :: Anything
anythingProcess = 
  Anything {anythingColumnTitle         = ["Process", "ProcessId"]
           ,anythingColumnFun           = anythingProcessColumnFun
           ,anythingSearch              = AnythingSearch anythingProcessSearch
           ,anythingFilterRule          = anythingFuzzyFilterRule False
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingProcessCommandFun
           ,anythingCalculateDelay      = 500
           }

-- | Search all running processes.
anythingProcessSearch :: AnythingInput -> Client -> IO [ProcStatus]
anythingProcessSearch _ _ = procGetAllProcessStatus

-- | Function to generate candidate column.
anythingProcessColumnFun :: [AnythingColumnFun]
anythingProcessColumnFun =
    [psCommand . anythingCandidateUnpack
    ,show . psProcessId . anythingCandidateUnpack]

-- | Function to generate command list.
anythingProcessCommandFun :: AnythingCommandFun
anythingProcessCommandFun _ (AnythingCandidateWrap a) _ = do
  let processIdStr = candidateCommandName a
  return [("Kill process", anythingProcessActionKill processIdStr)]

-- | Kill process.
anythingProcessActionKill :: String -> Client -> IO ()
anythingProcessActionKill processId _ = 
  runCommand_ ("kill " ++ processId)
