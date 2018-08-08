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
module Manatee.Plugin.Anything.AnythingApplication where

import Control.Monad
import DBus.Client hiding (Signal)
import Data.ByteString.UTF8 hiding (take, length)
import Data.Char (toLower)
import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Data.Typeable
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.General.String
import Manatee.Toolkit.Gio.Gio
import System.FilePath
import System.GIO

data ApplicationInfo =
    ApplicationInfo {applicationInfoName        :: String
                    ,applicationInfoDescription :: String
                    ,applicationInfoCommand     :: String
                    } deriving Typeable

instance AnythingCandidate ApplicationInfo where
  candidateCommandName         = applicationInfoCommand
  candidateFilterName a        = applicationInfoName a ++ " " ++ applicationInfoDescription a
  candidateCompletionName a _  = applicationInfoName a
  candidateExpandName a _      = return $ applicationInfoName a

-- | Search and run register application from system.
anythingApplication :: Anything
anythingApplication = 
  Anything {anythingColumnTitle         = ["Application", "Description"]
           ,anythingColumnFun           = anythingApplicationColumnFun
           ,anythingSearch              = AnythingSearch anythingApplicationSearch
           ,anythingFilterRule          = anythingFuzzyFilterRule False
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingApplicationCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search candidate.
anythingApplicationSearch :: AnythingInput -> Client -> IO [ApplicationInfo]
anythingApplicationSearch _ _ = 
   liftM (sortBy (comparing (map toLower . applicationInfoName)) . map 
                     (\x -> ApplicationInfo 
                           (takeFileName $ appInfoGetExecutable x)
                           (fromMaybe "" $ appInfoGetDescription x)
                           (anythingApplicationFilterCommandName $ toString $ fromMaybe "" $ appInfoGetCommandline x)
                     )) appInfoGetAll

-- | Function for generate candidate column.
anythingApplicationColumnFun :: [AnythingColumnFun] 
anythingApplicationColumnFun =
    [applicationInfoName . anythingCandidateUnpack
    ,applicationInfoDescription . anythingCandidateUnpack]

-- | Function for generate command list.
anythingApplicationCommandFun :: AnythingCommandFun
anythingApplicationCommandFun _ (AnythingCandidateWrap a) _ = do
  let command = candidateCommandName a
  return [("Startup application", anythingApplicationActionStartup command)
         ,("Running in terminal", anythingApplicationActionRunInTerminal command)]

-- | Startup application.
anythingApplicationActionStartup :: String -> Client -> IO ()
anythingApplicationActionStartup command _ = 
  runExternalCommand command

-- | Run command in terminal.
anythingApplicationActionRunInTerminal :: String -> Client -> IO ()
anythingApplicationActionRunInTerminal command _ = 
  launchCommandInTerminal command

-- | Because appInfoGetCommandline will append `%u` or `%U` after command line.
-- This only work if gio compiled with FUSE support.
anythingApplicationFilterCommandName :: String -> String
anythingApplicationFilterCommandName = 
    dropSuffix ["%u", "%U"]
