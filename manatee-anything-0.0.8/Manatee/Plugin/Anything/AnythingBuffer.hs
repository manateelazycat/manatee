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
module Manatee.Plugin.Anything.AnythingBuffer where

import DBus.Client hiding (Signal)
import DBus.Message
import DBus.Types
import Data.Maybe
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types

instance AnythingCandidate BufferInfo where
  candidateCommandName _      = ""
  candidateFilterName a       = bName ++ " " ++ bMode ++ " " ++ bPath
      where bName = bufferInfoName a
            bMode = bufferInfoMode a
            bPath = bufferInfoPath a
  candidateCompletionName a _ = bufferInfoName a
  candidateExpandName a _     = return $ bufferInfoName a

-- | Search all buffer files.
anythingBuffer :: Anything
anythingBuffer = 
  Anything {anythingColumnTitle         = ["Buffer", "Mode", "Path"]
           ,anythingColumnFun           = anythingBufferColumnFun
           ,anythingSearch              = AnythingSearch anythingBufferSearch
           ,anythingFilterRule          = anythingFuzzyFilterRule False
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingBufferCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search all buffers.
anythingBufferSearch :: AnythingInput -> Client -> IO [BufferInfo]
anythingBufferSearch _ client = do
    reply <- callDaemonMethod client "GetBufferList" []
    case reply of    
      Left err -> do
        putStrLn $ "anythingBufferSearch : Call remote function 'GetBufferList' failed." 
                   ++ "\n  Reason : " ++ show err
        return []
      Right methodReturn -> do
          let variants = messageBody methodReturn
          return $ if null variants 
                      then []
                      else fromMaybe [] (fromVariant (head variants) :: Maybe [BufferInfo])

-- | Function for generate candidate column.
anythingBufferColumnFun :: [AnythingColumnFun]
anythingBufferColumnFun =
  [bufferInfoName . anythingCandidateUnpack
  ,bufferInfoMode . anythingCandidateUnpack
  ,bufferInfoPath . anythingCandidateUnpack]

-- | Function for generate command list.
anythingBufferCommandFun :: AnythingCommandFun
anythingBufferCommandFun _ wrap _ = do
   let modeName = bufferInfoMode $ anythingCandidateUnpack wrap
       pageId   = bufferInfoId $ anythingCandidateUnpack wrap
   return [("Switch buffer",    anythingBufferSwitchBuffer modeName pageId)]
  
-- | Swtich buffer.
anythingBufferSwitchBuffer :: PageModeName -> PageId -> Client -> IO ()
anythingBufferSwitchBuffer modeName pageId client =
  mkDaemonSignal client SwitchBuffer (SwitchBufferArgs modeName pageId)
