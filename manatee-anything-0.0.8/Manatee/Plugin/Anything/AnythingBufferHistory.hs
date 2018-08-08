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
module Manatee.Plugin.Anything.AnythingBufferHistory where

import DBus.Client hiding (Signal)
import DBus.Message
import DBus.Types
import Data.Maybe
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types
import Network.URI

instance AnythingCandidate BufferHistory where
  candidateCommandName _      = ""
  candidateFilterName a       = bPath ++ " " ++ bMode
      where bMode = bufferHistoryMode a
            bPath = bufferHistoryPath a
  candidateCompletionName a _ = bufferHistoryPath a
  candidateExpandName a _     = return $ bufferHistoryPath a

-- | Search buffer history.
anythingBufferHistory :: Anything
anythingBufferHistory = 
  Anything {anythingColumnTitle         = ["BufferHistory", "Mode"]
           ,anythingColumnFun           = anythingBufferHistoryColumnFun
           ,anythingSearch              = AnythingSearch anythingBufferHistorySearch
           ,anythingFilterRule          = anythingFuzzyFilterRule False
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = False
           ,anythingCommandFun          = anythingBufferHistoryCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search all buffers.
anythingBufferHistorySearch :: AnythingInput -> Client -> IO [BufferHistory]
anythingBufferHistorySearch _ client = do
    reply <- callDaemonMethod client "GetBufferHistory" []
    case reply of    
      Left err -> do
        putStrLn $ "anythingBufferHistorySearch : Call remote function 'GetBufferList' failed." 
                   ++ "\n  Reason : " ++ show err
        return []
      Right methodReturn -> do
          let variants = messageBody methodReturn
          return $ if null variants 
                      then []
                      else 
                          let list = fromMaybe [] (fromVariant (head variants) :: Maybe [BufferHistory])
                          -- Filter browser buffer.
                          -- AnythingBrowseHistory can do better. :)
                          in filter (not . isURI . bufferHistoryPath) list

-- | Function for generate candidate column.
anythingBufferHistoryColumnFun :: [AnythingColumnFun]
anythingBufferHistoryColumnFun =
  [bufferHistoryPath . anythingCandidateUnpack
  ,bufferHistoryMode . anythingCandidateUnpack]

-- | Function for generate command list.
anythingBufferHistoryCommandFun :: AnythingCommandFun
anythingBufferHistoryCommandFun _ wrap _ = do
  let pageType = bufferHistoryType $ anythingCandidateUnpack wrap
      pagePath = bufferHistoryPath $ anythingCandidateUnpack wrap
  return [("Open buffer",    anythingBufferHistoryOpenBuffer pageType pagePath)]
  
-- | Open buffer.
anythingBufferHistoryOpenBuffer :: PageType -> String -> Client -> IO ()
anythingBufferHistoryOpenBuffer pageType pagePath client =
  mkDaemonSignal client NewTab (NewTabArgs pageType pagePath [])
