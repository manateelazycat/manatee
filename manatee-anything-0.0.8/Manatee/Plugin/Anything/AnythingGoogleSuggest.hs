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
module Manatee.Plugin.Anything.AnythingGoogleSuggest where

import DBus.Client hiding (Signal)
import Data.Typeable
import Google.Suggest
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types
import Manatee.Toolkit.General.Misc
import Network.URI

data Suggestion =
    Suggestion {suggestionName  :: String
               ,suggestionNumber:: Int
               } deriving (Show, Ord, Eq, Typeable)

instance AnythingCandidate Suggestion where
    candidateCommandName        = suggestionName
    candidateFilterName a       = suggestionName a ++ " " ++ show (suggestionNumber a)
    candidateCompletionName a _ = suggestionName a
    candidateExpandName a _     = return $ suggestionName a

-- | Search all execute files that can find in PATH.
anythingGoogleSuggest :: Anything
anythingGoogleSuggest = 
  Anything {anythingColumnTitle         = ["GoogleSuggest", "QueryNumber"]
           ,anythingColumnFun           = anythingGoogleSuggestColumnFun
           ,anythingSearch              = AnythingSearch anythingGoogleSuggestSearch
           ,anythingFilterRule          = anythingFuzzyFilterRule True
           ,anythingCompletionRule      = id
           ,anythingInputDepend         = True
           ,anythingCommandFun          = anythingGoogleSuggestCommandFun
           ,anythingCalculateDelay      = 500
           }

-- | Search all execute files.
anythingGoogleSuggestSearch :: AnythingInput -> Client -> IO [Suggestion]
anythingGoogleSuggestSearch input _ = do
    result <- suggest input Nothing
    return $ case result of
               Right list -> map (uncurry Suggestion) list
               Left _ -> []

-- | Function to generate candidate column.
anythingGoogleSuggestColumnFun :: [AnythingColumnFun]
anythingGoogleSuggestColumnFun =
    [suggestionName . anythingCandidateUnpack
    ,splitInt . suggestionNumber . anythingCandidateUnpack]

-- | Function for generate command list.
anythingGoogleSuggestCommandFun :: AnythingCommandFun
anythingGoogleSuggestCommandFun _ (AnythingCandidateWrap a) _ 
    | isURI input
        = return $ 
          anythingOpenUri input
          ++ (anythingSearchCommands input) 
          ++ anythingDownload input
    | isURI httpInput
        = return $ 
          anythingOpenUri httpInput
          ++ (anythingSearchCommands input) 
          ++ anythingDownload httpInput
    | otherwise
        = return (anythingSearchCommands input)
          where input = candidateCommandName a
                httpInput = "http://" ++ input
