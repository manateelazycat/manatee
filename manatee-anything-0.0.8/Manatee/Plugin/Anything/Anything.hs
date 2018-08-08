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
module Manatee.Plugin.Anything.Anything where

import Control.Arrow
import Data.Char
import Data.List
import Data.Typeable
import Manatee.Core.DBus
import Manatee.Core.Types 
import Manatee.Plugin.Anything.Types
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Typeable
import Manatee.Toolkit.General.Url

-- | Pick up anything name.
anythingName :: Anything -> AnythingName
anythingName = head . anythingColumnTitle

-- | Unpack anything candidate.
anythingCandidateUnpack :: Typeable a => AnythingCandidateWrap -> a
anythingCandidateUnpack (AnythingCandidateWrap c) = convertType c

-- | Default fuzzy filter rule.
anythingFuzzyFilterRule :: Bool -> AnythingInput -> String -> Bool
anythingFuzzyFilterRule caseSensitivity input candidateStr =
    all (`isInfixOf` filterStr) inputWords
  where (inputStr, filterStr) = 
            (if caseSensitivity 
                then id
                else map toLower *** map toLower) 
            (input, candidateStr)
        inputWords = words inputStr

-- | Search in different place. :)
anythingSearchCommands :: String -> [(AnythingCommandName, AnythingAction)]
anythingSearchCommands keyword =
    [("Search in Google",       search encodeUrl "http://www.google.com/search?&ie=UTF-8&oe=UTF-8&q=")
    ,("I'm feeling lucky",      search encodeUrl "http://www.google.com/search?&btnI=Im+Feeling+Lucky&ie=UTF-8&oe=UTF-8&q=")
    ,("Search in WolframAlpha", search encodeUrl "http://www.wolframalpha.com/input/?i=")
    ,("Search in Blekko",       search concatKeywords "http://blekko.com/ws/")
    ,("Search in Bing",         search encodeUrl "http://bing.com/search?q=")
    ,("Search in Yahoo",        search encodeUrl "http://search.yahoo.com/search?q=")
    ,("Search in Wikipedia",    search encodeUrl "http://en.wikipedia.org/wiki/")
    ,("Search in Google Image", search encodeUrl "http://images.google.com/images?sa=N&tab=wi&q=")
    ,("Search in Google Blog",  search encodeUrl "http://blogsearch.google.com/blogsearch?&ie=UTF-8&oe=UTF-8&q=")
    ,("Search in Google Group", search encodeUrl "http://groups.google.com/groups?&ie=UTF-8&oe=UTF-8&q=")
    ,("Search in Google Code",  search encodeUrl "http://www.google.com/codesearch?&lr=&q=")
    ,("Search in Google Map",   search encodeUrl "http://maps.google.com/maps?z=17&q=")
    ,("Search in HaskellWiki",  search encodeUrl "http://www.google.com/cse?cx=014102838545582129901%3Anhonl7a8bw8&q=")
    ,("Search in Hoogle",       search encodeUrl "http://haskell.org/hoogle/?hoogle=")
    ,("Search Slang",           search encodeUrl "http://www.urbandictionary.com/define.php?term=")
    ,("Search in Baidu Mp3",    search encodeUrl "http://mp3.baidu.com/m?f=ms&tn=baidump3&ct=134217728&lf=&rn=&lm=0&word=")
    ,("Open RFC",               search encodeUrl "http://www.ietf.org/rfc/rfc")
    ,("Open goo.gl link",       search encodeUrl "http://goo.gl/")
    ]
      where search encodeFun url client = 
                mkDaemonSignal client NewTab (NewTabArgs "PageBrowser" (url ++ encodeFun keyword) [])

-- | Open uri.
anythingOpenUri :: String -> [(AnythingCommandName, AnythingAction)]
anythingOpenUri uri = 
    [("Open URI",       open uri)]
    where open uri client = 
              mkDaemonSignal client NewTab (NewTabArgs "PageBrowser" uri [])

-- | Concat keywords by "+".
concatKeywords :: String -> String
concatKeywords = addMap (++ "+") . words

-- | Download file.
anythingDownload :: String -> [(AnythingCommandName, AnythingAction)]
anythingDownload url = 
    [("Download", download url)]
    where download url client = 
            ifM (isBusNameExist $ packGenericBusName "curl") 
                -- Send DBus signal if curl process exit.
                (mkGenericDaemonSignal client "curl" Generic (GenericArgs "Download" [url]))
                -- Otherwise start process and pass url.
                (mkDaemonSignal client NewTab (NewTabArgs "PageCurl" "Download" [url]))
