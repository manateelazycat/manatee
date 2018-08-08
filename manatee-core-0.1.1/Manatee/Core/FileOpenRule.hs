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
module Manatee.Core.FileOpenRule where

import Control.Monad
import DBus.Client hiding (Signal)
import Data.ByteString.UTF8
import Manatee.Core.DBus
import Manatee.Core.Config
import Manatee.Core.Types 
import System.GIO.File.AppInfo
import System.GIO.File.File
import Text.Regex.TDFA

import qualified Data.Map as M

-- | The rule how to open file.
fileOpenRule :: FilePath -> String -> IO [(String, Client -> IO ())] 
fileOpenRule filePath contentType = do
  -- The default rule.
  (FileOpenRule ruleMap) <- readConfig fileOpenRulePath (FileOpenRule M.empty)
  let rule = M.toList ruleMap
      getDefalutRule [] = []
      getDefalutRule ((fileMatch, rules) :xs) = 
          let isMatch = case fileMatch of
                          ContentTypeMatch match ->
                            contentType == match
                          RegexpMatch match ->
                            contentType =~ match
          in if isMatch
                then 
                    map (\ (name, pType, prefix) -> 
                             (name, 
                              \client -> 
                                  mkDaemonSignal client NewTab (NewTabArgs pType (prefix ++ filePath) []))
                        ) rules
                else getDefalutRule xs
  let defaultRule = getDefalutRule rule

  -- Try open file by default editor.
  let editorRule = if contentType =~ ("^text/*" :: String)
                      then [("Open file", openByDefaultEditor filePath)]
                      else []

  -- Get system rule. 
  systemRule <- systemOpenRule filePath contentType

  return $ defaultRule ++ editorRule ++ systemRule
  
-- | The open rule that handle by system.
systemOpenRule :: FilePath -> String -> IO [(String, Client -> IO ())]
systemOpenRule filePath contentType = do
  infos <- appInfoGetAllForType contentType
  rule <- forM infos $ \info -> do 
    let appName = appInfoGetDisplayName info
        file = (fileFromPath . fromString) filePath
    return ("Open by " ++ appName, \_ -> appInfoLaunch info [file] Nothing >> return ())
  return rule

-- | Open by default editor.
openByDefaultEditor :: FilePath -> Client -> IO ()
openByDefaultEditor filePath client =  
    mkDaemonSignal client NewTab (NewTabArgs "PageEditor" filePath []) 
