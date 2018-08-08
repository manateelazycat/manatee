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
module Manatee.Plugin.Anything.AnythingLocate where

import DBus.Client hiding (Signal)
import Data.ByteString.UTF8 hiding (drop, length)
import Data.List
import Data.Typeable
import Manatee.Core.DBus
import Manatee.Core.FileOpenRule
import Manatee.Core.Types 
import Manatee.Plugin.Anything.Anything
import Manatee.Plugin.Anything.Types
import Manatee.Toolkit.General.FilePath
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Misc
import Manatee.Toolkit.Gio.Gio
import System.Environment
import System.FilePath
import System.GIO.Enums
import System.GIO.File.FileInfo hiding (FileInfo)

import qualified Data.ByteString.Char8 as B

data FileInfo = 
    FileInfo {fileInfoPath              :: ByteString
             ,fileInfoDisplayName       :: String
             ,fileInfoContentType       :: String
             ,fileInfoFileType          :: FileType
             ,fileInfoSize              :: String
             } deriving Typeable

instance AnythingCandidate FileInfo where
  candidateCommandName _      = ""
  candidateFilterName a       = fileInfoDisplayName a ++ " " ++ fileInfoSize a
  candidateCompletionName a _ = fileInfoDisplayName a
  candidateExpandName a input = do
      home <- getEnv "HOME"
      return $
        if ("~" :: String) `isPrefixOf` input
           -- Replace HOME with ~ if user input is beginning with ~
           then '~' : drop (length home) (toString $ fileInfoPath a)
           -- Otherwise keep user input.
           else toString $ fileInfoPath a

-- | Search local files.
anythingLocate :: Anything
anythingLocate = 
  Anything {anythingColumnTitle         = ["Locate", "Size"]
           ,anythingColumnFun           = anythingLocateColumnFun
           ,anythingSearch              = AnythingSearch (anythingLocateSearch False)
           ,anythingFilterRule          = anythingLocateFilterRule
           ,anythingCompletionRule      = takeFileName
           ,anythingInputDepend         = True
           ,anythingCommandFun          = anythingLocateCommandFun
           ,anythingCalculateDelay      = 0
           }

-- | Search local files.
anythingLocateSearch :: Bool -> AnythingInput -> Client -> IO [FileInfo]
anythingLocateSearch filterDir filepath _ = do
  home <- getEnv "HOME"
  let -- Expand '~' with HOME value. 
      path = if ("~" :: String) `isPrefixOf` filepath
                then home ++ drop (length ("~" :: String)) filepath
                else filepath
      -- Try to get upper directory of current file path.
      upperDir | hasTrailingPathSeparator path
                   = fromString path
               | otherwise
                   = fromString $ getUpperDirectory path
  anythingLocateGetFileInfos upperDir filterDir

-- | Get FileInfo.
anythingLocateGetFileInfos :: ByteString -> Bool -> IO [FileInfo]
anythingLocateGetFileInfos upperDir filterDir =
    if directoryDoesExist upperDir    -- get result.
      then do 
        -- Get file infos.
        infos <- do
             fileInfos <- directoryGetFileInfos upperDir -- just search deep when upper directory exist.
             return $
               map (\info -> 
                         FileInfo (B.concat [upperDir, fileInfoGetNameWithType info])
                                  (fileInfoGetDisplayNameWithType info)
                                  (maybeError (fileInfoGetContentType info) 
                                   "anythingInteractiveDirectorySearch - fileInfoGetContentType.")
                                  (fileInfoGetFileType info)
                                  (formatFileSizeForDisplay (toInteger $ fileInfoGetSize info) 2)) 
                   $ (\x -> 
                       if filterDir 
                          then filter (\x -> fileInfoGetFileType x == FileTypeDirectory) x
                          else x
                     ) fileInfos
        return $ sortBy (\ a b -> compareFileWithType 
                         (fileInfoDisplayName a, fileInfoFileType a)
                         (fileInfoDisplayName b, fileInfoFileType b)
                        ) infos
      else return []

-- | Rule for filter file name.
anythingLocateFilterRule :: AnythingInput -> String -> Bool
anythingLocateFilterRule input candidate = 
    -- Current path is end by path separator?
    hasTrailingPathSeparator input || 
        -- Or have same prefix with filename.
        (takeFileName input `isPrefixOf` candidate)

-- | Function for generate candidate column.
anythingLocateColumnFun :: [AnythingColumnFun]
anythingLocateColumnFun =
  [fileInfoDisplayName . anythingCandidateUnpack
  ,fileInfoSize . anythingCandidateUnpack]

-- | Function to generate command list.
anythingLocateCommandFun :: AnythingCommandFun
anythingLocateCommandFun _ wrap _
    | directoryDoesExist path
        = return [("Open directory", anythingLocateActionOpenDirectory filepath)
                 ,("Play directory", anythingLocateActionPlayDirectory filepath)]
    | otherwise = 
          fileOpenRule filepath contentType
          where path = fileInfoPath $ anythingCandidateUnpack wrap
                filepath = toString path
                contentType = fileInfoContentType $ anythingCandidateUnpack wrap

-- | Open file.
anythingLocateActionOpenFile :: FilePath -> Client -> IO ()
anythingLocateActionOpenFile path client = 
  mkDaemonSignal client NewTab (NewTabArgs "PageEditor" path [])

-- | Open directory.
anythingLocateActionOpenDirectory :: FilePath -> Client -> IO ()
anythingLocateActionOpenDirectory path client = 
  mkDaemonSignal client NewTab (NewTabArgs "PageFileManager" path [])

-- | Play directory.
anythingLocateActionPlayDirectory :: FilePath -> Client -> IO ()
anythingLocateActionPlayDirectory path client = 
  mkDaemonSignal client NewTab (NewTabArgs "PagePlayer" path [])
