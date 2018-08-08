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

module Manatee.Toolkit.Gio.Gio where

import Control.Monad (liftM)
import Control.Applicative hiding (empty)
import Data.Map (Map)
import Data.List (nub)
import Data.ByteString.UTF8
import Distribution.Simple.Utils
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.General.IconTheme
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import System.FilePath
import System.GIO
import Manatee.Toolkit.Glib.GError

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import qualified Manatee.Toolkit.General.ByteString as GB

type FileContentType = String
type FileIconPixbufDatabase = Map FileContentType Pixbuf

-- | Check file is exist.
fileDoesExist :: ByteString -> Bool
fileDoesExist filepath = 
  fileQueryExists (fileFromPath filepath) Nothing

-- | Check directory is exist.
directoryDoesExist :: ByteString -> Bool
directoryDoesExist directory = 
    fileDoesExist directory && isDirectory directory

-- | Check file path whether directory.
isDirectory :: ByteString -> Bool          
isDirectory directory = 
  FileTypeDirectory == fileQueryFileType (fileFromPath directory) [] Nothing

-- | Get files from given directory.
directoryGetFiles :: ByteString -> IO [ByteString]
directoryGetFiles directory = 
  map fileInfoGetNameWithType <$> directoryGetFileInfos directory

-- | Get files recursive.
directoryGetFilesRecursive :: ByteString -> IO [ByteString]
directoryGetFilesRecursive dir = do
  infos <- directoryGetFileInfos dir
  liftM concat $ mapM (\info -> do
                         let fType = fileInfoGetFileType info
                             fName = maybeError (fileInfoGetName info)
                                     "directoryGetFilesRecursive - `fileInfoGetName`."
                             fPath = GB.combine dir fName
                         if fType == FileTypeDirectory
                            -- Get files recursively if current path is directory.
                            then directoryGetFilesRecursive fPath
                            -- Otherwise return current file.
                            else return [fPath]
                      ) infos
    
-- | Get info name with type.
fileInfoGetNameWithType :: FileInfoClass info => info -> ByteString
fileInfoGetNameWithType info 
    | fType == FileTypeDirectory 
        = B.concat [fName, B.singleton pathSeparator]
    | otherwise 
        = fName
    where fType = fileInfoGetFileType info
          fName = maybeError (fileInfoGetName info) 
                  "fileInfoGetNameWithType - `fileInfoGetName`." 

-- | Get info name with type.
fileInfoGetDisplayNameWithType :: FileInfoClass info => info -> String
fileInfoGetDisplayNameWithType info 
    | fType == FileTypeDirectory 
        = fName ++ [pathSeparator]
    | otherwise 
        = fName
    where fType = fileInfoGetFileType info
          fName = maybeError (fileInfoGetDisplayName info) 
                  "fileInfoGetDisplayNameWithType - `fileInfoGetDisplayName`." 

-- | Get display name for given filepath.
filepathGetDisplayName :: ByteString -> FilePath
filepathGetDisplayName =
  fileParseName . fileFromPath

-- | Get FileInfo list from given directory.
directoryGetFileInfos :: ByteString -> IO [FileInfo]
directoryGetFileInfos directory = catchGErrorM (return []) $ do
  let dir = fileFromPath directory
  enumerator <- fileEnumerateChildren dir "*" [] Nothing
  fileEnumeratorGetFileInfos enumerator

-- | We need use absolute *byte string* to identifier a filepath in filesystem.
-- So we need 'directory name' and 'file name' are valid 'byte string', no encoding information.
-- 'Byte string file name' can get by 'fileEnumeratorNextFile' and 'fileInfoGetName'.
-- 'Byte string directory name ' can get by 'fileEnumerateChildren' and 'fileEnumeratorGetContainer'
-- So this function is convert user input directory name (UTF8) to 'byte string'.
getDirectoryPath :: String -> IO ByteString
getDirectoryPath dirStr = do
  let dir = fileFromParseName dirStr
  enumerator <- fileEnumerateChildren dir "*" [] Nothing
  dirFile <- fileEnumeratorGetContainer enumerator
  return $ filePath dirFile

-- | Get FileInfo list from given FileEnumerator.
fileEnumeratorGetFileInfos :: FileEnumeratorClass enumerator => enumerator -> IO [FileInfo]
fileEnumeratorGetFileInfos enum = do
  fileInfo <- fileEnumeratorNextFile enum Nothing
  case fileInfo of
    Just info -> do
      infos <- fileEnumeratorGetFileInfos enum
      return $ info : infos
    Nothing -> return []
    
-- | Compare file name with file type.
-- Directory first, then sort file. 
-- All sort is alpha sort.
compareFileWithType :: (FilePath, FileType) -> (FilePath, FileType) -> Ordering
compareFileWithType (fPath1, fType1) (fPath2, fType2) 
    | fType1 == fType2 
      = compare fName1 fName2
    | fType1 == FileTypeDirectory
      = LT
    | fType2 == FileTypeDirectory
      = GT
    | otherwise 
      = compare fName1 fName2
    -- Don't care file case.
    where fName1 = lowercase fPath1        
          fName2 = lowercase fPath2

-- | Get description for file info.
fileInfoGetDescription :: FileInfoClass info => info -> String
fileInfoGetDescription info =
    case contentType of
      Just ct -> contentTypeGetDescription ct
      Nothing -> ""
    where contentType = fileInfoGetContentType info

-- | Get all execute files.
getAllExecuteFiles :: IO [ByteString]
getAllExecuteFiles = 
  catchGErrorM (return []) $ do
    paths <- liftM (filter directoryDoesExist . map fromString . nub) getSearchPath
    liftM nub $ concatMapM directoryGetFiles paths

-- | Launch command in terminal.
launchCommandInTerminal :: String -> IO ()
launchCommandInTerminal command = do
  appinfo <- appInfoCreateFromCommandline command Nothing [AppInfoCreateNeedsTerminal]
  appInfoLaunch appinfo [] Nothing
  return ()

-- | Get icon pixbuf.
fileInfoGetIconPixbuf :: FileInfoClass info => info -> IO Pixbuf
fileInfoGetIconPixbuf info = 
  getIconPixbuf =<< fileInfoGetIcon info

-- | Get icon pixbuf.
getIconPixbuf :: IconClass icon => icon -> IO Pixbuf
getIconPixbuf icon = do
  iconTheme <- iconThemeGetDefault
  iconInfo <- iconThemeLookupByGIcon iconTheme icon 24 IconLookupUseBuiltin
  case iconInfo of
    Just ii -> iconInfoLoadIcon ii
    Nothing -> do
      pixbuf <- iconThemeLoadIcon iconTheme "unknown" 24 IconLookupUseBuiltin
      return $ maybeError pixbuf "getFileIconPixbuf: can't get `unknown` icon pixbuf."

-- | Find icon pixbuf.
findIconPixbuf :: FileIconPixbufDatabase -> String -> Maybe Pixbuf
findIconPixbuf database fMime = 
  fmap snd $ findMinMatch database (\ mime _ -> mime == fMime)

-- | Update file icon database.
updateFileIconPixbufDatabase :: FileInfoClass info => info -> FileIconPixbufDatabase -> IO FileIconPixbufDatabase
updateFileIconPixbufDatabase info database = do
  let fMime = maybeError (fileInfoGetContentType info) 
              "getFileIconPixbuf: can't get file content type."  
  case findIconPixbuf database fMime of
    Just _  -> return database
    Nothing -> do
      pixbuf <- fileInfoGetIconPixbuf info
      return $ M.insert fMime pixbuf database
     
-- | Update file icon database.
updateFileIconPixbufDatabaseWithFilePath :: FilePath -> FileIconPixbufDatabase -> IO FileIconPixbufDatabase
updateFileIconPixbufDatabaseWithFilePath filepath database = do
  (_, fMime) <- contentTypeGuess filepath "" 0
  case findIconPixbuf database fMime of
    Just _  -> return database
    Nothing -> do
      pixbuf <- getIconPixbuf (contentTypeGetIcon fMime)
      return $ M.insert fMime pixbuf database
