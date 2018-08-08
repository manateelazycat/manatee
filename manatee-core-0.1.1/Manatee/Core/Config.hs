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

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Manatee.Core.Config where

import Control.Exception
import Data.Binary
import System.Directory
import System.FilePath
import System.IO

import qualified Control.Exception as Exc
import qualified Data.ByteString.Lazy as L

-- | Config file directly.
getConfigDirectory :: IO FilePath
getConfigDirectory = do
  home <- getHomeDirectory
  return $ home </> ".manatee"

-- | Browse history path.
browseHistoryPath :: FilePath
browseHistoryPath = "web/history"

-- | Page type path.
pageTypeRulePath :: FilePath
pageTypeRulePath = "core/pageTypeRule"

-- | File open rule.
fileOpenRulePath :: FilePath
fileOpenRulePath = "core/fileOpenRule"

-- | Page mode rule.
pageModeRulePath :: FilePath
pageModeRulePath = "core/pageModeRule"

-- | Duplicate tab list path.
pageModeDuplicateList :: FilePath
pageModeDuplicateList = "core/pageModeDuplicateList"

-- | Extension global keymap.
extensionGlobalKeymapPath :: FilePath
extensionGlobalKeymapPath = "core/extensionGlobalKeymap"

-- | Welcome Application info.
welcomeApplicationPath :: FilePath
welcomeApplicationPath = "core/welcomeApplications"

-- | State path.
statePath :: FilePath
statePath = "state"

-- | Layout path.
layoutPath :: FilePath
layoutPath = 
  statePath </> "layout"

-- | Buffer state.
bufferStatePath :: FilePath
bufferStatePath =
    "bufferState"

-- | View state.
viewStatePath :: FilePath
viewStatePath =
    "viewState"

-- | Startup filename.
subprocessStartupPath :: FilePath
subprocessStartupPath = "startup"

-- | Customize path.
configPath :: FilePath
configPath = "config"

-- | Write config file.
writeConfig :: Binary a => FilePath -> a -> IO ()
writeConfig path config = do
  configDir <- getConfigDirectory
  writeConfigPath (configDir </> path) config

-- | Write config file.
writeConfigPath :: Binary a => FilePath -> a -> IO ()
writeConfigPath configPath config = 
  bracket (openFile configPath WriteMode)        
          hClose
          (\fileHandle -> L.hPutStr fileHandle (encode config))

-- | Read config file.
readConfig :: Binary a => FilePath -> a -> IO a
readConfig path emptyConfig = do
  configDir <- getConfigDirectory
  readConfigPath (configDir </> path) emptyConfig

-- | Read config file.
readConfigPath :: Binary a => FilePath -> a -> IO a
readConfigPath configPath emptyConfig = do
  isExist <- doesFileExist configPath
  if isExist
     then 
       bracket (openFile configPath ReadMode)
               hClose
               (\ fileHandle -> do
                  -- Get file content.
                  content <- L.hGetContents fileHandle
                  -- Force evaluate file content. 
                  -- Otherwise Lazy content perhaps access after FileHandle be closed.
                  str <- Exc.evaluate content
                  -- Return empty value when file is empty, otherwise decode content.
                  if str == L.empty 
                     then return emptyConfig 
                     else 
                         -- Return empty value if decode operation failed.
                         Exc.catch (return $ decode str)
                                   (\ (_ :: IOException) -> return emptyConfig))
     else do
       -- Create directly first.
       createDirectoryIfMissing True (takeDirectory configPath)
       -- Create history file.
       bracket (openFile configPath WriteMode)
               hClose
               (`L.hPutStr` L.empty)
       -- Return empty config.
       putStrLn ("Config file " ++ configPath ++ " doesn't exist, create it.")
       return emptyConfig
