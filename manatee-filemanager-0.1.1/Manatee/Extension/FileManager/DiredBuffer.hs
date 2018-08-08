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

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Extension.FileManager.DiredBuffer where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import DBus.Client hiding (Signal)
import Data.Binary
import Data.ByteString.UTF8
import Data.DeriveTH
import Data.List (insert, delete)
import Data.Ord (comparing)
import Data.Typeable
import Graphics.UI.Gtk hiding (get)
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Extension.FileManager.PageMode
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Misc
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.Time
import Manatee.Toolkit.Gio.Gio
import Manatee.Toolkit.Gtk.Concurrent
import System.GIO
import System.GIO.File.FileAttribute
import System.Glib.GDateTime
import System.Locale
import System.Time

import qualified Data.Map as M

data DiredBuffer = 
    DiredBuffer {diredBufferCurrentDirectory    :: TVar FilePath
                ,diredBufferClient              :: Client
                ,diredBufferPageId              :: PageId
                ,diredBufferFileInfoOptions     :: [(FileInfoOption, SortColumnId)]
                ,diredBufferFileInfos           :: TVar [DiredFileInfo]
                ,diredBufferSortStatus          :: TVar (FileInfoOption, SortType)
                ,diredBufferIconPixbufDatabase  :: TVar FileIconPixbufDatabase
                ,diredBufferFileMonitor         :: TVar FileMonitor
                ,diredBufferMode                :: PageMode
                ,diredBufferBroadcastChannel    :: TChan DiredBufferSignal
                ,diredBufferState               :: TVar DiredState
                }
    deriving Typeable

data DiredState =
    DiredState {diredStateSelectedPath          :: (Maybe TreePath)
               ,diredStateScrolledPosition      :: (Double, Double)
               }

data DiredBufferSignal = UpdateBuffer FilePath
                       | UpdateView (Maybe String)
                       | UpdateFile [DiredFileInfo] [DiredFileInfo]
                         deriving (Show, Eq, Ord)

data DiredFileInfo =
    DiredFileInfo {dfiNameDescrible    :: (String,    String)
                  ,dfiTypeDescrible    :: (FileType,  String)
                  ,dfiSizeDescrible    :: (Integer,   String)
                  ,dfiMimeDescrible    :: (String,    String)
                  ,dfiMtimeDescrible   :: (GTimeVal,  String)
                  ,dfiPermDescrible    :: (String,    String)
                  }
    deriving (Show, Eq, Ord)

data FileInfoOption = FIName | FISize | FIMime | FIMTime | FIPermission
                    deriving (Eq, Show, Read)

class DiredFileInfoClass a where
    getColumnTitle      :: a -> String                 -- get title for treeColumn 
    getCellText         :: a -> DiredFileInfo -> String -- get text for cell
    getCellXAlign       :: a -> Float                  -- get x align for cell
    compareRow          :: a -> DiredFileInfo -> DiredFileInfo -> IO Ordering -- compare row for treeView

instance DiredFileInfoClass FileInfoOption where
      getColumnTitle FIName       = "File name"
      getColumnTitle FISize       = "Size"
      getColumnTitle FIMime       = "Type"
      getColumnTitle FIMTime      = "Modification time"
      getColumnTitle FIPermission = "Permission"

      getCellText FIName       info = snd $ dfiNameDescrible  info
      getCellText FISize       info = snd $ dfiSizeDescrible  info
      getCellText FIMime       info = snd $ dfiMimeDescrible  info
      getCellText FIMTime      info = snd $ dfiMtimeDescrible info
      getCellText FIPermission info = snd $ dfiPermDescrible  info

      getCellXAlign FIName       = 0.0
      getCellXAlign FISize       = 1.0
      getCellXAlign FIMime       = 0.0
      getCellXAlign FIMTime      = 0.0
      getCellXAlign FIPermission = 0.0

      compareRow FIName       row1 row2 = return $ diredFileInfoNameCompare row1 row2
      compareRow FISize       row1 row2 = return $ comparing fst (dfiSizeDescrible  row1) (dfiSizeDescrible  row2)
      compareRow FIMime       row1 row2 = return $ comparing fst (dfiMimeDescrible  row1) (dfiMimeDescrible  row2)
      compareRow FIMTime      row1 row2 = return $ comparing fst (dfiMtimeDescrible row1) (dfiMtimeDescrible row2)
      compareRow FIPermission row1 row2 = return $ comparing fst (dfiPermDescrible  row1) (dfiPermDescrible  row2)

-- | Init state.
diredInitState :: DiredState      
diredInitState = 
  DiredState Nothing (0, 0)

-- | New.
diredBufferNew :: FilePath -> [String] -> Client -> PageId -> CustomizeWrap -> IO DiredBuffer
diredBufferNew dir _ client pageId _ = do
  -- Create file monitor.
  monitor <- fileMonitor (fileFromParseName dir) [] Nothing

  -- Create buffer.
  buffer <- DiredBuffer <$> newTVarIO dir 
                        <*> pure client 
                        <*> pure pageId 
                        <*> pure (pairPred [FIName, FISize, FIMime, FIMTime, FIPermission])
                        <*> newTVarIO []
                        <*> newTVarIO (FIName, SortAscending) 
                        <*> newTVarIO M.empty 
                        <*> newTVarIO monitor
                        <*> pure diredMode
                        <*> (newTChanIO :: IO (TChan DiredBufferSignal)) 
                        <*> newTVarIO diredInitState

  -- Load directory content.
  diredBufferLoad buffer dir

  -- Listen broadcast channel.
  diredBufferListenChannel buffer

  -- Monitor file changed event.
  diredBufferHandleFileMonitor buffer

  return buffer

-- | Listen broadcast channel.
diredBufferListenChannel :: DiredBuffer -> IO ()
diredBufferListenChannel buffer@(DiredBuffer {diredBufferClient                 = client
                                             ,diredBufferPageId                 = pageId
                                             ,diredBufferMode                   = mode
                                             ,diredBufferBroadcastChannel       = channel
                                             }) = 
  listenBufferChannel channel $ \ signal ->
    case signal of
      UpdateBuffer path -> do
         -- Send SynchronizationPathName signal to daemon process.
         mkDaemonSignal client SynchronizationPathName (SynchronizationPathNameArgs (pageModeName mode) pageId path)
         -- Update file monitor path.
         diredBufferUpdateFileMonitorPath buffer path
      _ -> return ()

-- | Load.
diredBufferLoad :: DiredBuffer -> FilePath -> IO ()
diredBufferLoad buffer dir = do
  -- Get file infos.
  infos <- directoryGetFileInfos (fromString dir)

  -- Update value.
  writeTVarIO (diredBufferCurrentDirectory buffer) dir
  writeTVarIOM (diredBufferFileInfos buffer) 
               (diredBufferGenerateFileInfos infos)
  forM_ infos $ \info -> 
      modifyTVarIOM (diredBufferIconPixbufDatabase buffer)
                    (updateFileIconPixbufDatabase info)

-- | Get file info.
diredBufferGenerateFileInfo :: FileInfo -> IO DiredFileInfo   
diredBufferGenerateFileInfo info = do
    -- Get Type.
    typeDes <- do
      let fType = fileInfoGetFileType info
          title = show fType
      return (fType, title)

    -- Get Name.
    nameDes <- do
      let fName = maybeError (fileInfoGetName info) 
                  "diredBufferGenerateFileInfos - `fileInfoGetName`." 
          title = maybeError (fileInfoGetDisplayName info)
                  "diredBufferGenerateFileInfos - `fileInfoGetDisplayName`."
      return (toString fName, title)

    -- Get Size.
    sizeDes <- do
      let fSize = toInteger $ fileInfoGetSize info
          title = formatFileSizeForDisplay fSize 2
      return (fSize, title)

    -- Get MIME.
    mimeDes <- do
      let fMime = maybeError (fileInfoGetContentType info) 
                  "diredBufferGenerateFileInfos - `fileInfoGetContentType`."
          title = contentTypeGetDescription fMime
          -- title = fMime
      return (fMime, title)

    -- Get modification time.
    mtimeDes <- do
      let fTime = fileInfoGetModificationTime info
      title <- do
        calTime <- toCalendarTime $ gTimeValToClockTime fTime
        return (formatCalendarTime defaultTimeLocale "%Y/%m/%d %T" calTime)
      return (fTime, title)

    -- Get permission.
    permissionDes <- do
      let isDir  = fileInfoGetFileType info == FileTypeDirectory
      canRead    <- fileInfoGetAttributeBool info fileAttributeAccessCanRead
      canWrite   <- fileInfoGetAttributeBool info fileAttributeAccessCanWrite
      canExecute <- fileInfoGetAttributeBool info fileAttributeAccessCanExecute
      let permission =    
              (if isDir         then "d" else "-")
           ++ (if canRead       then "r" else "-") 
           ++ (if canWrite      then "w" else "-") 
           ++ (if canExecute    then "x" else "-")
      return (permission, permission)

    return $ DiredFileInfo nameDes typeDes sizeDes mimeDes mtimeDes permissionDes

-- | Get file infos.
diredBufferGenerateFileInfos :: [FileInfo] -> IO [DiredFileInfo]
diredBufferGenerateFileInfos infos = 
  forM infos diredBufferGenerateFileInfo
  
-- | Compare file name.
diredFileInfoNameCompare :: DiredFileInfo -> DiredFileInfo -> Ordering
diredFileInfoNameCompare row1 row2 = 
  compareFileWithType (fName1, fType1) (fName2, fType2)
  where fType1 = fst $ dfiTypeDescrible row1
        fType2 = fst $ dfiTypeDescrible row2
        fName1 = fst $ dfiNameDescrible row1
        fName2 = fst $ dfiNameDescrible row2

-- | Handle file monitor event.
diredBufferHandleFileMonitor :: DiredBuffer -> IO ()        
diredBufferHandleFileMonitor buffer@(DiredBuffer {diredBufferFileMonitor = fileMonitorTVar}) = do
  fm <- readTVarIO fileMonitorTVar
  fm `on` fileMonitorChanged $ \ fileSrc _ monitorEvent -> 
      fileSrc ?>= \file -> 
             case monitorEvent of                                         
                -- Create.
                FileMonitorEventCreated -> do 
                   -- Get new add file info.
                   fileInfo <- fileQueryInfo file "*" [] Nothing
                   dFileInfo <- diredBufferGenerateFileInfo fileInfo
                   -- Update new add info.
                   modifyTVarIO (diredBufferFileInfos buffer) 
                                (insert dFileInfo)
                   -- Update icon database for new add file.
                   modifyTVarIOM (diredBufferIconPixbufDatabase buffer)
                                 (updateFileIconPixbufDatabase fileInfo)
                   -- Send `UpdateFile` signal to broadcast channel.
                   writeTChanIO (diredBufferBroadcastChannel buffer)
                                (UpdateFile [dFileInfo] [])
                -- Delete.
                FileMonitorEventDeleted -> do
                   -- FIXED: Got runtime user error, here, why?
                   -- manatee-filemanager: user error (�և�/data/test.txt���	�*���U)
                   fileInfo <- fileQueryInfo file "*" [] Nothing
                   dFileInfo <- diredBufferGenerateFileInfo fileInfo
                   -- Update delete file.
                   modifyTVarIO (diredBufferFileInfos buffer) 
                                (delete dFileInfo)
                   -- Send `UpdateFile` signal to broadcast channel.
                   writeTChanIO (diredBufferBroadcastChannel buffer)
                                (UpdateFile [] [dFileInfo])
                _ -> 
                  putStrLn ("Uncatch monitor event : " ++ show monitorEvent)
  return ()

-- | Update file monitor path.
diredBufferUpdateFileMonitorPath :: DiredBuffer -> FilePath -> IO ()                
diredBufferUpdateFileMonitorPath buffer@(DiredBuffer {diredBufferFileMonitor = fileMonitorTVar}) 
                                 newDir = do
   -- Cancel old file monitor.
   oldFileMonitor <- readTVarIO fileMonitorTVar
   fileMonitorCancel oldFileMonitor
   -- Update new file monitor.
   writeTVarIOM fileMonitorTVar (fileMonitor (fileFromParseName newDir) [] Nothing)
   -- Hook file monitor with new file.
   diredBufferHandleFileMonitor buffer
  
-- | Write state.
diredBufferWriteState :: DiredBuffer -> FilePath -> IO ()
diredBufferWriteState buffer path = do
  state <- readTVarIO $ diredBufferState buffer
  writeConfigPath path state

-- | Read state.
diredBufferReadState :: DiredBuffer -> FilePath -> IO ()  
diredBufferReadState buffer path = do
  state <- readConfigPath path diredInitState
  writeTVarIO (diredBufferState buffer) state
  
$(derive makeBinary ''DiredState)
