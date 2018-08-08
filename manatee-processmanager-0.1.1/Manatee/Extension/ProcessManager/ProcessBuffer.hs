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
module Manatee.Extension.ProcessManager.ProcessBuffer where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM 
import Control.Monad
import DBus.Client hiding (Signal)
import Data.Binary
import Data.DeriveTH
import Data.List
import Data.Ord
import Data.Typeable
import Graphics.UI.Gtk hiding (get)
import Manatee.Core.Config
import Manatee.Core.Types
import Manatee.Extension.ProcessManager.PageMode
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Misc
import Manatee.Toolkit.General.STM
import System.Linux.Proc hiding (ProcessState)

data ProcessBuffer =
    ProcessBuffer {processBufferName                    :: String
                  ,processBufferClient                  :: Client
                  ,processBufferPageId                  :: PageId
                  ,processBufferMode                    :: PageMode
                  ,processBufferStatus                  :: TVar [ProcStatus]
                  ,processBufferOptions                 :: [(ProcOption, SortColumnId)]
                  ,processBufferSortStatus              :: TVar (ProcOption, SortType)
                  ,processBufferBroadcastChannel        :: TChan ProcTChanSignal
                  ,processBufferDelay                   :: Int
                  ,processBufferViewCounter             :: TVar Int
                  ,processBufferState                   :: TVar ProcessState
                  }
    deriving Typeable

data ProcessState =
    ProcessState {processStateSelectedPath          :: (Maybe TreePath)
                 ,processStateScrolledPosition      :: (Double, Double)
                 }

data ProcTChanSignal = Empty
                     | UpdateProcesses ([ProcStatus], [ProcStatus], [ProcStatus])
                     | KillProcess Int
                       deriving (Show, Eq, Ord)

class ProcStatusClass a where
    getColumnTitle      :: a -> String -- get title for treeColumn 
    getColumnMaxWidth   :: a -> Maybe Int
    getCellText         :: a -> ProcStatus -> String -- get text for cell
    getCellXAlign       :: a -> Float               -- get x align for cell
    compareRow          :: a -> ProcStatus -> ProcStatus -> IO Ordering

data ProcOption = MProcessId
                | MCommand
                | MState
                | MParentProcessId
                | MProcessGroupId
                | MSessionId
                | MPriority
                | MThreads
                | MCPUPercent
                | MVirtualMemory
                | MResidentMemory
                | MCmdline
                | MUser
                  deriving (Eq, Show, Read)

instance ProcStatusClass ProcOption where
    getColumnTitle MProcessId           = "Process Id"
    getColumnTitle MCommand             = "Name"
    getColumnTitle MState               = "Status"
    getColumnTitle MParentProcessId     = "Parent Process Id"
    getColumnTitle MProcessGroupId      = "Group Id"
    getColumnTitle MSessionId           = "Session Id"
    getColumnTitle MPriority            = "Priority"
    getColumnTitle MThreads             = "Threads"
    getColumnTitle MCPUPercent          = "CPU"
    getColumnTitle MVirtualMemory       = "Virtual Memory"
    getColumnTitle MResidentMemory      = "Resident Memory"
    getColumnTitle MCmdline             = "Command line"
    getColumnTitle MUser                = "User"

    getColumnMaxWidth MProcessId           = Nothing
    getColumnMaxWidth MCommand             = Just 500
    getColumnMaxWidth MState               = Nothing
    getColumnMaxWidth MParentProcessId     = Nothing
    getColumnMaxWidth MProcessGroupId      = Nothing
    getColumnMaxWidth MSessionId           = Nothing
    getColumnMaxWidth MPriority            = Nothing
    getColumnMaxWidth MThreads             = Nothing
    getColumnMaxWidth MCPUPercent          = Nothing
    getColumnMaxWidth MVirtualMemory       = Nothing
    getColumnMaxWidth MResidentMemory      = Nothing
    getColumnMaxWidth MCmdline             = Nothing
    getColumnMaxWidth MUser                = Nothing

    getCellText MProcessId          info = show $ psProcessId info
    getCellText MCommand            info = psCommand info
    getCellText MState              info = show $ psState info
    getCellText MParentProcessId    info = show $ psParentProcessId info
    getCellText MProcessGroupId     info = show $ psProcessGroupId info
    getCellText MSessionId          info = show $ psSessionId info
    getCellText MPriority           info = showPriority $ psNice info
    getCellText MThreads            info = show $ psNumThreads info
    getCellText MCPUPercent         info = show (psCpuPercent info) ++ "%"
    getCellText MVirtualMemory      info = formatFileSizeForDisplay (fromIntegral $ psVirtualMem info) 2
    getCellText MResidentMemory     info = formatFileSizeForDisplay (fromIntegral $ psResidentMem info) 2
    getCellText MCmdline            info = psCmdline info
    getCellText MUser               info = psUsername info

    getCellXAlign MProcessId          = 1.0
    getCellXAlign MCommand            = 0.0
    getCellXAlign MState              = 0.0
    getCellXAlign MParentProcessId    = 1.0
    getCellXAlign MProcessGroupId     = 1.0
    getCellXAlign MSessionId          = 1.0
    getCellXAlign MPriority           = 1.0
    getCellXAlign MThreads            = 1.0
    getCellXAlign MCPUPercent         = 1.0
    getCellXAlign MVirtualMemory      = 1.0
    getCellXAlign MResidentMemory     = 1.0
    getCellXAlign MCmdline            = 0.0
    getCellXAlign MUser               = 1.0

    compareRow MProcessId        row1 row2 = return $ comparing psProcessId       row1 row2
    compareRow MCommand          row1 row2 = return $ comparing psCommand         row1 row2
    compareRow MState            row1 row2 = return $ comparing psState           row1 row2
    compareRow MParentProcessId  row1 row2 = return $ comparing psParentProcessId row1 row2
    compareRow MProcessGroupId   row1 row2 = return $ comparing psProcessGroupId  row1 row2
    compareRow MSessionId        row1 row2 = return $ comparing psSessionId       row1 row2
    compareRow MPriority         row1 row2 = return $ comparing psNice            row1 row2
    compareRow MThreads          row1 row2 = return $ comparing psNumThreads      row1 row2
    compareRow MCPUPercent       row1 row2 = return $ comparing psCpuPercent      row1 row2
    compareRow MVirtualMemory    row1 row2 = return $ comparing psVirtualMem      row1 row2
    compareRow MResidentMemory   row1 row2 = return $ comparing psResidentMem     row1 row2
    compareRow MCmdline          row1 row2 = return $ comparing psCmdline         row1 row2
    compareRow MUser             row1 row2 = return $ comparing psUsername        row1 row2

-- | Init state.
processInitState :: ProcessState      
processInitState = 
  ProcessState Nothing (0, 0)

-- | Create process buffer.
processBufferNew :: FilePath -> [String] -> Client -> PageId -> CustomizeWrap -> IO ProcessBuffer
processBufferNew path _ client pageId _ = do
  -- Get process status.
  infos <- procGetAllProcessStatus

  ProcessBuffer <$> pure path
                <*> pure client
                <*> pure pageId
                <*> pure processManagerMode
                <*> newTVarIO infos
                <*> pure (pairPred [MCommand
                                   ,MProcessId
                                   ,MUser
                                   ,MState
                                   ,MResidentMemory
                                   ,MCPUPercent
                                   ,MPriority
                                   ,MThreads
                                   ,MCmdline
                                   ])
                <*> newTVarIO (MResidentMemory, SortDescending)
                <*> newTChanIO
                <*> pure 3000   -- default delay is 3s
                <*> newTVarIO 0
                <*> newTVarIO processInitState

-- | Update process buffer status.
processBufferUpdate :: ProcessBuffer -> IO () 
processBufferUpdate buffer = do
   timeoutAddFull (do
                    counter <- readTVarIO $ processBufferViewCounter buffer
                    -- Just update status when proces view is running.
                    when (counter > 0) $ do
                      forkIO $ do
                        -- Update process status.
                        infos <- procGetAllProcessStatus
                        oldInfos <- readTVarIO (processBufferStatus buffer)
                        -- Get update process status.
                        let (deleteInfos, restInfos) = 
                                partition (\x -> psProcessId x `notElem` map psProcessId infos) oldInfos
                            (addInfos, diffInfos)    = diffProcessStatus infos restInfos
                        -- Update status in ProcessBuffer.
                        writeTVarIO (processBufferStatus buffer) infos
                        -- Update status in ProcessView.
                        writeTChanIO (processBufferBroadcastChannel buffer) 
                                     (UpdateProcesses (addInfos, deleteInfos, diffInfos))

                        -- Just continue update process status when have view display. 
                        counter <- readTVarIO $ processBufferViewCounter buffer
                        when (counter > 0) $ processBufferUpdate buffer
                      return ()
                    return False)
                  priorityHigh
                  (processBufferDelay buffer)
   return ()

-- | Show priority.
showPriority :: Int -> String
showPriority priority 
    | priority > 5      = "Low"
    | priority > -5     = "Normal"
    | otherwise         = "High"

-- | Diff process status.
-- Return new (add, change) status. 
diffProcessStatus :: [ProcStatus] -> [ProcStatus] -> ([ProcStatus], [ProcStatus])
diffProcessStatus [] _ = ([], [])
diffProcessStatus (x:xs) oldStatus =
  let (add, diff) = 
          case find (\y -> psProcessId x == psProcessId y) oldStatus of
            Just a  -> if x == a then ([], []) else ([], [x])
            Nothing -> ([x], [])
  in ((++) add *** (++) diff) (diffProcessStatus xs oldStatus)
    
-- | Write state.
processBufferWriteState :: ProcessBuffer -> FilePath -> IO ()
processBufferWriteState buffer path = do
  state <- readTVarIO $ processBufferState buffer
  writeConfigPath path state

-- | Read state.
processBufferReadState :: ProcessBuffer -> FilePath -> IO ()  
processBufferReadState buffer path = do
  state <- readConfigPath path processInitState
  writeTVarIO (processBufferState buffer) state
  
$(derive makeBinary ''ProcessState)

