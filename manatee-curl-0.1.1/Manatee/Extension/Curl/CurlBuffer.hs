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
{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Extension.Curl.CurlBuffer where

import Config.Import
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM 
import Control.Monad
import DBus.Client hiding (Signal)
import Data.Binary
import Data.DeriveTH
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Typeable
import Foreign
import Graphics.UI.Gtk hiding (get)
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.Dynload
import Manatee.Core.Types
import Manatee.Extension.Curl.PageMode
import Manatee.Extension.Curl.Types
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Concurrent
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Misc
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.Time
import Manatee.Toolkit.Gio.Gio
import Manatee.Toolkit.Gtk.Gtk
import Network.Curl hiding (CurlBuffer)
import Network.URI
import System.Directory
import System.FilePath
import System.IO
import Text.Printf
import Text.Regex.TDFA

import qualified Control.Exception as Exc
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Internal as S
import qualified Data.Map as M

data CurlBuffer =
    CurlBuffer {curlBufferName                  :: String
               ,curlBufferClient                :: Client
               ,curlBufferPageId                :: PageId
               ,curlBufferFileInfoOptions       :: [(DownloadFileOption, SortColumnId)]
               ,curlBufferFileInfos             :: TVar [DownloadFile]
               ,curlBufferSortStatus            :: TVar (DownloadFileOption, SortType)
               ,curlBufferIconPixbufDatabase    :: TVar FileIconPixbufDatabase
               ,curlBufferDownloadStatusPixbuf  :: DownloadStatusPixbuf
               ,curlBufferMode                  :: PageMode
               ,curlBufferBroadcastChannel      :: TChan CurlTChanSignal
               ,curlBufferCustomize             :: CurlCustomize
               ,curlBufferState                 :: TVar CurlState
               }
    deriving Typeable

data CurlState =
    CurlState {curlStateSelectedPath          :: (Maybe TreePath)
              ,curlStateScrolledPosition      :: (Double, Double)
              }

data CurlTChanSignal = AddDownload DownloadFile
                     | DeleteDownload String
                     | UpdateStatus
                       deriving (Show, Eq, Ord)

class CurlFileInfoClass a where
    getColumnTitle      :: a -> String                         -- get title for treeColumn 
    getCellXAlign       :: a -> Float                          -- get x align for cell
    getCellWidthChars   :: a -> Int                            -- get min width for cell
    getCellMaxWidth     :: a -> Maybe Int                      -- get max width for cell
    getCellText         :: a -> DownloadFile -> IO String       -- get text for cell
    compareRow          :: a -> DownloadFile -> DownloadFile -> IO Ordering -- compare row for treeView

instance CurlFileInfoClass DownloadFileOption where
  getColumnTitle        DFName          = "File Name"
  getColumnTitle        DFSize          = "Size"
  getColumnTitle        DFDownloadSize  = "Download"
  getColumnTitle        DFRestSize      = "Rest Size"
  getColumnTitle        DFSpeed         = "Speed"
  getColumnTitle        DFRestTime      = "Rest Time"
  getColumnTitle        DFThread        = "Thread"
  getColumnTitle        DFURL           = "URL"

  getCellXAlign        DFName          = 0.0
  getCellXAlign        DFSize          = 1.0
  getCellXAlign        DFDownloadSize  = 1.0
  getCellXAlign        DFRestSize      = 1.0
  getCellXAlign        DFSpeed         = 1.0
  getCellXAlign        DFRestTime      = 1.0
  getCellXAlign        DFThread        = 1.0
  getCellXAlign        DFURL           = 0.0

  getCellWidthChars        DFName          = -1
  getCellWidthChars        DFSize          = length ("xxx.xx KB" :: String)
  getCellWidthChars        DFDownloadSize  = length ("xxx.xx KB" :: String)
  getCellWidthChars        DFRestSize      = length ("xxx.xx KB" :: String)
  getCellWidthChars        DFSpeed         = length ("xxx.xx Kb/s" :: String)
  getCellWidthChars        DFRestTime      = length ("xxxd xxh xxm xxs" :: String)
  getCellWidthChars        DFThread        = -1
  getCellWidthChars        DFURL           = -1

  getCellMaxWidth        DFName          = Just 300
  getCellMaxWidth        DFSize          = Nothing
  getCellMaxWidth        DFDownloadSize  = Nothing
  getCellMaxWidth        DFRestSize      = Nothing
  getCellMaxWidth        DFSpeed         = Nothing
  getCellMaxWidth        DFRestTime      = Nothing
  getCellMaxWidth        DFThread        = Nothing
  getCellMaxWidth        DFURL           = Nothing

  getCellText   DFName          file = downloadFileGetFileName file
  getCellText   DFSize          file = fmap formatSize $ downloadFileGetSize file
  getCellText   DFDownloadSize  file = fmap formatSize $ downloadFileGetDownloadSize file
  getCellText   DFRestSize      file = fmap formatSize $ downloadFileGetRestSize file
  getCellText   DFSpeed         file = do
      downloadStatus <- readTVarIO (dfDownloadStatus file)
      if downloadStatus == Running
         then fmap formatSpeed $ downloadFileGetSpeed file
         else return ""
  getCellText   DFRestTime      file = do
      downloadStatus <- readTVarIO (dfDownloadStatus file)
      if downloadStatus == Running
         then fmap secondToDaytime $ downloadFileGetRestTime file
         else return ""
  getCellText   DFThread        file = do
      downloadStatus <- readTVarIO (dfDownloadStatus file)
      if downloadStatus == Running
         then fmap show $ downloadFileGetThread file
         else return ""
  getCellText   DFURL           file = downloadFileGetURL file

  compareRow DFName             row1 row2 = 
      liftA2 compare (downloadFileGetFileName row1) (downloadFileGetFileName row2)
  compareRow DFSize             row1 row2 = 
      liftA2 compare (downloadFileGetSize row1) (downloadFileGetSize row2)
  compareRow DFDownloadSize     row1 row2 = 
      liftA2 compare (downloadFileGetDownloadSize row1) (downloadFileGetDownloadSize row2)
  compareRow DFRestSize         row1 row2 = 
      liftA2 compare (downloadFileGetRestSize row1) (downloadFileGetRestSize row2)
  compareRow DFSpeed            row1 row2 = 
      liftA2 compare (downloadFileGetSpeed row1) (downloadFileGetSpeed row2)
  compareRow DFRestTime         row1 row2 = 
      liftA2 compare (downloadFileGetRestTime row1) (downloadFileGetRestTime row2)
  compareRow DFThread           row1 row2 = 
      liftA2 compare (downloadFileGetThread row1) (downloadFileGetThread row2)
  compareRow DFURL              row1 row2 =
      liftA2 compare (downloadFileGetURL row1) (downloadFileGetURL row2)

-- | Default download directory.
defaultDownloadDir :: FilePath
defaultDownloadDir = 
    "download"

-- | Default cache directory.
defaultCacheDir :: FilePath
defaultCacheDir = 
    "download/cache"

-- | Init state.
curlInitState :: CurlState      
curlInitState = 
  CurlState Nothing (0, 0)

-- | New.
curlBufferNew :: FilePath -> [String] -> Client -> PageId -> CustomizeWrap -> IO CurlBuffer
curlBufferNew name urls client pageId c = do
  -- Get customize option.
  let customize = castCustomize c

  -- Create directory if missing.
  configDir <- getConfigDirectory
  createDirectoryIfMissing True (configDir </> defaultDownloadDir)
  createDirectoryIfMissing True (configDir </> defaultCacheDir)
  
  -- Create buffer.
  buffer <- 
      CurlBuffer <$> pure name
                 <*> pure client
                 <*> pure pageId
                 <*> pure (pairPred [DFName, DFSize, DFSpeed, DFRestTime, DFThread, DFURL])
                 <*> newTVarIO []
                 <*> newTVarIO (DFName, SortAscending)
                 <*> newTVarIO M.empty 
                 <*> (DownloadStatusPixbuf 
                      <$> pixbufNewFromIcon "gtk-media-pause" 16
                      <*> pixbufNewFromIcon "gtk-media-play-ltr" 16
                      <*> pixbufNewFromIcon "gtk-apply" 16
                      <*> pixbufNewFromIcon "gtk-cancel" 16)
                 <*> pure curlMode
                 <*> (newTChanIO :: IO (TChan CurlTChanSignal)) 
                 <*> pure customize
                 <*> newTVarIO curlInitState

  -- Scan cache file to resume.
  curlBufferScanCacheFile buffer

  -- Add download.
  logPathes <- getLogPathes
  logUrls   <- 
      forM logPathes $ \ logPath -> 
          fmap dlURL $ readConfig logPath (DownloadLog "" "" 0 M.empty)

  forM_ urls $ \ url -> 
    -- Just start new url not cache in disk.
    when (url `notElem` logUrls) $
         curlBufferAddDownload buffer url

  -- Listen DBus signal.
  daemonClient <- mkSessionClientWithName (packGenericBusName "curl")
  mkGenericDaemonMatchRule 
    daemonClient "curl"
    (Generic, \ (GenericArgs command options) -> 
         curlBufferListenDBus buffer command options)

  return buffer

-- | Listen DBus signal.
curlBufferListenDBus :: CurlBuffer -> String -> [String] -> IO ()
curlBufferListenDBus curlBuffer command options = 
  case command of
    -- Add download. 
    "Download" -> forM_ options (curlBufferAddDownload curlBuffer)
    -- Print error information if mismatch command.
    _ -> putStrLn $ "Invalid curl command : " ++ command

-- | Add download.
curlBufferAddDownload :: CurlBuffer -> String -> IO ()
curlBufferAddDownload curlBuffer@(CurlBuffer 
                                  {curlBufferCustomize =
                                       CurlCustomize 
                                       {curlCustomizeDefaultThreadNumber = threadNumberTVar}}) 
                      uri = 
  case parseURI uri of
    Nothing -> putStrLn $ "Malformed URI : " ++ uri
    Just _  -> do
      -- Pick up file name.
      let file = takeFileName $ uriPath $ fromJust $ parseURI uri

      -- Create DownloadFile.
      downloadFile <- 
          DownloadFile <$> pure uri 
                       <*> newTVarIO file 
                       <*> newTVarIO 0
                       <*> newTVarIO 0.0
                       <*> newTVarIO 0
                       <*> newTVarIO 0
                       <*> newTVarIO []
                       <*> newEmptyMVar
                       <*> newTVarIO Running
                       <*> newTVarIO 0

      -- Add DownloadFile in CurlBuffer/CurlView.
      modifyTVarIO (curlBufferFileInfos curlBuffer) (\x -> downloadFile : x)
      writeTChanIO (curlBufferBroadcastChannel curlBuffer) (AddDownload downloadFile)

      -- Start download.
      forkIO_ $ do
        -- Fetch file size fist.
        size <- getFileSize uri

        if size == 0
           -- Stop download when file size is zero.
           then putStrLn $ "Size is zero, stop download " ++ uri
           -- Otherwise start download.
           else do
             -- Update size.
             writeTVarIO (dfSize downloadFile) size

             -- Update DownloadBuffer.
             defaultThreadNumber <- readTVarIO threadNumberTVar
             buffers <- initDownloadBuffers size defaultThreadNumber
             writeTVarIO (dfBuffers downloadFile) buffers

             -- Start speed calculate.
             startSpeedCalculate downloadFile

             -- Fork threads to download different part of file.
             forM_ buffers (\ buffer -> do
                              -- Record log file first.
                              recordLogFile downloadFile buffer
                              -- Start thread.
                              forkIO_ $ getFile uri downloadFile curlBuffer buffer (dbRange buffer))

             -- Wait result.
             curlBufferWaitResult downloadFile

-- | Delete download.
curlBufferDeleteDownload :: CurlBuffer -> DownloadFile -> IO ()
curlBufferDeleteDownload curlBuffer downloadFile = do
  -- Update Failed status, thread will stop immediately.
  let url = dfURL downloadFile
  writeTVarIO (dfDownloadStatus downloadFile) Failed

  -- Clean temp file.
  cleanTempFile downloadFile

  -- Delete DownloadFile from CurlBuffer.
  modifyTVarIO (curlBufferFileInfos curlBuffer) $ \ list -> 
    filter (\x -> dfURL x /= url) list

  -- Delete DownloadFile from CurlView.
  writeTChanIO (curlBufferBroadcastChannel curlBuffer) (DeleteDownload url)

-- | Init DownloadBuffer.
initDownloadBuffers :: Int -> Int -> IO [DownloadBuffer]
initDownloadBuffers size threadNum = do
  -- Get download range of thread.
  let ranges = getDownloadRange size threadNum 
  zipWithIndexM ranges $ \ (a, b) index -> 
    DownloadBuffer <$> pure index 
                   <*> pure (a, b)
                   <*> newTVarIO Nothing
                   <*> pure 0
                   <*> newTVarIO 0
                   <*> newTVarIO 0
                   <*> newTVarIO 0
                   <*> newTVarIO False
    
-- | Scan cache file to resume.
curlBufferScanCacheFile :: CurlBuffer -> IO ()
curlBufferScanCacheFile curlBuffer@(CurlBuffer 
                                    {curlBufferCustomize = 
                                         CurlCustomize 
                                         {curlCustomizeAutoStart = autoStartTVar}}) = 
  forkIO_ $ do
    -- Get log pathes.
    logPathes <- getLogPathes

    -- Scan log patch to pick cache information.
    forM_ logPathes $ \ logPath -> 
        forkIO_ $ do
          autoStart <- readTVarIO autoStartTVar
          -- Get DownloadLog.
          let emptyLog = DownloadLog "" "" 0 M.empty
          downloadLog@(DownloadLog {dlBuffers = logBufferMap}) <- readConfig logPath emptyLog
          
          -- Pick DownloadBuffer from DownloadLog.
          buffers <- pickDownloadBuffers logBufferMap
          
          -- Init new DownloadFile.
          downloadFile <- 
              DownloadFile <$> pure (dlURL downloadLog)
                           <*> newTVarIO (dlName downloadLog)
                           <*> newTVarIO (dlSize downloadLog)
                           <*> (do 
                                 cacheSize <- fmap sum $ forM buffers $ \buffer -> readTVarIO (dbCacheSize buffer)
                                 newTVarIO $ i2d (cacheSize * 100) / i2d (dlSize downloadLog))
                           <*> newTVarIO 0
                           <*> newTVarIO 0
                           <*> newTVarIO buffers
                           <*> newEmptyMVar
                           <*> newTVarIO (if autoStart then Running else Pause)
                           <*> newTVarIO 0
          
          -- Update DownloadFile in CurlBuffer/CurlView.
          modifyTVarIO (curlBufferFileInfos curlBuffer) (\x -> downloadFile : x)
          writeTChanIO (curlBufferBroadcastChannel curlBuffer) (AddDownload downloadFile)
          
          -- Start download if option 'autoStart' is enable.
          when autoStart $ downloadFileStart curlBuffer downloadFile buffers

-- | Wait download result.
curlBufferWaitResult :: DownloadFile -> IO ()
curlBufferWaitResult downloadFile@(DownloadFile {dfURL = url}) = do
  e <- takeMVar (dfFinishLock downloadFile)
  case e of
    -- Kill thread if DownloadError.
    DownloadError err -> do
        buffers <- readTVarIO (dfBuffers downloadFile)
        forM_ buffers (\ buffer -> 
                           readTVarIO (dbThreadId buffer)
                                          >?>= \ i -> killThread i)
        putStrLn $ "Failed to connect: " ++ url ++ " with reason (" ++ err ++ ")"
    DownloadPause  -> putStrLn $ "Fetch pause : " ++ url
    DownloadFinish -> putStrLn $ "Fetch complete : " ++ url

-- | Pause.
curlBufferPause :: DownloadFile -> IO ()
curlBufferPause downloadFile = do
  -- Just pause when status is 'Running'.
  downloadStatus <- readTVarIO (dfDownloadStatus downloadFile) 
  when (downloadStatus == Running) $ 
       writeTVarIO (dfDownloadStatus downloadFile) Pause

-- | Continue download.
curlBufferContinue :: CurlBuffer -> DownloadFile -> IO ()
curlBufferContinue curlBuffer downloadFile = do
  downloadStatus <- readTVarIO (dfDownloadStatus downloadFile) 
  when (downloadStatus == Pause) $ 
    forkIO_ $ do
      let url = dfURL downloadFile
      buffers <- readTVarIO (curlBufferFileInfos curlBuffer)
      find (\x -> dfURL x == url) buffers 
           ?>= \ downloadFile -> do
                -- Get download log.
                logPath <- getFileLogFile downloadFile
                let emptyLog = DownloadLog "" "" 0 M.empty
                (DownloadLog {dlBuffers = logBufferMap}) <- readConfig logPath emptyLog
                
                -- Pick DownloadBuffer from DownloadLog.
                buffers <- pickDownloadBuffers logBufferMap
                
                -- Update DownloadFile for ready download.
                writeTVarIO (dfBuffers downloadFile) buffers
                tryTakeMVar (dfFinishLock downloadFile)
                curlBufferChangeStatus curlBuffer downloadFile Running
                
                -- Start download.
                downloadFileStart curlBuffer downloadFile buffers

-- | Change download status.
curlBufferChangeStatus :: CurlBuffer -> DownloadFile -> DownloadStatus -> IO ()
curlBufferChangeStatus curlBuffer downloadFile status = do
  -- Change download status.
  writeTVarIO (dfDownloadStatus downloadFile) status

  -- Update in CurlView.
  writeTChanIO (curlBufferBroadcastChannel curlBuffer) UpdateStatus

-- | Get log paths.
getLogPathes :: IO [String]
getLogPathes = do
  cacheDir <- fmap (</> defaultCacheDir) getConfigDirectory
  files    <- getDirectoryContents cacheDir
  return $ map (\x -> cacheDir </> x) (filter (\x -> x =~ ("^*_log$" :: String)) files)

-- | Get download range with thread number.
getDownloadRange :: Int -> Int -> [(Int, Int)]  
getDownloadRange size threadNum =
    map (\ n -> (n * p, min (size - 1) ((n + 1) * p - 1))) [0.. (threadNum - 1)]
    where p = ceiling (i2d size / i2d threadNum)

-- | Get file size.
getFileSize :: String -> IO Int
getFileSize url = do
  -- Init curl.
  h <- initialize

  -- Set option.
  setDefaultSSLOpts h url
  setopt h (CurlURL url)
  setopt h (CurlNoProgress True)
  setopt h (CurlNoBody True)
  setopt h (CurlFailOnError True)
  setopt h (CurlWriteFunction (\ _ sz n _ -> return (sz * n)))

  -- Perform.
  _ <- perform h

  -- Get file size.
  infoValue <- getInfo h ContentLengthDownload
  return $ case infoValue of
             IDouble s -> floor s
             _         -> error "getFileSize: impossible type!"

-- | Get file.
getFile :: String -> DownloadFile -> CurlBuffer -> DownloadBuffer -> (Int, Int) -> IO ()
getFile url downloadFile curlBuffer downloadBuffer (start, end) = do
  -- Init.
  tId <- myThreadId
  writeTVarIO (dbThreadId downloadBuffer) (Just tId)
  let range = show start ++ "-" ++ show end
      lock  = dfFinishLock downloadFile
      cacheSizeTVar = curlCustomizeDefaultCacheSize $ curlBufferCustomize curlBuffer 

  -- Init curl.
  h <- initialize

  -- Create buffer cache.
  cache <- newBufferCache
  ref   <- newTVarIO cache

  -- Set option.
  setDefaultSSLOpts h url
  setopt h (CurlFailOnError True)
  setopt h (CurlURL url)
  setopt h (CurlRange range)
  setopt h (CurlNoProgress False)
  -- setopt h (CurlVerbose True)   -- debug
  setopt h (CurlWriteFunction (gather ref cacheSizeTVar downloadFile downloadBuffer))
  setopt h (CurlProgressData nullPtr)
  setopt h (CurlProgressFunction 
            (\ _ _ download _ _ -> do
               -- Get download offset.
               let offset = dbDownloadOffset downloadBuffer
               -- Update download size.
               writeTVarIO (dbDownloadSize downloadBuffer) (offset + floor download)

               -- Update download progress.
               buffers <- readTVarIO (dfBuffers downloadFile)
               downloadSize <- fmap sum $ forM buffers $ \ buffer -> readTVarIO (dbDownloadSize buffer)
               fileSize <- readTVarIO (dfSize downloadFile)
               writeTVarIO (dfProgress downloadFile) (i2d (downloadSize * 100) / i2d fileSize)
               writeTChanIO (curlBufferBroadcastChannel curlBuffer) UpdateStatus

               -- Stop thread if download status is not 'Running'.
               downloadStatus <- readTVarIO (dfDownloadStatus downloadFile)
               return $ case downloadStatus of
                          Running -> 0
                          -- Return non-zero value will abort transfer immediately.
                          _   -> 1
            ))

  -- Perform.
  rc <- perform h
  BufferCache buf' size <- readTVarIO ref

  downloadStatus <- readTVarIO (dfDownloadStatus downloadFile)
  if rc == CurlAbortedByCallback && downloadStatus == Pause 
     -- Flush cache to disk if pause by user.
     then do
       -- Flush cache.
       flushBufferCache downloadFile downloadBuffer (buf', size)

       -- Return DownloadPause result.
       tryPutMVar lock DownloadPause >> return ()
     else if rc /= CurlOK
         -- Clean temp file if download failed.
         then do
           -- Update download status.
           curlBufferChangeStatus curlBuffer downloadFile Failed

           -- Clean temp file.
           cleanTempFile downloadFile

           -- Free memory.
           free buf'

           -- Return DownloadError result.
           tryPutMVar lock (DownloadError (show rc)) >> return ()
         else do
           -- Update finish lock.
           writeTVarIO (dbIsFinish downloadBuffer) True

           -- Flush cache to disk.
           flushBufferCache downloadFile downloadBuffer (buf', size)

           -- Get download buffer information.
           buffers  <- readTVarIO (dfBuffers downloadFile)
           finishes <- forM buffers $ \ buffer -> readTVarIO (dbIsFinish buffer)

           -- Generate file if all part fetch complete.
           when (all id finishes) $ do
                -- Update progress.
                writeTVarIO (dfProgress downloadFile) 100.0

                -- Update download status.
                curlBufferChangeStatus curlBuffer downloadFile Finish
         
                -- Generate file.
                generateFile downloadFile
         
                -- Clean temp file.
                cleanTempFile downloadFile
         
                -- Return DownloadFinish result.
                tryPutMVar lock DownloadFinish >> return ()

-- | Gather network data.
gather :: TVar BufferCache -> TVar Int -> DownloadFile -> DownloadBuffer -> WriteFunction
gather tvar cacheSizeTVar downloadFile downloadBuffer = 
  writer $ \(srcPtr, bytes) -> do
    -- Copy memory chunks of data into bytestring.
    BufferCache destPtr offset <- readTVarIO tvar
    let newOffset = offset + bytes
    newDestPtr <- reallocBytes destPtr newOffset
    S.memcpy (newDestPtr `plusPtr` offset) srcPtr (fromIntegral bytes)

    defaultCacheSize <- readTVarIO cacheSizeTVar
    if newOffset >= defaultCacheSize 
       -- Flush cache to disk if data reach cache size.
       then do
         -- Update new cache.
         writeTVarIOM tvar newBufferCache

         -- Flush cache to disk.
         flushBufferCache downloadFile downloadBuffer (newDestPtr, newOffset)
       else 
           -- Update data and offset.
           writeTVarIO tvar (BufferCache newDestPtr newOffset)

-- | Copy memory chunks of data into our bytestring.
writer :: ((Ptr Word8, Int) -> IO ()) -> WriteFunction
writer f srcPtr size nelems _ = do
    let bytes = size * nelems
    f (castPtr srcPtr, fromIntegral bytes)
    return bytes

-- | Flush buffer cache.
flushBufferCache :: DownloadFile -> DownloadBuffer -> (Ptr Word8, Int) -> IO ()
flushBufferCache downloadFile downloadBuffer (ptr, size) = do
  -- Get cache bytes.
  fp <- newForeignPtr finalizerFree ptr
  let dat = S.fromForeignPtr fp 0 (fromIntegral size)

  -- Flush cache to disk.
  filepath <- getBufferCacheFile downloadFile downloadBuffer
  Exc.bracket (openFile filepath AppendMode)
              hClose
              (`B.hPutStr` dat)

  -- Update cache size.
  modifyTVarIO (dbCacheSize downloadBuffer) (+ size)

  -- Record log file.
  recordLogFile downloadFile downloadBuffer

  -- Debug.
  -- counter <- tickTVarIO (dfFlushCounter downloadFile)
  -- putStrLn $ "Flush " ++ show counter

-- | Record log file.
recordLogFile :: DownloadFile -> DownloadBuffer -> IO ()
recordLogFile downloadFile downloadBuffer = do
  -- Get DownloadLog.
  logPath <- getFileLogFile downloadFile
  let emptyLog = DownloadLog "" "" 0 M.empty
  (DownloadLog {dlBuffers = buffers}) <- readConfig logPath emptyLog
  
  -- Build new log.
  newLog <- 
    DownloadLog <$> pure (dfURL downloadFile) 
                <*> readTVarIO (dfName downloadFile) 
                <*> readTVarIO (dfSize downloadFile) 
                <*> (do
                      logBuffer <- 
                          DownloadLogBuffer <$> pure (dbRange downloadBuffer)
                                            <*> readTVarIO (dbCacheSize downloadBuffer)
                                            <*> readTVarIO (dbIsFinish downloadBuffer)
                      return $ M.insert (dbIndex downloadBuffer) logBuffer buffers)
                               
  -- Update cache log.
  writeConfig logPath newLog

-- | Get buffer cache file.
getBufferCacheFile :: DownloadFile -> DownloadBuffer -> IO FilePath 
getBufferCacheFile downloadFile downloadBuffer = do
  cacheDir <- fmap (</> defaultCacheDir) getConfigDirectory
  file <- readTVarIO (dfName downloadFile)
  return $ cacheDir </> file ++ printf "_cache_%d" (dbIndex downloadBuffer)

-- | Get file log file.
getFileLogFile :: DownloadFile -> IO FilePath
getFileLogFile downloadFile = do
  cacheDir <- fmap (</> defaultCacheDir) getConfigDirectory
  file <- readTVarIO (dfName downloadFile)
  return $ cacheDir </> file ++ "_log"

-- | New buffer cache.
newBufferCache :: IO BufferCache
newBufferCache = 
  BufferCache <$> mallocBytes 1024
              <*> pure 0

-- | Generate file.
generateFile :: DownloadFile -> IO ()
generateFile downloadFile = do
  -- Init.
  buffers     <- readTVarIO (dfBuffers downloadFile)
  cacheFiles  <- forM buffers $ \ buffer -> getBufferCacheFile downloadFile buffer
  downloadDir <- fmap (</> defaultDownloadDir) getConfigDirectory
  file <- readTVarIO (dfName downloadFile)
  
  -- Concat cache file.
  forM cacheFiles BL.readFile
      >>= \ caches -> BL.writeFile (downloadDir </> file) (BL.concat caches)

-- | Clean temp file.
cleanTempFile :: DownloadFile -> IO ()
cleanTempFile downloadFile = do
  -- Get cache files.
  buffers    <- readTVarIO (dfBuffers downloadFile)
  cacheFiles <- forM buffers $ \ buffer -> getBufferCacheFile downloadFile buffer

  -- Remove cache file.
  forM_ cacheFiles (\ file -> whenM (doesFileExist file) (removeFile file))

  -- Remove log file.
  logFile <- getFileLogFile downloadFile
  whenM (doesFileExist logFile) (removeFile logFile)

-- | Start speed calculate.
startSpeedCalculate :: DownloadFile -> IO ()
startSpeedCalculate downloadFile = 
    timeoutAdd 
    (do
      -- Init.
      buffers <- readTVarIO (dfBuffers downloadFile)

      -- Get download size.
      downloadSize <- 
          fmap sum $ forM buffers $ \ buffer -> readTVarIO (dbDownloadSize buffer)

      -- Update download speed.
      speed <- 
          fmap sum $ forM buffers $ \ buffer -> do
              currentSize <- readTVarIO (dbDownloadSize buffer)
              recordSize <- readTVarIO (dbRecordSize buffer)
              return (currentSize - recordSize)
      writeTVarIO (dfSpeed downloadFile) speed
      
      -- Update rest time.
      totalSize  <- readTVarIO (dfSize downloadFile)
      let second = floor (i2d (totalSize - downloadSize) / i2d speed)
      writeTVarIO (dfRestTime downloadFile) second
      
      -- Update record size for next calculate.
      forM_ buffers $ \ buffer -> do
              currentSize <- readTVarIO (dbDownloadSize buffer)
              writeTVarIO (dbRecordSize buffer) currentSize

      -- Continue calculate when status is 'Running'
      downloadStatus <- readTVarIO (dfDownloadStatus downloadFile)
      return (downloadStatus == Running)
    ) 1000 >> return ()

-- | Get file name.
downloadFileGetFileName :: DownloadFile -> IO FilePath
downloadFileGetFileName = return . takeFileName . uriPath . fromJust . parseURI . dfURL 

-- | Get size.
downloadFileGetSize :: DownloadFile -> IO Int
downloadFileGetSize = readTVarIO . dfSize

-- | Get download size (bytes).
downloadFileGetDownloadSize :: DownloadFile -> IO Int
downloadFileGetDownloadSize file = do
    size <- readTVarIO $ dfSize file
    progress <- readTVarIO $ dfProgress file
    return $ floor $ i2d size * progress / 100.0
  
-- | Get rest size (bytes). 
downloadFileGetRestSize :: DownloadFile -> IO Int
downloadFileGetRestSize file = do
    size <- readTVarIO $ dfSize file
    progress <- readTVarIO $ dfProgress file
    return $ floor $ i2d size * (100.0 - progress) / 100.0

-- | Get thread number.
downloadFileGetThread :: DownloadFile -> IO Int
downloadFileGetThread file = do
  buffers <- readTVarIO (dfBuffers file)
  fmap length $ filterM (fmap not . readTVarIO . dbIsFinish) buffers

-- | Get speed (bytes). 
downloadFileGetSpeed :: DownloadFile -> IO Int
downloadFileGetSpeed file = 
    readTVarIO $ dfSpeed file

-- | Get rest time (second). 
downloadFileGetRestTime :: DownloadFile -> IO Int
downloadFileGetRestTime file = 
    readTVarIO $ dfRestTime file

-- | Get URL.
downloadFileGetURL :: DownloadFile -> IO String
downloadFileGetURL = return . dfURL

-- | Start download.
downloadFileStart :: CurlBuffer -> DownloadFile -> [DownloadBuffer] -> IO ()
downloadFileStart curlBuffer downloadFile buffers = do
  -- Start speed calculate.
  startSpeedCalculate downloadFile
      
  -- Start new threads to download.
  forM_ buffers $ \ buffer -> do
    isFinish <- readTVarIO (dbIsFinish buffer)
    -- Just start thread haven't finish.
    unless isFinish $ do
      -- Get range and cache size.
      let (start, end) = dbRange buffer
      cacheSize <- readTVarIO (dbCacheSize buffer)
      -- Download data from cache size.
      forkIO_ $ getFile (dfURL downloadFile) downloadFile 
                        curlBuffer buffer 
                        (start + cacheSize, end)

  -- Wait download result.
  curlBufferWaitResult downloadFile

-- | Format size.
formatSize :: Int -> String
formatSize size = 
  formatFileSizeForDisplay (fromIntegral size) 2

-- | Format size.
formatSpeed :: Int -> String
formatSpeed size = 
  formatFileSizeForDisplay (fromIntegral size) 2 ++ "/s"

-- | Pick download buffer.
pickDownloadBuffers :: Map Int DownloadLogBuffer -> IO [DownloadBuffer]
pickDownloadBuffers logBufferMap = 
    forM (M.toList logBufferMap) 
         (\ (index, DownloadLogBuffer {dlbRange         = range
                                      ,dlbCacheSize     = cacheSize
                                      ,dlbIsFinish      = isFinish}) -> 
          DownloadBuffer <$> pure index 
                         <*> pure range 
                         <*> newTVarIO Nothing
                         <*> pure cacheSize
                         <*> newTVarIO cacheSize
                         <*> newTVarIO cacheSize
                         <*> newTVarIO cacheSize
                         <*> newTVarIO isFinish)
  
-- | Curl customize new.
curlCustomizeNew :: IO CustomizeWrap
curlCustomizeNew =
  fmap CustomizeWrap $
       CurlCustomize <$> newTVarIO defaultThreadNumber
                     <*> newTVarIO defaultCacheSize
                     <*> newTVarIO autoStart

-- | Write state.
curlBufferWriteState :: CurlBuffer -> FilePath -> IO ()
curlBufferWriteState buffer path = do
  state <- readTVarIO $ curlBufferState buffer
  writeConfigPath path state

-- | Read state.
curlBufferReadState :: CurlBuffer -> FilePath -> IO ()  
curlBufferReadState buffer path = do
  state <- readConfigPath path curlInitState
  writeTVarIO (curlBufferState buffer) state
  
$(derive makeBinary ''CurlState)

