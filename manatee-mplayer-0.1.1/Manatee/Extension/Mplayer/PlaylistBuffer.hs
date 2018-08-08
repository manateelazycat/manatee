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
module Manatee.Extension.Mplayer.PlaylistBuffer where

import Audio.TagLib.TagLib
import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad (liftM, unless)
import DBus.Client hiding (Signal)
import Data.Binary
import Data.ByteString.UTF8
import Data.DeriveTH
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Data.Time
import Data.Typeable
import Graphics.UI.Gtk hiding (get)
import Manatee.Core.Config
import Manatee.Core.DBus
import Manatee.Core.Types
import Manatee.Extension.Mplayer.DBus
import Manatee.Extension.Mplayer.PageMode
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.DBus
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.Process
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gio.Gio
import System.FilePath
import System.GIO.File.File 
import System.GIO.File.FileInfo 
import System.Posix.Process
import Text.Regex.TDFA

data PlaylistBuffer =
    PlaylistBuffer {playlistBufferName                  :: TVar String
                   ,playlistBufferClient                :: Client
                   ,playlistBufferPageId                :: PageId
                   ,playlistBufferMode                  :: PageMode
                   ,playlistBufferInfos                 :: TVar [MultimediaInfo]
                   ,playlistBufferOptions               :: [(MultimediaOption, SortColumnId)]
                   ,playlistBufferSortStatus            :: TVar (MultimediaOption, SortType)
                   ,playlistBufferState                 :: TVar PlaylistState
                   }
    deriving Typeable

data PlaylistState =
    PlaylistState {playlistStateSelectedPath          :: (Maybe TreePath)
                    ,playlistStateScrolledPosition      :: (Double, Double)
                    }

data MultimediaInfo =
    MultimediaInfo {miFilePath          :: ByteString
                   ,miArtist            :: String
                   ,miTitle             :: String
                   ,miAlbum             :: String
                   ,miTrack             :: Int
                   ,miGenre             :: String
                   ,miYear              :: Int
                   ,miDuration          :: Int
                   ,miBitRate           :: Int
                   ,miSampleRate        :: Int
                   ,miChannels          :: Int
                   ,miComment           :: String
                   }
    deriving Show

class MultimediaInfoClass a where
    getColumnTitle      :: a -> String                  -- get title for treeColumn 
    getColumnMaxWidth   :: a -> Maybe Int
    getCellText         :: a -> MultimediaInfo -> String -- get text for cell
    getCellXAlign       :: a -> Float                   -- get x align for cell
    compareRow          :: a -> MultimediaInfo -> MultimediaInfo -> IO Ordering

instance MultimediaInfoClass MultimediaOption where
    getColumnTitle MOArtist     = "Artist"
    getColumnTitle MOTitle      = "Title"
    getColumnTitle MOAlbum      = "Album"
    getColumnTitle MOTrack      = "Track"
    getColumnTitle MOGenre      = "Genre"
    getColumnTitle MOYear       = "Year"
    getColumnTitle MODuration   = "Duration"
    getColumnTitle MOBitRate    = "Bit Rate"
    getColumnTitle MOSampleRate = "Sample Rate"
    getColumnTitle MOChannels   = "Channels"
    getColumnTitle MOComment    = "comment"

    getColumnMaxWidth MOArtist     = Just 200
    getColumnMaxWidth MOTitle      = Just 400
    getColumnMaxWidth MOAlbum      = Just 400
    getColumnMaxWidth MOTrack      = Nothing
    getColumnMaxWidth MOGenre      = Nothing
    getColumnMaxWidth MOYear       = Nothing
    getColumnMaxWidth MODuration   = Nothing
    getColumnMaxWidth MOBitRate    = Nothing
    getColumnMaxWidth MOSampleRate = Nothing
    getColumnMaxWidth MOChannels   = Nothing
    getColumnMaxWidth MOComment    = Nothing

    getCellText MOArtist     info = miArtist info
    getCellText MOTitle      info = miTitle info
    getCellText MOAlbum      info = miAlbum info
    getCellText MOTrack      info = if miTrack info == 0 then "" else show $ miTrack info
    getCellText MOGenre      info = miGenre info
    getCellText MOYear       info = if miYear info == 0 then "" else show $ miYear info
    getCellText MODuration   info = showDuration (miDuration info)
    getCellText MOBitRate    info = show (miBitRate info) ++ " kb/s"
    getCellText MOSampleRate info = show (miSampleRate info) ++ " HZ"
    getCellText MOChannels   info = show $ miChannels info
    getCellText MOComment    info = miComment info

    getCellXAlign MOArtist     = 0.0
    getCellXAlign MOTitle      = 0.0
    getCellXAlign MOAlbum      = 0.0
    getCellXAlign MOTrack      = 1.0
    getCellXAlign MOGenre      = 0.0
    getCellXAlign MOYear       = 1.0
    getCellXAlign MODuration   = 0.0
    getCellXAlign MOBitRate    = 0.0
    getCellXAlign MOSampleRate = 0.0
    getCellXAlign MOChannels   = 0.0
    getCellXAlign MOComment    = 0.0

    compareRow MOArtist     row1 row2 = return $ comparing miArtist     row1 row2
    compareRow MOTitle      row1 row2 = return $ comparing miTitle      row1 row2
    compareRow MOAlbum      row1 row2 = return $ playlistAlbumCompare   row1 row2
    compareRow MOTrack      row1 row2 = return $ comparing miTrack      row1 row2
    compareRow MOGenre      row1 row2 = return $ comparing miGenre      row1 row2
    compareRow MOYear       row1 row2 = return $ comparing miYear       row1 row2
    compareRow MODuration   row1 row2 = return $ comparing miDuration   row1 row2
    compareRow MOBitRate    row1 row2 = return $ comparing miBitRate    row1 row2
    compareRow MOSampleRate row1 row2 = return $ comparing miSampleRate row1 row2
    compareRow MOChannels   row1 row2 = return $ comparing miChannels   row1 row2
    compareRow MOComment    row1 row2 = return $ comparing miComment    row1 row2


data MultimediaOption = MOArtist 
                      | MOTitle
                      | MOAlbum
                      | MOTrack
                      | MOGenre
                      | MOYear
                      | MODuration
                      | MOBitRate
                      | MOSampleRate
                      | MOChannels
                      | MOComment
                        deriving (Eq, Show, Read)

-- | Init state.
playlistInitState :: PlaylistState      
playlistInitState = 
  PlaylistState Nothing (0, 0)

-- | New playlist buffer.
-- If path is directory, will search multimedia files with given directory.
-- If path is multimedia file, then add to play list.
playlistBufferNew :: FilePath -> [String] -> Client -> PageId -> CustomizeWrap -> IO PlaylistBuffer
playlistBufferNew path _ client pageId _ = do
  -- Get multimedia info list.
  infos <- playlistBufferGenerateInfos path

  -- Play first music ... sweet ... :)
  let sendPlaySignal = unless (null infos) $ do
         let filepath = toString $ miFilePath $ head infos 
         processId <- fmap show getProcessID
         mkMplayerDaemonSignal client "Play" [filepath, processId]

  -- Build DBus match rule.
  mkMplayerClientMatchRule client
     (DaemonProcessStartup,
      \ DaemonProcessStartupArgs -> sendPlaySignal)

  -- Check whether daemon process has startup.
  ifM (isBusNameExist $ packGenericBusName "mplayer") 
      -- Send play signal if daemon process has startup.
      sendPlaySignal
      -- Otherwise startup daemon process, send join signal after daemon process startup complete.
      (do 
        processId <- getProcessID
        putStrLn "No mplayer daemon process, starting one."
        runProcess_ "manatee-mplayer-daemon" [show processId])

  PlaylistBuffer <$> newTVarIO path
                 <*> pure client
                 <*> pure pageId
                 <*> pure mplayerMode
                 <*> newTVarIO infos
                 <*> pure (pairPred [MOTitle, MOAlbum, MOArtist, MOYear, MOTrack, MOBitRate, MODuration])
                 <*> newTVarIO (MOAlbum, SortAscending)
                 <*> newTVarIO playlistInitState

-- |  Generate multimedia infos.
playlistBufferGenerateInfos :: FilePath -> IO [MultimediaInfo]
playlistBufferGenerateInfos path
    -- Search multimedia file info is path is directory.
    | directoryDoesExist (fromString path) 
        = directoryGetFilesRecursive (fromString path)
          >>= concatMapM filterMultimediaFile 
    | fileDoesExist (fromString path)
        = filterMultimediaFile (fromString path)
          where filterMultimediaFile file = do
                    isMFile <- isMultimediaFile file
                    if isMFile
                       then liftM maybeToList $ getMultimediaInfo file
                       else return []                       
  
-- | Whether path match multimedia regular expression.
isMultimediaFile :: ByteString -> IO Bool
isMultimediaFile path = do
  let file = fileFromPath path
  fileInfo <- fileQueryInfo file "*" [] Nothing
  let contentType = fileInfoGetContentType fileInfo
  return $ 
    case contentType of
      Just ct -> ct =~ ("^(audio|video)/.+" :: String)
      Nothing -> False

-- | Get multimedia information.
getMultimediaInfo :: ByteString -> IO (Maybe MultimediaInfo)
getMultimediaInfo path = 
  tagFileOpen path 
    >?>=> \tf ->
  tagFileGetTag tf 
    >?>=> \t -> 
  tagFileGetAudioProperties tf 
    >?>=> \p -> liftM Just $ 
        MultimediaInfo <$> pure path
                       <*> tagGetArtist t
                       <*> getMultimediaTitle (toString path) t
                       <*> tagGetAlbum t
                       <*> tagGetTrack t
                       <*> tagGetGenre t
                       <*> tagGetYear t                                                                      
                       <*> audioPropertiesGetDuration p
                       <*> audioPropertiesGetBitRate p
                       <*> audioPropertiesGetSampleRate p
                       <*> audioPropertiesGetChannels p
                       <*> tagGetComment t
-- | Show duration.
showDuration :: Int -> String
showDuration = 
    show . timeToTimeOfDay . secondsToDiffTime . fromIntegral

-- | Get multimedia title.
getMultimediaTitle :: FilePath -> Tag -> IO String
getMultimediaTitle path tag = do
  title <- tagGetTitle tag
  return $ if null title
              -- Use file name if title is empty.
              then takeBaseName path
              else title

-- | Compare album.
playlistAlbumCompare :: MultimediaInfo -> MultimediaInfo -> Ordering
playlistAlbumCompare row1 row2 
    | mAlbum1 /= mAlbum2
        = compare mAlbum1 mAlbum2
    | mArtist1 /= mArtist2
        = compare mArtist1 mArtist2
    | mTrack1 /= mTrack2
        = compare mTrack1 mTrack2
    | otherwise
        = compare mTitle1 mTitle2
    where 
      mAlbum1  = miAlbum row1
      mAlbum2  = miAlbum row2
      mArtist1 = miArtist row1
      mArtist2 = miArtist row2
      mTrack1  = miTrack row1
      mTrack2  = miTrack row2
      mTitle1  = miTitle row1
      mTitle2  = miTitle row2

-- | Write state.
playlistBufferWriteState :: PlaylistBuffer -> FilePath -> IO ()
playlistBufferWriteState buffer path = do
  state <- readTVarIO $ playlistBufferState buffer
  writeConfigPath path state

-- | Read state.
playlistBufferReadState :: PlaylistBuffer -> FilePath -> IO ()  
playlistBufferReadState buffer path = do
  state <- readConfigPath path playlistInitState
  writeTVarIO (playlistBufferState buffer) state
  
$(derive makeBinary ''PlaylistState)

