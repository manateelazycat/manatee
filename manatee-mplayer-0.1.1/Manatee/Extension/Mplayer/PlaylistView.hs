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

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Manatee.Extension.Mplayer.PlaylistView where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import Data.ByteString.UTF8
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get)
import Manatee.Core.Config
import Manatee.Core.PageFrame
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Extension.Mplayer.DBus
import Manatee.Extension.Mplayer.PlaylistBuffer
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Functor
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gio.Gio
import Manatee.Toolkit.Gtk.ModelView
import Manatee.Toolkit.Gtk.ScrolledWindow
import Paths_manatee_mplayer
import System.FilePath
import System.GIO.File.ContentType 
import System.Posix.Process
import System.Random

import qualified Data.Map as M

data PlaylistView = 
    PlaylistView {playlistViewPlugId              :: TVar PagePlugId
                 ,playlistViewFrame               :: PageFrame
                 ,playlistViewBuffer              :: PlaylistBuffer
                 ,playlistViewTreeView            :: TreeView
                 ,playlistViewListStore           :: ListStore MultimediaInfo
                 ,playlistViewSortModel           :: TypedTreeModelSort MultimediaInfo
                 ,playlistViewCurrentPlayPath     :: TVar TreePath
                 ,playlistViewPlayMode            :: TVar PlayMode}
    deriving Typeable

instance PageBuffer PlaylistBuffer where
    pageBufferGetName           = readTVarIO . playlistBufferName
    pageBufferSetName a         = writeTVarIO (playlistBufferName a)
    pageBufferClient            = playlistBufferClient
    pageBufferCreateView a pId  = PageViewWrap <$> playlistViewNew a pId
    pageBufferMode              = playlistBufferMode
    pageBufferPackageName _     = fmap takeFileName getDataDir

instance PageView PlaylistView where
    pageViewBuffer               = PageBufferWrap . playlistViewBuffer
    pageViewPlugId               = playlistViewPlugId
    pageViewFrame                = playlistViewFrame
    pageViewLocalKeymap _        = playlistViewLocalKeymap
    pageViewLocalCommandMap _    = playlistViewLocalCommandMap
    pageViewFocus                = treeViewFocus . playlistViewTreeView
    pageViewPropagateWidget      = castToWidget . playlistViewTreeView
    pageViewSaveState view       = playlistViewSaveState view Nothing
    pageViewRestoreState view    = playlistViewRestoreState view Nothing
    pageViewWriteState view path = playlistViewSaveState view (Just path)
    pageViewReadState view path  = playlistViewRestoreState view (Just path)
    pageViewScrollToTop          = playlistViewScrollToTop
    pageViewScrollToBottom       = playlistViewScrollToBottom
    pageViewScrollVerticalPage   = playlistViewScrollVerticalPage
    pageViewScrollVerticalStep   = playlistViewScrollVerticalStep

data PlayMode = SingleMode
              | ListMode
              | RandomMode
                deriving (Show, Eq, Ord)

-- | Internal new function.
playlistViewNew :: PlaylistBuffer -> PagePlugId -> IO PlaylistView
playlistViewNew buffer plugId = do 
  -- Create plug id.
  pId <- newTVarIO plugId

  -- Create UI frame.
  pFrame <- pageFrameNewWithModeName (pageModeName $ playlistBufferMode buffer)

  -- Tree view.
  treeView <- treeViewNew
  treeViewSetEnableTreeLines treeView True
  pageFrameAddChild pFrame treeView

  -- List store.
  listStore <- listStoreNew []

  -- Sort model.
  sortModel <- treeModelSortNewWithModel listStore

  -- Current play path.
  path <- newTVarIO [0]

  -- Play mode.
  playMode <- newTVarIO ListMode

  -- Playlist view.
  let playlistView = PlaylistView pId pFrame buffer treeView listStore sortModel path playMode 

  -- Build match rule listen DBus message.
  playlistViewBuildMatchRule playlistView

  -- Draw view.
  playlistViewDraw playlistView

  return playlistView

-- | Build match rule.
playlistViewBuildMatchRule :: PlaylistView -> IO ()  
playlistViewBuildMatchRule view = 
  mkMplayerClientMatchRule 
    (pageViewClient view) 
    (PlayFinished, 
     \ _ -> do
       mode <- readTVarIO (playlistViewPlayMode view)
       case mode of
         SingleMode -> playlistViewPlayCurrent view
         ListMode   -> playlistViewPlayNext view
         RandomMode -> playlistViewPlayRandom view)

-- | Switch play mode.
playlistViewSwitchPlayMode :: PlaylistView -> IO ()
playlistViewSwitchPlayMode view = do
  mode <- readTVarIO (playlistViewPlayMode view)
  let newMode = case mode of
                  SingleMode -> ListMode
                  ListMode   -> RandomMode
                  RandomMode -> SingleMode
  writeTVarIO (playlistViewPlayMode view) newMode
  pageViewUpdateStatusbar view "PlayMode" (" PlayMode (" ++ show newMode ++ ")")

-- | Play current track.
playlistViewPlayCurrent :: PlaylistView -> IO ()
playlistViewPlayCurrent view = do
  currentPath <- readTVarIO $ playlistViewCurrentPlayPath view
  playlistViewPlayInternal view currentPath

-- | Draw playlist view.
playlistViewDraw :: PlaylistView -> IO ()
playlistViewDraw view = do
  -- Get value.
  let buffer    = playlistViewBuffer view
      treeView  = playlistViewTreeView view
      store     = playlistViewListStore view
      model     = playlistViewSortModel view
  infos <- readTVarIO $ playlistBufferInfos buffer 

  -- Append to list store.
  listStoreClear store
  forM_ infos (listStoreAppend store) 

  -- Set tree view model.
  treeViewSetModel treeView model

  -- Clean tree view.
  treeViewRemoveColumns treeView

  -- Add icon column.
  playlistViewAddIconColumn treeView store

  -- Add column file info to tree view.
  forM_ (playlistBufferOptions buffer) (playlistViewAddColumn treeView store model)

  -- Set default sort rule.
  sortStatus <- readTVarIO $ playlistBufferSortStatus buffer
  playlistViewSortInternal view sortStatus

  return ()

-- | Internal sort function.
playlistViewSortInternal :: PlaylistView -> (MultimediaOption, SortType) -> IO ()
playlistViewSortInternal view (option, sortType) = do
  let options = playlistBufferOptions $ playlistViewBuffer view 
  lookup option options ?>= \x -> 
      treeSortableSetSortColumnId (playlistViewSortModel view) x sortType

-- | Add icon column.
playlistViewAddIconColumn :: (TreeViewClass self
                            ,TreeModelClass (model row)
                            ,TypedTreeModelClass model) 
                            => self 
                            -> model row 
                            -> IO ()
playlistViewAddIconColumn treeView store = do
  -- Any music format can work, we just need music icon pixbuf.
  pixbuf <- getIconPixbuf $ contentTypeGetIcon "audio/mp3"
    
  tvc <- treeViewColumnNew
  set tvc [treeViewColumnTitle := ""]
  treeViewAppendColumn treeView tvc
  
  icon <- cellRendererPixbufNew
  treeViewColumnPackStart tvc icon True
  cellLayoutSetAttributes tvc icon store $ \_ -> [cellPixbuf := pixbuf]

-- | Add column.
playlistViewAddColumn :: (MultimediaInfoClass t
                        ,TreeViewClass self1
                        ,TreeModelClass self
                        ,TreeModelSortClass self
                        ,TypedTreeModelClass model
                        ,TreeSortableClass self) 
                        => self1
                        -> model MultimediaInfo
                        -> self
                        -> (t, SortColumnId)
                        -> IO ()
playlistViewAddColumn treeView model sortModel option@(info,sortId) = do
  playlistViewSetSortFunc model sortModel option

  let name = getColumnTitle info
      maxWidth = getColumnMaxWidth info
  tvc <- treeViewAddColumnWithTitle treeView name sortId
  maxWidth ?>= \width -> treeViewColumnSetMaxWidth tvc width

  cell <- cellRendererTextNew
  treeViewColumnPackStart tvc cell True

  playlistViewSetCellText tvc cell model sortModel info

-- | Set sort function.
playlistViewSetSortFunc :: (TreeSortableClass self,
                           TypedTreeModelClass model,
                           MultimediaInfoClass a) =>
                          model MultimediaInfo
                        -> self
                        -> (a, SortColumnId)
                        -> IO ()
playlistViewSetSortFunc model sortModel (info, sortId) = 
  treeSortableSetSortFunc sortModel sortId $ \iter1 iter2 -> do
    row1 <- treeModelGetRow model iter1
    row2 <- treeModelGetRow model iter2
    compareRow info row1 row2

-- | Set cell text.
playlistViewSetCellText :: (CellLayoutClass self,
                        CellRendererTextClass cell,
                        TreeModelClass model,
                        TreeModelSortClass model,
                        TypedTreeModelClass model1,
                        MultimediaInfoClass a) 
                     => self
                     -> cell
                     -> model1 MultimediaInfo
                     -> model
                     -> a
                     -> IO ()
playlistViewSetCellText tvc cell model sortModel info = 
    cellLayoutSetAttributeFunc tvc cell sortModel $ \iter -> do
      row <- treeModelSortGetRow model sortModel iter
      set cell [cellText   := getCellText info row
               ,cellXAlign := getCellXAlign info]

-- | Next node.
playlistViewNextNode :: PlaylistView -> IO ()
playlistViewNextNode = treeViewFocusNextToplevelNode . playlistViewTreeView
    
-- | Previous node.
playlistViewPrevNode :: PlaylistView -> IO ()    
playlistViewPrevNode = treeViewFocusPrevToplevelNode . playlistViewTreeView

-- | Keymap.
playlistViewLocalKeymap :: Map Text Text
playlistViewLocalKeymap = 
    M.fromList
         [("j",         "Next")
         ,("k",         "Previous")
         ,("Down",      "Next")
         ,("Up",        "Previous")          
         ,("J",         "Scroll to bottom")
         ,("K",         "Scroll to top")
         ,(" ",         "Scroll page up")
         ,("b",         "Scroll page down")
         ,("PageDown",  "Scroll page up")
         ,("PageUp",    "Scroll page down")
         ,("m",         "Play")
         ,("Return",    "Play")
         ,("M",         "Pause")
         ,("z",         "Stop")
         ,("h",         "Backward")
         ,("l",         "Forward")
         ,("Right",     "Backward")
         ,("Left",      "Forward")
         ,(",",         "Decrease volume")
         ,(".",         "Increase volume")
         ,("-",         "Decrease volume")
         ,("=",         "Increase volume")
         ,("n",         "Play next")
         ,("p",         "Play previous")
         ,("N",         "Play random")
         ,("P",         "Switch play mode")
         ,("1",         "Sort by title")
         ,("2",         "Sort by album")
         ,("3",         "Sort by artist")
         ,("4",         "Sort by year")
         ,("5",         "Sort by track")
         ,("6",         "Sort by bit rate")
         ,("7",         "Sort by duration")
         ]

-- | Keymap.
playlistViewLocalCommandMap :: Map Text (PlaylistView -> IO ())
playlistViewLocalCommandMap = 
    M.fromList
         [("Next",              playlistViewNextNode)
         ,("Previous",          playlistViewPrevNode)
         ,("Scroll to bottom",  playlistViewScrollToBottom)
         ,("Scroll to top",     playlistViewScrollToTop)
         ,("Scroll page up",    playlistViewScrollVerticalPage True)
         ,("Scroll page down",  playlistViewScrollVerticalPage False)
         ,("Play",              playlistViewPlay)
         ,("Pause",             playlistViewPause)
         ,("Stop",              playlistViewStop)
         ,("Backward",          playlistViewBackward)
         ,("Forward",           playlistViewForward)
         ,("Decrease volume",   playlistViewVolumeDec)
         ,("Increase volume",   playlistViewVolumeInc)
         ,("Play next",         playlistViewPlayNext)
         ,("Play previous",     playlistViewPlayPrev)
         ,("Play random",       playlistViewPlayRandom)
         ,("Switch play mode",  playlistViewSwitchPlayMode)
         ,("Sort by title",     playlistViewSortByTitle)
         ,("Sort by album",     playlistViewSortByAlbum)
         ,("Sort by artist",    playlistViewSortByArtist)
         ,("Sort by year",      playlistViewSortByYear)
         ,("Sort by track",     playlistViewSortByTrack)
         ,("Sort by bit rate",  playlistViewSortByBitRate)
         ,("Sort by duration",  playlistViewSortByDuration)
         ]

-- | Sort by title.
playlistViewSortByTitle :: PlaylistView -> IO ()
playlistViewSortByTitle view = playlistViewSort view MOTitle

-- | Sort by album.
playlistViewSortByAlbum :: PlaylistView -> IO ()
playlistViewSortByAlbum view = playlistViewSort view MOAlbum

-- | Sort by artist.
playlistViewSortByArtist :: PlaylistView -> IO ()
playlistViewSortByArtist view = playlistViewSort view MOArtist

-- | Sort by year.
playlistViewSortByYear :: PlaylistView -> IO ()
playlistViewSortByYear view = playlistViewSort view MOYear

-- | Sort by track.
playlistViewSortByTrack :: PlaylistView -> IO ()
playlistViewSortByTrack view = playlistViewSort view MOTrack

-- | Sort by BitRate.
playlistViewSortByBitRate :: PlaylistView -> IO ()
playlistViewSortByBitRate view = playlistViewSort view MOBitRate

-- | Sort by duration.
playlistViewSortByDuration :: PlaylistView -> IO ()
playlistViewSortByDuration view = playlistViewSort view MODuration

-- | Sort column.
playlistViewSort :: PlaylistView -> MultimediaOption -> IO ()
playlistViewSort view option = do
  -- Get model and options.
  let model   = playlistViewSortModel view 
      buffer  = playlistViewBuffer view
      options = playlistBufferOptions buffer
  -- Get current sortType and columnId.
  (curSortType, _, curSortColumnId) <- treeSortableGetSortColumnId model
  lookup option options ?>= \id -> do
    treeSortableSetSortColumnId model id $
      if id == curSortColumnId
         -- Just change sort order when sort column id is same.
         then 
             -- Just change sort order.
             case curSortType of
               SortAscending  -> SortDescending
               SortDescending -> SortAscending
         -- Otherwise sort ascending.
         else SortAscending

    -- Get new sort type.
    (newSortType, _, _) <- treeSortableGetSortColumnId model

    -- Update sort status of buffer.
    writeTVarIO (playlistBufferSortStatus $ playlistViewBuffer view) (option, newSortType)

    -- Focus to cell.
    treeViewFocus (playlistViewTreeView view)

-- | Play current selected track.
playlistViewPlay :: PlaylistView -> IO ()
playlistViewPlay view = do
  let treeView = playlistViewTreeView view
  treeViewGetSelectedPath treeView
    >?>= \ path -> do
      -- Convert sorted path to unsorted model path.
      currentPath <- treeModelSortConvertPathToChildPath (playlistViewSortModel view) path
      playlistViewPlayInternal view currentPath

-- | Play next track randomly.
playlistViewPlayRandom :: PlaylistView -> IO ()
playlistViewPlayRandom view = do
  size <- treeViewGetToplevelNodeCount (playlistViewTreeView view)
  randomIndex <- randomRIO (0, size - 1)
  playlistViewPlayInternal view [randomIndex]

-- | Play next track.
playlistViewPlayNext :: PlaylistView -> IO ()
playlistViewPlayNext view = do
  currentPath <- readTVarIO $ playlistViewCurrentPlayPath view
  nextSortPath <- treeViewNextSortPath (playlistViewTreeView view) (playlistViewSortModel view) currentPath
  playlistViewPlayInternal view nextSortPath

-- | Play previous track.
playlistViewPlayPrev :: PlaylistView -> IO ()
playlistViewPlayPrev view = do
  currentPath <- readTVarIO $ playlistViewCurrentPlayPath view
  prevSortPath <- treeViewPrevSortPath (playlistViewTreeView view) (playlistViewSortModel view) currentPath
  playlistViewPlayInternal view prevSortPath

-- | The internal function of `playlistViewPlay'.
playlistViewPlayInternal :: PlaylistView -> TreePath -> IO ()
playlistViewPlayInternal view path = do
  -- Get file path and update play path status.
  filepath <- liftM miFilePath $ listStoreGetValue (playlistViewListStore view) (head path)
  let displayPath = filepathGetDisplayName filepath
  writeTVarIO (playlistViewCurrentPlayPath view) path

  -- Update play mode status.
  mode <- readTVarIO (playlistViewPlayMode view)
  pageViewUpdateStatusbar view "PlayMode" (" PlayMode (" ++ show mode ++ ")")

  -- Update playing status.
  pageViewUpdateStatusbar view "Playing" (" Playing : " ++ displayPath)

  -- Send daemon signal to play.
  processId <- fmap show getProcessID
  mkMplayerDaemonSignal (pageViewClient view) "Play" [toString filepath, processId]

-- | Stop play.
playlistViewStop :: PlaylistView -> IO ()
playlistViewStop view = 
    mkMplayerDaemonSignal (pageViewClient view) "Stop" []

-- | Pause or continue play.
playlistViewPause :: PlaylistView -> IO ()
playlistViewPause view = 
    mkMplayerDaemonSignal (pageViewClient view) "Pause" []

-- | Forward current track.
playlistViewForward :: PlaylistView -> IO ()
playlistViewForward view =
    mkMplayerDaemonSignal (pageViewClient view) "Forward" ["10"]

-- | Backward current track.
playlistViewBackward :: PlaylistView -> IO ()
playlistViewBackward view =
    mkMplayerDaemonSignal (pageViewClient view) "Backward" ["10"]

-- | Increase volume.
playlistViewVolumeInc :: PlaylistView -> IO ()
playlistViewVolumeInc view =
    mkMplayerDaemonSignal (pageViewClient view) "VolumeInc" ["10"]

-- | Decrease volume.
playlistViewVolumeDec :: PlaylistView -> IO ()
playlistViewVolumeDec view =
    mkMplayerDaemonSignal (pageViewClient view) "VolumeDec" ["10"]

-- | Begin.
playlistViewScrollToTop :: PlaylistView -> IO ()
playlistViewScrollToTop = 
    treeViewFocusFirstToplevelNode . playlistViewTreeView

-- | End.
playlistViewScrollToBottom :: PlaylistView -> IO ()
playlistViewScrollToBottom = 
    treeViewFocusLastToplevelNode . playlistViewTreeView 

-- | Scroll page vertically.
playlistViewScrollVerticalPage :: Bool -> PlaylistView -> IO ()
playlistViewScrollVerticalPage isDown a = do
  let sw = playlistViewScrolledWindow a
      tv = playlistViewTreeView a
  pageInc <- (<=<) adjustmentGetPageIncrement scrolledWindowGetVAdjustment sw
  treeViewScrollVertical tv sw (if isDown then pageInc else (- pageInc))

-- | Scroll step vertically.
playlistViewScrollVerticalStep :: Bool -> PlaylistView -> IO ()
playlistViewScrollVerticalStep isDown a = do
  let sw = playlistViewScrolledWindow a
      tv = playlistViewTreeView a
  stepInc <- (<<<=) i2d treeViewGetSelectedCellHeight tv
  treeViewScrollVertical tv sw (if isDown then stepInc else (- stepInc))

-- | Scrolled window.
playlistViewScrolledWindow :: PlaylistView -> ScrolledWindow    
playlistViewScrolledWindow =
    pageFrameScrolledWindow . playlistViewFrame

-- | Save state.
playlistViewSaveState :: PlaylistView -> Maybe FilePath -> IO ()
playlistViewSaveState view@(PlaylistView {playlistViewBuffer     = buffer
                                         ,playlistViewTreeView   = treeView}) 
                      statePath = do
  -- Get selected path.
  selectedPath <- treeViewGetSelectedPath treeView

  -- Get scroll position.
  scrolledWindowPosition <- scrolledWindowGetValue (playlistViewScrolledWindow view)

  -- Save state.
  let state = PlaylistState selectedPath scrolledWindowPosition
  case statePath of
    Nothing   -> writeTVarIO (playlistBufferState buffer) state
    Just path -> writeConfigPath path state

-- | Restore state.
playlistViewRestoreState :: PlaylistView -> Maybe FilePath -> IO ()
playlistViewRestoreState view@(PlaylistView {playlistViewBuffer  = buffer
                                            ,playlistViewTreeView= treeView})
                         statePath = do
  bufferState <- readTVarIO (playlistBufferState buffer)
  (PlaylistState selectedPath scrolledWindowPosition) <- 
      case statePath of
        Just path -> readConfigPath path bufferState
        Nothing   -> return bufferState

  -- Restore selected path.
  selectedPath ?>= \path -> treeViewSetCursor treeView path Nothing

  -- Restore scroll position.
  scrolledWindowSetValue (playlistViewScrolledWindow view) scrolledWindowPosition
