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

module Manatee.Toolkit.Widget.Statusbar where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad
import Data.Sequence (Seq)
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew)
import Manatee.Toolkit.General.Functor
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.Seq
import Manatee.Toolkit.Gtk.Box
import Manatee.Toolkit.Gtk.Container

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Graphics.UI.Gtk.Display.Statusbar as S

type StatusbarSubitem = S.Statusbar
type StatusbarInfoTable = Seq (String, String)

data Statusbar =
    Statusbar {statusbarBox             :: HBox
              ,statusbarInfoSubitem     :: StatusbarSubitem
              ,statusbarInfoTable       :: TVar StatusbarInfoTable
              ,statusbarProgressAlign   :: Alignment
              ,statusbarProgressBar     :: ProgressBar}

-- | Progress bar padding.
progressPadding :: Int
progressPadding = 3

-- | Progress height.
progressHeight :: Int
progressHeight = 3

-- | Create new statusbar.
statusbarNew :: BoxClass b => b -> IO Statusbar
statusbarNew b = do
  -- Status box for contain status boxes.
  statusBox <- hBoxNew False 0
  boxPackStart (toBox b) statusBox PackNatural 0

  -- Progress box.
  progressAlign <- alignmentNew 0.0 0.5 1.0 1.0
  alignmentSetPadding progressAlign progressPadding progressPadding progressPadding progressPadding

  -- Progress bar.
  progressBar <- progressBarNew
  widgetSetSizeRequest progressBar (-1) progressHeight

  -- Connect.
  progressAlign `containerAdd` progressBar

  Statusbar <$> pure statusBox
            <*> statusbarSubitemNew statusBox
            <*> newTVarIO Seq.empty 
            <*> pure progressAlign
            <*> pure progressBar

-- | Clone statusbar.
statusbarClone :: BoxClass b => b -> Statusbar -> IO Statusbar  
statusbarClone box oldStatusbar = do
  -- Clone status from old statusbar. 
  newStatusbar <- statusbarNew box
  replaceTVarField newStatusbar oldStatusbar statusbarInfoTable

  -- Update status.
  statusbarInfoSubitemUpdate newStatusbar

  return newStatusbar

-- | Get info table.
statusbarGetInfoTable :: Statusbar -> IO StatusbarInfoTable
statusbarGetInfoTable = readTVarIO . statusbarInfoTable

-- | Create new sub-statusbar.
statusbarSubitemNew :: BoxClass b => b -> IO StatusbarSubitem
statusbarSubitemNew b = do
  bar <- S.statusbarNew
  statusbarSetHasResizeGrip bar False   -- don't show resize grip
  boxPackStart (toBox b) bar PackGrow 0 -- use PackGrow for grow size

  return bar

-- | Update info status.
statusbarInfoSubitemSetText :: Statusbar -> String -> IO ()
statusbarInfoSubitemSetText = 
    statusbarSubitemSetText . statusbarInfoSubitem

-- | Set text internal.
statusbarSubitemSetText :: StatusbarSubitem -> String -> IO ()
statusbarSubitemSetText ss str = do
  id <- statusbarGetContextId ss str
  statusbarPush ss id str
  return ()

-- | Add new info item to info subitem.
statusbarInfoItemAdd :: Statusbar -> String -> String -> IO ()
statusbarInfoItemAdd = statusbarInfoItemUpdate

-- | Update info item.
statusbarInfoItemUpdate :: Statusbar -> String -> String -> IO ()
statusbarInfoItemUpdate statusbar item info = 
    statusbarApplyInfoTable statusbar (replaceOrAdd (\x -> fst x == item) (item, info))

-- | Remove info item from info subitem.
statusbarInfoItemRemove :: Statusbar -> String -> IO ()
statusbarInfoItemRemove statusbar item = 
    statusbarApplyInfoTable statusbar (deleteMatch (\y -> fst y == item))

-- | Apply info table.
statusbarApplyInfoTable :: Statusbar -> (StatusbarInfoTable -> StatusbarInfoTable) -> IO ()
statusbarApplyInfoTable statusbar f = do
  -- Apply function.
  modifyTVarIO (statusbarInfoTable statusbar) f

  -- Update info subitem.
  statusbarInfoSubitemUpdate statusbar

-- | Update info item subitem information.
statusbarInfoSubitemUpdate :: Statusbar -> IO ()
statusbarInfoSubitemUpdate statusbar = do
  infoList <- (<<<=) F.toList (readTVarIO . statusbarInfoTable) statusbar 
  unlessNull infoList (statusbarInfoSubitemSetText statusbar (concatMap (\x -> snd x ++ " ") infoList))

-- | Update progress status.
statusbarProgressUpdate :: Statusbar -> Double -> IO ()  
statusbarProgressUpdate (Statusbar {statusbarBox                = box
                                   ,statusbarProgressAlign      = progressAlign
                                   ,statusbarProgressBar        = progressBar}) 
                        progress = do
  -- Show progress bar first.
  boxTryPack box progressAlign PackNatural Nothing Nothing
  widgetShowAll progressAlign

  -- Set progress.
  set progressBar  [progressBarFraction := progress / 100]
  progressBarSetText progressBar  (show progress ++ "%")

  -- Hide progress bar after reach 100% (default is 100 ms).
  when (progress == 100.0) $
     timeoutAdd (do
                  set progressBar [progressBarFraction := 0]
                  containerTryRemove box progressAlign
                  return False) 100
     >> return ()
