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

module Manatee.Toolkit.Widget.NotebookTab where

import Data.Maybe
import Graphics.UI.Gtk
import Manatee.Toolkit.Gtk.Box
import Manatee.Toolkit.Gtk.Container
import Manatee.Toolkit.Gtk.Gtk

data NotebookTab =
    NotebookTab {ntBox          :: HBox
                ,ntSpinner      :: Spinner
                ,ntLabel        :: Label
                ,ntCloseButton  :: ToolButton
                ,ntSize         :: Int
                }

-- | Default size.
notebookTabDefaultSize :: Int
notebookTabDefaultSize = 8

-- | Create notebook tab.
notebookTabNew :: Maybe String -> Maybe Int -> IO NotebookTab
notebookTabNew name size = do
  -- Init.
  let iconSize = fromMaybe notebookTabDefaultSize size
  box <- hBoxNew False 0
  spinner <- spinnerNew
  label <- labelNew name
  image <- imageNewFromIcon "gtk-close" iconSize
  closeButton <- toolButtonNew (Just image) Nothing

  -- Show.
  boxPackStart box label PackRepel 0
  boxPackStart box closeButton PackRepel 0
  widgetShowAll box

  return $ NotebookTab box spinner label closeButton iconSize

-- | Set tab name.
notebookTabSetName :: NotebookTab -> String -> IO ()
notebookTabSetName tab = 
  labelSetText (ntLabel tab)

-- | Start spinner animation.
notebookTabStart :: NotebookTab -> IO ()
notebookTabStart NotebookTab {ntBox     = box
                             ,ntSpinner = spinner
                             ,ntSize    = size} = do
  boxTryPack box spinner PackNatural (Just 0) (Just (size `div` 2))
  spinnerStart spinner
  widgetShow spinner

-- | Stop spinner animation.
notebookTabStop :: NotebookTab -> IO ()
notebookTabStop NotebookTab {ntBox     = box
                            ,ntSpinner = spinner} = do
  containerTryRemove box spinner
  spinnerStop spinner

