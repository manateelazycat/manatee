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

module Manatee.Plugin.Anything.TitleWidget where

import Graphics.UI.Gtk
import Manatee.Toolkit.Gtk.Box
import Manatee.Toolkit.Gtk.Container

data TitleWidget =
    TitleWidget {titleWidgetBox      :: HBox
                ,titleWidgetLabel    :: Label
                ,titleWidgetSpinner  :: Spinner}

-- | Title color.
titleColor :: Color
titleColor = Color 61440 30464 17920

-- | Number color.
numberColor :: Color
numberColor = Color 25000 25000 25000

-- | Spinner space.
spinnerSpace :: Int
spinnerSpace = 6

-- | Create candidate title.
titleWidgetNew :: Maybe String -> IO TitleWidget
titleWidgetNew title = do
  -- Init.
  box     <- hBoxNew False 0
  label   <- labelNew title
  spinner <- spinnerNew
  
  -- Show.
  boxPackStart box label PackRepel 0
  widgetShowAll box
  
  return $ TitleWidget box label spinner

-- | Set title.
titleWidgetSetTitle :: TitleWidget -> String -> Int -> IO ()
titleWidgetSetTitle widget title number 
    | number == 0 = do
      labelSetText label title
      labelSetAttributes label [AttrForeground 0 (length title) titleColor]
    | otherwise = do
      labelSetText label (title ++ numberStr)
      labelSetAttributes label [AttrForeground 0 (length title) titleColor
                               ,AttrWeight 0 (length title) WeightBold
                               ,AttrForeground (length title) (length (title ++ numberStr)) numberColor]
    where 
      label = titleWidgetLabel widget
      numberStr = " (" ++ show number ++ ")"

-- | Start spinner animation.
titleWidgetStart :: TitleWidget -> IO ()
titleWidgetStart TitleWidget {titleWidgetBox     = box
                             ,titleWidgetSpinner = spinner} = do
  boxTryPack box spinner PackNatural (Just 1) (Just spinnerSpace)
  spinnerStart spinner
  widgetShow spinner

-- | Stop spinner animation.
titleWidgetStop :: TitleWidget -> IO ()
titleWidgetStop TitleWidget {titleWidgetBox     = box
                            ,titleWidgetSpinner = spinner} = do
  containerTryRemove box spinner
  spinnerStop spinner


