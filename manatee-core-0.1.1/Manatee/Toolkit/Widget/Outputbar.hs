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

module Manatee.Toolkit.Widget.Outputbar where

import Control.Concurrent.STM 
import Graphics.UI.Gtk
import Manatee.Toolkit.General.Functor
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Box

data Outputbar =
    Outputbar {outputbarFrame           :: Frame -- frame for decorative
              ,outputbarLabel           :: Label -- label for output
              ,outputbarHandlerId       :: TVar (Maybe HandlerId) -- handlerId for track timeout
              }

-- | Background color.
outputbarBackgroundColor :: Color
outputbarBackgroundColor = Color 62720 62720 46336

-- | Create outputbar.
outputbarNew :: IO Outputbar
outputbarNew = do
  -- Create frame.
  frame <- frameNew
  set frame [frameShadowType := ShadowNone] -- don't display frame border

  -- Create box.
  box <- hBoxNew False 0

  -- Create label.
  label <- labelNew $ Just ""

  -- Create id.
  id <- newTVarIO Nothing

  -- Connect module.
  frame `containerAdd` box
  boxPackStart box label PackNatural 0

  return $ Outputbar frame label id
  
-- | Show outputbar.  
outputbarShow :: BoxClass box => box -> Outputbar -> String -> IO ()
outputbarShow box bar str = do
  -- Init.
  let frame = outputbarFrame bar
      label = outputbarLabel bar
      handlerId = outputbarHandlerId bar
  id <- readTVarIO handlerId

  -- Remove previous timeout handler first.
  id ?>= timeoutRemove
  
  -- Set output.
  labelSetText label str
  width <- labelGetMaxWidthChars label
  set label [labelAttributes := [AttrBackground 0 width outputbarBackgroundColor]]

  -- Keep outputbar display before statusbar of page.
  childNumber <- (<<<=) length containerGetChildren box 
  boxTryPack box frame PackNatural (Just $ pred childNumber) Nothing
  
  -- Show
  widgetShowAll frame
  
  -- Hide outputbar 10s later.
  newId <- timeoutAdd (box `containerRemove` frame >> return False) 10000
  modifyTVarIO handlerId (const $ Just newId)
