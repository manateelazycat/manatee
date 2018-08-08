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

{-# LANGUAGE RankNTypes #-}
module Manatee.Core.Page where

import Graphics.UI.Gtk hiding (Frame, frameNew, Signal, Plug)
import Manatee.Core.PageView
import Manatee.Core.Types
import Manatee.Toolkit.Gtk.Struct
import Manatee.Toolkit.Widget.Plug

-- | New page.
pageNew :: PageId -> SignalBoxId -> PageType -> PagePlug -> PageViewWrap -> IO Page
pageNew pId sId pType plug pvw = 
  return $ Page pId sId pType plug pvw

-- | Page box.
pageBox :: Page -> VBox
pageBox page =
  pageApplyViewWrap page pageViewBox

-- | Applying function with page buffer.
pageApplyViewWrap :: Page -> (forall a . PageView a => a -> b) -> b
pageApplyViewWrap (Page {pageView = (PageViewWrap x)}) f = f x

-- | Page plug id.
pagePlugId :: Page -> GWindowId
pagePlugId = plugId . pagePlug
