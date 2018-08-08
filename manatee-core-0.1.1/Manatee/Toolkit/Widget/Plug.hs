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

module Manatee.Toolkit.Widget.Plug where

import Graphics.UI.Gtk hiding (Plug, plugNew, plugGetId)
import Manatee.Toolkit.Gtk.Struct

import qualified Graphics.UI.Gtk.Embedding.Plug as P

data Plug =
    Plug {plugBody      :: P.Plug           -- GtkPlug
         ,plugId        :: GWindowId        -- GtkPlug id
         ,plugHandler   :: ConnectId P.Plug -- `deleteEvent` signal handler 
         }

-- | Create new plug.
plugNew :: Maybe NativeWindowId -> IO Plug
plugNew socketId = do
  -- Create body.
  plug <- P.plugNew socketId

  -- Get plug id.
  plugId <- P.plugGetId plug

  -- This is hacking trick for hide GtkPlug when re-parent GtkSocket.
  -- Because current Gtk+ haven't explicit mechanism to re-parent GtkSocket.
  -- GtkPlug will destroy when GtkSocket remove from it's container, 
  -- And `deleteEvent` signal can hide GtkPlug instead destroy it,
  -- so we can re-use GtkPlug with `socketAddId` in new GtkSocket container.
  -- When you want do "real destroy" action GtkPlug, disable this signal first.
  -- Otherwise GtkPlug always live.
  handler <- plug `on` deleteEvent $ tryEvent $ return ()

  return (Plug plug (GWindowId plugId) handler)

-- | Destroy plug.
-- Disconnect `deleteEvent` signal before destroy GtkPlug.
-- Make sure GtkPlug destroy completely.
plugDestroy :: Plug -> IO ()
plugDestroy plug = do
  -- Disconnect `deleteEvent` signal handler before destroy,
  -- otherwise, can't destroy GtkPlug.
  signalDisconnect $ plugHandler plug

  -- Destroy GtkPlug.
  widgetDestroy $ plugBody plug
