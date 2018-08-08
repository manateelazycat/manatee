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

module Manatee.Toolkit.Widget.Interactivebar where

import Graphics.UI.Gtk
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.Gtk.Box
import Manatee.Toolkit.Gtk.Container
import Manatee.Toolkit.Gtk.Editable
import Manatee.Toolkit.Gtk.Gtk

data Interactivebar =
    Interactivebar {interactivebarFrame         :: Frame           -- frame for decorative
                   ,interactivebarBox           :: HBox            -- box for container
                   ,interactivebarTitleLabel    :: Label           -- title label
                   ,interactivebarEntry         :: Entry           -- main entry
                   }

type InteractivebarTitle        = String
type InteractivebarEntryStatus  = EditableStatus
type InteractivebarStatus       = (InteractivebarTitle, InteractivebarEntryStatus)

-- | Create new interactivebar.
interactivebarNew :: IO Interactivebar
interactivebarNew = do
  -- Create interactivebar.
  frame  <- frameNew
  box    <- hBoxNew False 0
  title  <- labelNew $ Just ""
  entry  <- entryNew

  -- Connect module.
  frame `containerAdd` box
  boxTryPack box title  PackNatural (Just 0) Nothing
  boxTryPack box entry  PackGrow    (Just 1) Nothing

  return $ Interactivebar frame box title entry

-- | Clone interactivebar.
interactivebarClone :: BoxClass box => box -> Interactivebar -> IO Interactivebar  
interactivebarClone box oldBar = do
  newBar <- interactivebarNew

  -- Clone status.
  interactivebarGetStatus oldBar 
    >?>= \status -> do
      -- Set status.
      interactivebarSetStatus newBar status
      -- Show.
      interactivebarShow box newBar

  return newBar

-- | Init.
interactivebarInit :: Interactivebar -> VBox -> String -> String -> IO ()
interactivebarInit interactivebar box title content = do
  interactivebarSetTitle interactivebar title
  interactivebarSetContent interactivebar content
  interactivebarShow box interactivebar

-- | Set title.
interactivebarSetTitle :: Interactivebar -> String -> IO ()  
interactivebarSetTitle bar =
  labelSetText (interactivebarTitleLabel bar) 

-- | Get title.
interactivebarGetTitle :: Interactivebar -> IO String
interactivebarGetTitle = 
  labelGetText . interactivebarTitleLabel

-- | Set content.
interactivebarSetContent :: Interactivebar -> String -> IO ()
interactivebarSetContent bar =
  editableSetText (interactivebarEntry bar)

-- | Show interactivebar.
interactivebarShow :: BoxClass box => box -> Interactivebar -> IO ()
interactivebarShow b bar = do
  -- Init.
  let iFrame = interactivebarFrame bar

  -- Connect to frame.
  boxTryPack (toBox b) iFrame PackNatural (Just 0) Nothing

  -- Focus entry.
  widgetShowAll iFrame
  editableFocus (interactivebarEntry bar)

-- | Exit interactivebar.
interactivebarExit :: BoxClass box => box -> Interactivebar -> IO ()
interactivebarExit b = 
    containerTryRemove (toBox b) . interactivebarFrame 

-- | Is visible.
interactivebarIsVisible :: Interactivebar -> IO Bool
interactivebarIsVisible = 
  widgetHasParent . interactivebarFrame

-- | Set status.
interactivebarSetStatus :: Interactivebar -> InteractivebarStatus -> IO ()
interactivebarSetStatus bar (title, entryStatus) = do
  interactivebarSetTitle bar title
  editableSetStatus (interactivebarEntry bar) entryStatus

-- | Get interactivebar status.
-- Return Nothing if interactivebar is not visible.
interactivebarGetStatus :: Interactivebar -> IO (Maybe InteractivebarStatus)
interactivebarGetStatus bar = 
    ifM (interactivebarIsVisible bar)
        (do
          title  <- labelGetText $ interactivebarTitleLabel bar
          entryStatus <- editableGetStatus $ interactivebarEntry bar
          return $ Just (title, entryStatus))
        (return Nothing)


