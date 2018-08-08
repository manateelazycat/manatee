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

module Manatee.Toolkit.Gtk.Editable where

import Control.Monad
import Graphics.UI.Gtk
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.String

type EditableContent = String
type EditableBound   = (Int, Int)
type EditableStatus  = (EditableContent, EditableBound)

-- | Focus editable and keep status.
editableFocus :: EditableClass self => self -> IO ()
editableFocus ed = do
  bound <- editableGetSelectionBounds ed
  widgetGrabFocus $ castToWidget ed
  editableSetBound ed bound

-- | Get unselect text.
editableGetUnselectText :: EditableClass self => self -> IO String
editableGetUnselectText ed = do
  allText   <- editableGetAllText ed         -- get editable content
  (start, _) <- editableGetSelectionBounds ed -- get highlight bound
  return $ take start allText               -- remove highlight completion part from content

-- | Select from current position to end.
editableSelectToEnd :: EditableClass self => self -> Int -> IO ()
editableSelectToEnd ed current = 
  editableSelectRegion ed current (-1)

-- | Get all text.
editableGetAllText :: EditableClass self => self -> IO String
editableGetAllText ed = editableGetChars ed 0 (-1)

-- | Set text.
editableSetText :: EditableClass self => self -> String -> IO ()
editableSetText ed content = do
  editableDeleteAllText ed
  end <- editableInsertText ed content 0
  editableSetPosition ed end

-- | Set completion text.
editableSetCompletionText :: EditableClass self => self -> String -> String -> IO ()
editableSetCompletionText ed input common = do
  editableSetText ed (input ++ common)
  editableSelectToEnd ed (length input)

-- | Set bound.
editableSetBound :: EditableClass self => self -> (Int, Int) -> IO ()  
editableSetBound ed (start, end) = 
  editableSelectRegion ed start end

-- | Get status.
editableGetStatus :: EditableClass self => self -> IO EditableStatus  
editableGetStatus ed = 
  liftM2 (,) (editableGetAllText ed) (editableGetSelectionBounds ed)

-- | Set status.
editableSetStatus :: EditableClass self => self -> EditableStatus -> IO ()
editableSetStatus ed (content, bound) = do
  editableSetText ed content
  editableSetBound ed bound

-- | Delete all text.
editableDeleteAllText :: EditableClass self => self -> IO ()  
editableDeleteAllText ed = editableDeleteText ed 0 (-1)

-- | Expand completion.
editableExpandCompletion :: EditableClass self => self -> IO ()
editableExpandCompletion ed =
  editableSetPosition ed (-1)

-- | Delete backward char.
editableDeleteBackwardChar :: EditableClass self => self -> IO ()
editableDeleteBackwardChar ed = do
  pos <- editableGetPosition ed
  editableDeleteText ed (max (pos - 1) 0) pos

-- | Delete forward char.
editableDeleteForwardChar :: EditableClass self => self -> IO ()
editableDeleteForwardChar ed = do
  pos <- editableGetPosition ed
  len <- editableGetLenth ed
  editableDeleteText ed pos (min (pos + 1) len)

-- | Delete backward word.
editableDeleteBackwardWord :: EditableClass self => self -> IO ()
editableDeleteBackwardWord ed = do
  (firstStr, secondStr) <- editableGetParts ed
  let (word, restStr) = searchBackwardWord firstStr
      text = if null word
                -- If not word at last, delete char.
                then init_ firstStr
                -- Otherwise delete word.
                else restStr
  editableSetText ed (text ++ secondStr)
  editableSetPosition ed (length text)

-- | Delete to start.
editableDeleteToStart :: EditableClass self => self -> IO ()
editableDeleteToStart ed = do
  pos <- editableGetPosition ed
  editableDeleteText ed 0 pos
  
-- | Delete to end.  
editableDeleteToEnd :: EditableClass self => self -> IO ()
editableDeleteToEnd ed = do
  pos <- editableGetPosition ed
  editableDeleteText ed pos (-1)

-- | Move to start.
editableMoveToStart :: EditableClass self => self -> IO ()
editableMoveToStart ed =
    editableSetPosition ed 0

-- | Move to start.
editableMoveToEnd :: EditableClass self => self -> IO ()
editableMoveToEnd ed =
    editableSetPosition ed (-1)

-- | Delete forward word.
editableDeleteForwardWord :: EditableClass self => self -> IO ()
editableDeleteForwardWord ed = do
  (firstStr, secondStr) <- editableGetParts ed
  let (word, restStr) = searchForwardWord secondStr
      text = if null word
                -- If not word at last, delete char.
                then tail_ secondStr
                -- Otherwise delete word.
                else restStr
  editableSetText ed (firstStr ++ text)
  editableSetPosition ed (length firstStr)

-- | Backward word.
editableBackwardWord :: EditableClass self => self -> IO ()
editableBackwardWord ed = do
  pos <- editableGetPosition ed
  (firstStr, _) <- editableGetParts ed
  let (word, _) = searchBackwardWord firstStr
      offset = if null word
                  -- If not word at last, delete char.
                  then 1
                  -- Otherwise delete word.
                  else length word
  editableSetPosition ed (max (pos - offset) 0)

-- | Forward word.
editableForwardWord :: EditableClass self => self -> IO ()
editableForwardWord ed = do
  pos <- editableGetPosition ed
  (firstStr, secondStr) <- editableGetParts ed
  let (word, _) = searchForwardWord secondStr
      offset = if null word
                  -- If not word at last, delete char.
                  then 1
                  -- Otherwise delete word.
                  else length word
  editableSetPosition ed (min (pos + offset) (length (firstStr ++ secondStr)))

-- | Backward char.
editableBackwardChar :: EditableClass self => self -> IO ()
editableBackwardChar ed = do
  pos <- editableGetPosition ed  
  editableSetPosition ed (max (pos - 1) 0)

-- | Forward char.
editableForwardChar :: EditableClass self => self -> IO ()  
editableForwardChar ed = do
  pos <- editableGetPosition ed  
  len <- editableGetLenth ed
  editableSetPosition ed (min (pos + 1) len)

-- | Get length of editable.
editableGetLenth :: EditableClass self => self -> IO Int
editableGetLenth ed =
  editableGetChars ed 0 (-1)
  >>= return . length

-- | Whether changed after do editable action?
editableIsChanged :: EditableClass self => self -> IO () -> IO Bool
editableIsChanged ed action = do
  let getState = do
        text <- editableGetAllText ed
        (start, end) <- editableGetSelectionBounds ed
        return (text, start, end)
  beforeState <- getState
  action
  afterState <- getState
  return (beforeState /= afterState)

-- | Select all.
editableSelectAll :: EditableClass self => self -> IO ()
editableSelectAll ed = 
    editableSelectRegion ed 0 (-1)

-- | Get parts that before/after cursor.
editableGetParts :: EditableClass self => self -> IO (String, String)
editableGetParts ed = do
  pos <- editableGetPosition ed  
  firstPart  <- editableGetChars ed 0 pos
  secondPart <- editableGetChars ed pos (-1)
  return (firstPart, secondPart)
