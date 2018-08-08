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

module Manatee.Toolkit.Gtk.ModelView where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.Gtk.Gtk

-- | Focus first toplevel node.
-- It focus first node if model is list model.
treeViewFocusFirstToplevelNode :: TreeViewClass view => view -> IO ()
treeViewFocusFirstToplevelNode view =
  treeViewSetCursor view [0] Nothing

-- | Focus last toplevel node.
-- It focus last node if model is list model.
treeViewFocusLastToplevelNode :: TreeViewClass view => view -> IO ()
treeViewFocusLastToplevelNode view =
    treeViewApplyModel view $ \model -> do
      lastPath <- treeModelLastToplevelPath model
      treeViewSetCursor view lastPath Nothing

-- | Get number of toplevel node in TreeView.
treeViewGetToplevelNodeCount :: TreeViewClass view => view -> IO Int
treeViewGetToplevelNodeCount view = do
  model <- treeViewGetModel view
  case model of
    Just ml -> treeModelGetToplevelNodeCount ml
    Nothing -> return 0

-- | Get number of toplevel node in TreeModel.
treeModelGetToplevelNodeCount :: TreeModelClass model => model -> IO Int
treeModelGetToplevelNodeCount model =
  treeModelIterNChildren model Nothing

-- | Apply treeView model.
treeViewApplyModel :: TreeViewClass view => view -> (TreeModel -> IO ()) -> IO ()
treeViewApplyModel view f =
    treeViewGetModel view >?>= f

-- | Get current selection. 
-- Don't *store* TreeIter, it's wrong value after model change.
-- It's should just as a argument for another IO function.
treeViewGetSelectedPath :: TreeViewClass view => view -> IO (Maybe TreePath)
treeViewGetSelectedPath view = 
  fmap maybeHead $
       treeViewGetSelection view
           >>= treeSelectionGetSelectedRows

-- | Get tree coordinate of cell.
treeViewGetCellTreeCoordinate :: TreeViewClass view => view -> Maybe TreePath -> TreeViewColumn -> IO Rectangle
treeViewGetCellTreeCoordinate view treePath column = do
  -- Get coordinate relative to *visible area* of tree.
  (Rectangle cx cy cw ch) <- treeViewGetBackgroundArea view treePath column

  -- Try to get adjustment value.
  hAdjust <- treeViewGetHAdjustment view
  adjustX <- case hAdjust of
              Just ha -> ceiling <$> adjustmentGetValue ha
              Nothing -> return 0

  vAdjust <- treeViewGetVAdjustment view
  adjustY <- case vAdjust of
              Just va -> ceiling <$> adjustmentGetValue va
              Nothing -> return 0

  -- Because `treeViewGetSelectedBound` just return coordinate 
  -- relative to visible area of tree.
  -- For get coordinate relative to full scrollable area of tree,
  -- We need add value of adjustment.
  return (Rectangle (cx + adjustX) (cy + adjustY) cw ch)

-- | Get selection bound.
treeViewGetSelectedTreeCoordidnate :: TreeViewClass view => view -> IO (Maybe Rectangle)
treeViewGetSelectedTreeCoordidnate view = do
  treePath <- treeViewGetSelectedPath view
  treePath 
    ?>=> \ tp -> treeViewGetColumn view 0
         >?>=> (\x -> Just <$> treeViewGetCellTreeCoordinate view (Just tp) x)

-- | Get selection widget coordinate.
treeViewGetSelectedWidgetCoordinate :: TreeViewClass view => view -> IO (Maybe Rectangle)
treeViewGetSelectedWidgetCoordinate view = 
  treeViewGetSelectedTreeCoordidnate view
  >?>=> \(Rectangle rx ry rw rh) -> do
         -- Transform tree coordinate to widget coordinate.
         (wx, wy) <- treeViewConvertTreeToWidgetCoords view (rx, ry)

         return $ Just (Rectangle wx wy rw rh)

-- | Focus next toplevel node.
treeViewFocusNextToplevelNode :: TreeViewClass view => view -> IO ()
treeViewFocusNextToplevelNode view = 
  treeViewGetSelectedPath view
    >?>= \ currentPath -> do
           number <- treeViewGetToplevelNodeCount view
           let currentRootNode = head currentPath
           when (currentRootNode < number - 1) $ 
               treeViewSetCursor view [currentRootNode + 1] Nothing

-- | Focus prev toplevel node.
-- Return True if focus prev toplevel node,  
-- Otherwise return False.
treeViewFocusPrevToplevelNode :: TreeViewClass view => view -> IO ()
treeViewFocusPrevToplevelNode view = 
  treeViewGetSelectedPath view
    >?>= \ currentPath -> do
         let currentRootNode = head currentPath
         when (currentRootNode > 0) $ 
              treeViewSetCursor view [currentRootNode - 1] Nothing

-- | Last toplevel node path of TreeModel.
treeModelLastToplevelPath :: TreeModelClass model => model -> IO TreePath
treeModelLastToplevelPath model = do
    number <- treeModelGetToplevelNodeCount model
    return $ if number > 0 then [number - 1] else [0]
                
-- | Whether at first toplevel node?
-- It at first node if model is list model.
treeViewAtFirstToplevelNode :: TreeViewClass view => view -> IO Bool
treeViewAtFirstToplevelNode view = do
  currentPath <- treeViewGetSelectedPath view
  return $ case currentPath of
             Just cp -> head cp == 0 
             Nothing -> False

-- | Whether at last toplevel node?
-- It at last node if model is list model.
treeViewAtLastToplevelNode :: TreeViewClass view => view -> IO Bool
treeViewAtLastToplevelNode view = do
  currentPath <- treeViewGetSelectedPath view
  nodeCount <- treeViewGetToplevelNodeCount view
  return $ case currentPath of
             Just cp -> head cp == nodeCount - 1
             Nothing -> False

-- | Remove all column's from treeView.
treeViewRemoveColumns :: TreeViewClass self => self -> IO ()
treeViewRemoveColumns treeView = 
    mapM_ (treeViewRemoveColumn treeView) 
        =<< treeViewGetColumns treeView

-- | Have cell columns.
treeViewHaveColumn :: TreeViewClass self => self -> IO Bool
treeViewHaveColumn treeView = 
  has <$> treeViewGetColumns treeView

-- | Un-select all.
treeViewUnselectAll :: TreeViewClass view => view -> IO ()
treeViewUnselectAll view = 
  treeViewGetSelection view
  >>= treeSelectionUnselectAll

-- | Get default cell height.
treeViewGetDefaultCellHeight :: TreeViewClass view => view -> IO (Maybe Int)
treeViewGetDefaultCellHeight view = 
  treeViewGetColumn view 0
    >?>=> \col -> 
           (Just . rectangleH) <$> treeViewGetBackgroundArea view (Just [0]) col

-- | Get attribute of cursor cell.
treeViewGetSelectedCellArea :: TreeViewClass self => (Rectangle -> Int) -> self -> IO Int
treeViewGetSelectedCellArea f treeView = do
  (path, column) <- treeViewGetCursor treeView
  case column of
    -- Get attribute.
    Just c  -> (return . f) =<< treeViewGetCellArea treeView (Just path) c
    -- Otherwise return 0.
    Nothing -> return 0

-- | Get current cell height.
treeViewGetSelectedCellHeight :: TreeViewClass view => view -> IO Int
treeViewGetSelectedCellHeight = treeViewGetSelectedCellArea rectangleH

-- | Get current cell height.
treeViewGetSelectedCellY :: TreeViewClass view => view -> IO Int
treeViewGetSelectedCellY = treeViewGetSelectedCellArea rectangleY

-- | Get header height.
treeViewGetHeaderHeight :: TreeViewClass self => self -> IO Int
treeViewGetHeaderHeight treeView = do
  (_, by) <- treeViewConvertTreeToBinWindowCoords treeView (0, 0)
  (_, wy) <- treeViewConvertTreeToWidgetCoords treeView (0, 0)
  return $ wy - by

-- | Focus TreeView and keep current selected position.
treeViewFocus :: TreeViewClass self => self -> IO ()
treeViewFocus view = do
  selectedPath <- treeViewGetSelectedPath view
  let focusPath = fromMaybe [0] selectedPath
  treeViewSetCursor view focusPath Nothing 
  widgetGrabFocus view

-- | Scroll vertical.
treeViewScrollVertical :: (TreeViewClass self, ScrolledWindowClass swc) => self -> swc -> Double -> IO ()
treeViewScrollVertical view swc increment = do
  -- Get adjustment arguments.
  aj <- scrolledWindowGetVAdjustment swc
  ps <- adjustmentGetPageSize aj
  vl <- adjustmentGetValue aj
  ur <- adjustmentGetUpper aj
  lr <- adjustmentGetLower aj

  -- Get y coordinate and height of cell.
  cellY <- do
    cy <- treeViewGetSelectedCellY view
    -- Transform to tree coordinate for compare with adjustment value.
    (i2d . snd) <$> treeViewConvertBinWindowToTreeCoords view (0, cy)
  ch <- treeViewGetSelectedCellHeight view
  let cellH = i2d ch

  -- Update adjustment value.
  let inc = i2d $ floorToMultiple (truncate increment) ch
      iv  = vl + inc
      top = if inc >= 0
              then min iv $ ur - cellH -- don't bigger than upper value when scroll up
              else max iv lr           -- don't less than lower value when scroll down
      bottom = top + ps
  
  -- Calculate new y coordinate when cell at outside of visible area.
  let adjustY | top    >= cellY         = Just topY
              | bottom <= cellY + cellH = Just bottomY
              | otherwise               = Nothing
              where topY    = top + 1
                    bottomY = i2d $ floorToMultiple (truncate bottom) ch - 1

  -- Selected new cell when got new y coordinate.
  adjustY ?>= \ y -> do
    p <- treeViewConvertTreeToBinWindowCoords view (0, truncate y)
    treeViewGetPathAtPos view p >?>= 
                        \ (path, _, _) -> treeViewSetCursor view path Nothing

  -- Set adjustment.
  adjustmentSetValue aj top

treeViewAddColumnWithTitle :: TreeViewClass self => self -> String -> SortColumnId -> IO TreeViewColumn
treeViewAddColumnWithTitle treeView title sortId = do
  -- Get treeViewColumn.
  tvc <- treeViewColumnNew
  treeViewAppendColumn treeView tvc

  -- Set attribute.
  set tvc [treeViewColumnTitle          := title
          ,treeViewColumnResizable      := True
          ,treeViewColumnSortIndicator  := True
          ,treeViewColumnSortColumnId   := sortId]

  return tvc

-- | Get row.
treeModelSortGetRow :: (TreeModelSortClass self, 
                       TypedTreeModelClass model) 
 => model row
 -> self
 -> TreeIter
 -> IO row
treeModelSortGetRow model = 
    (<=<) (treeModelGetRow model) . treeModelSortConvertIterToChildIter

-- | Get next sort path.
-- Pass unsorted path to get next path in the given sorted model.
-- Very useful to track path after change sort rule.
treeViewNextSortPath :: (TreeViewClass view
                       ,TreeModelClass sortModel
                       ,TreeModelSortClass sortModel) 
                       => view 
                       -> sortModel
                       -> TreePath
                       -> IO TreePath
treeViewNextSortPath treeView sortModel path = do
  -- Converts the given path to a path relative to the given sorted model. 
  currentIndex <- liftM head $ treeModelSortConvertChildPathToPath sortModel path
  -- Get tree view size and index of next path.
  size <- treeViewGetToplevelNodeCount treeView
  let nextIndex = if currentIndex >= size - 1
                     then 0
                     else currentIndex + 1
  -- Converts path in the sorted model to a path on the unsorted model on which the given TreeModelSort is based. 
  treeModelSortConvertPathToChildPath sortModel [nextIndex]

-- | Get previous sort path.
-- Pass unsorted path to get previous path in the given sorted model.
-- Very useful to track path after change sort rule.
treeViewPrevSortPath :: (TreeViewClass view
                       ,TreeModelClass sortModel
                       ,TreeModelSortClass sortModel) 
                       => view 
                       -> sortModel
                       -> TreePath
                       -> IO TreePath
treeViewPrevSortPath treeView sortModel path = do
  -- Converts the given path to a path relative to the given sorted model. 
  currentIndex <- liftM head $ treeModelSortConvertChildPathToPath sortModel path
  -- Get tree view size and index of prev path.
  size <- treeViewGetToplevelNodeCount treeView
  let prevIndex = if currentIndex <= 0
                     then size - 1
                     else currentIndex - 1
  -- Converts path in the sorted model to a path on the unsorted model on which the given TreeModelSort is based. 
  treeModelSortConvertPathToChildPath sortModel [prevIndex]

-- | Get current value.
treeViewGetSelectedValue :: (TreeViewClass view
                           ,TreeModelSortClass self) 
                         => view
                         -> self
                         -> ListStore a
                         -> IO (Maybe a)
treeViewGetSelectedValue view sortModel listStore = 
  treeViewGetSelectedPath view
    >?>=> \ path -> do
      currentPath <- treeModelSortConvertPathToChildPath sortModel path
      liftM Just $ listStoreGetValue listStore (head currentPath)
  
-- | Icon view select first.
iconViewSelectFirstNode :: IconViewClass iconView => iconView -> IO ()
iconViewSelectFirstNode iconView = 
  iconViewSelectItem iconView [0]

-- | Icon view select last top node.
iconViewSelectLastNode :: IconViewClass iconView => iconView -> IO ()
iconViewSelectLastNode iconView = 
  iconViewGetModel iconView >?>= \model -> do
    lastPath <- treeModelLastToplevelPath model
    iconViewSelectItem iconView lastPath

-- | Icon view select element.
iconViewSelectItem :: IconViewClass iconView => iconView -> TreePath -> IO ()    
iconViewSelectItem iconView path = do
  iconViewSetCursor iconView (Left path :: Either TreePath CellRendererText) False
  iconViewSelectPath iconView path

-- | Select next node.
iconViewSelectNextColumnNode :: IconViewClass iconView => iconView -> IO ()
iconViewSelectNextColumnNode iconView = do
  path <- iconViewGetSelectPath iconView
  case path of
    Nothing -> iconViewSelectFirstNode iconView
    Just currentPath -> do
        number <- iconViewGetNodeCount iconView
        let currentRootNode = head currentPath
        when (currentRootNode < number - 1) $ 
             iconViewSelectItem iconView [currentRootNode + 1]

-- | Select previous node.
iconViewSelectPrevColumnNode :: IconViewClass iconView => iconView -> IO ()
iconViewSelectPrevColumnNode iconView = do
  path <- iconViewGetSelectPath iconView
  case path of
    Nothing -> iconViewSelectFirstNode iconView
    Just currentPath -> do
        let currentRootNode = head currentPath
        when (currentRootNode > 0) $ 
             iconViewSelectItem iconView [currentRootNode - 1]

-- | Select next row node.
iconViewSelectNextRowNode :: IconViewClass iconView => iconView -> IO ()
iconViewSelectNextRowNode iconView = do
  path <- iconViewGetSelectPath iconView
  case path of
    Nothing -> iconViewSelectFirstNode iconView
    Just currentPath -> do
        number <- iconViewGetNodeCount iconView
        columns <- iconViewGetColumns iconView
        let currentRootNode = head currentPath
        when (currentRootNode + columns <= number - 1) $
             iconViewSelectItem iconView [currentRootNode + columns]

-- | Select previous node.
iconViewSelectPrevRowNode :: IconViewClass iconView => iconView -> IO ()
iconViewSelectPrevRowNode iconView = do
  path <- iconViewGetSelectPath iconView
  case path of
    Nothing -> iconViewSelectFirstNode iconView
    Just currentPath -> do
        columns <- iconViewGetColumns iconView
        let currentRootNode = head currentPath
        when (currentRootNode - columns >= 0) $
             iconViewSelectItem iconView [currentRootNode - columns]

-- | Icon view activated cursor.
iconViewActivatedCursor :: IconViewClass iconView => iconView -> IO ()    
iconViewActivatedCursor iconView = 
    iconViewGetSelectPath iconView >?>= \path ->
        iconViewItemActivated iconView path

-- | Icon view get select path.
iconViewGetSelectPath :: IconViewClass iconView => iconView -> IO (Maybe TreePath)
iconViewGetSelectPath iconView = do
  (path, _) <- iconViewGetCursor iconView
  case path of
    [] -> return Nothing
    _  -> return $ Just path

-- | Get number of toplevel node in TreeView.
iconViewGetNodeCount :: IconViewClass iconView => iconView -> IO Int
iconViewGetNodeCount iconView = do
  model <- iconViewGetModel iconView
  case model of
    Just ml -> treeModelGetToplevelNodeCount ml
    Nothing -> return 0

-- | Icon view focus.
iconViewFocus :: IconViewClass iconView => iconView -> IO ()    
iconViewFocus iconView = do
  path <- iconViewGetSelectPath iconView
  case path of
    Just p  -> iconViewSelectItem iconView p
    Nothing -> iconViewSelectFirstNode iconView
