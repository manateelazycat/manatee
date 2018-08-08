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

module Manatee.Toolkit.Gtk.Notebook where

import Control.Monad
import Graphics.UI.Gtk
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Functor
import Manatee.Toolkit.General.Maybe

-- | In Gtk+ C code, GtkNotebook refuses to switch to a page unless the child widget is visible. 
-- So wrap below functions that show child widget before add them to a notebook. 
-- Details see documents of `gtk_notebook_set_current_page`.

notebookAppendPage_ :: (NotebookClass notebook, WidgetClass child) => notebook -> child -> String -> IO Int   
notebookAppendPage_ notebook child str = 
  widgetShowAll child >> notebookAppendPage notebook child str

notebookAppendPageMenu_ :: (NotebookClass notebook, WidgetClass child, WidgetClass tabLabel, WidgetClass menuLabel) => 
                          notebook -> child -> tabLabel -> menuLabel -> IO Int
notebookAppendPageMenu_ notebook child tabLabel menuLabel =
  widgetShowAll child >> widgetShowAll tabLabel >> notebookAppendPageMenu notebook child tabLabel menuLabel

notebookAppendPageTab_ :: (NotebookClass notebook, WidgetClass child, WidgetClass tabLabel) => 
                          notebook -> child -> tabLabel -> IO Int
notebookAppendPageTab_ notebook child tabLabel = do
  menuLabel <- labelNew Nothing
  widgetShowAll child >> widgetShowAll tabLabel >> notebookAppendPageMenu notebook child tabLabel menuLabel

notebookPrependPage_ :: (NotebookClass notebook, WidgetClass child) => notebook -> child -> String -> IO Int      
notebookPrependPage_ notebook child str =
  widgetShowAll child >> notebookPrependPage notebook child str

notebookPrependPageMenu_ :: (NotebookClass notebook, WidgetClass child, WidgetClass tabLabel, WidgetClass menuLabel) => 
                          notebook -> child -> tabLabel -> menuLabel -> IO Int
notebookPrependPageMenu_ notebook child tabLabel menuLabel =
  widgetShowAll child >> widgetShowAll tabLabel >> notebookPrependPageMenu notebook child tabLabel menuLabel

notebookPrependPageTab_ :: (NotebookClass notebook, WidgetClass child, WidgetClass tabLabel) => 
                          notebook -> child -> tabLabel -> IO Int
notebookPrependPageTab_ notebook child tabLabel = do
  menuLabel <- labelNew Nothing
  widgetShowAll child >> widgetShowAll tabLabel >> notebookPrependPageMenu notebook child tabLabel menuLabel

notebookInsertPage_ :: (NotebookClass notebook, WidgetClass child) => notebook -> child -> String -> Int -> IO Int      
notebookInsertPage_ notebook child str pos =
  widgetShowAll child >> notebookInsertPage notebook child str pos

notebookInsertPageMenu_ :: (NotebookClass notebook, WidgetClass child, WidgetClass tabLabel, WidgetClass menuLabel) => 
                         notebook -> child -> tabLabel -> menuLabel -> Int -> IO Int
notebookInsertPageMenu_ notebook child tabLabel menuLabel pos =
  widgetShowAll child >> widgetShowAll tabLabel >> notebookInsertPageMenu notebook child tabLabel menuLabel pos

notebookInsertPageTab_ :: (NotebookClass notebook, WidgetClass child, WidgetClass tabLabel) => 
                         notebook -> child -> tabLabel -> Int -> IO Int
notebookInsertPageTab_ notebook child tabLabel pos = do
  menuLabel <- labelNew Nothing
  widgetShowAll child >> widgetShowAll tabLabel >> notebookInsertPageMenu notebook child tabLabel menuLabel pos

-- | Remove child, just remove it's page when child is contained in notebook.
notebookRemoveChild :: (NotebookClass notebook, WidgetClass child) => notebook -> child -> IO ()
notebookRemoveChild notebook child = 
    notebookPageNum notebook child >>= 
    (?>= notebookRemovePage notebook)
  
-- | Insert page and make child reorderable.
notebookInsertReorderPage :: (NotebookClass notebook, WidgetClass child) => notebook -> child -> String -> Int -> IO ()
notebookInsertReorderPage notebook child str pos = do    
  notebookInsertPage_ notebook child str pos
  notebookSetTabReorderable notebook child True -- make tab can reorderable

-- | Focus current page.
notebookFocusCurrentPage :: NotebookClass notebook => notebook -> IO ()
notebookFocusCurrentPage notebook = do
  -- Focus notebook first.
  widgetGrabFocus notebook

  -- Get current page.
  page  <- notebookGetCurrentPage_ notebook

  -- Focus current page
  page ?>= widgetGrabFocus

-- | Get current page.
-- Like `notebookGetCurrentPage` except use widget instead current page index.
notebookGetCurrentPage_ :: NotebookClass notebook => notebook -> IO (Maybe Widget)
notebookGetCurrentPage_ notebook =
  notebookGetCurrentPage notebook
  >>= notebookGetNthPage notebook

-- | Select next page.
notebookSelectNextPage :: NotebookClass notebook => notebook -> IO ()
notebookSelectNextPage notebook = 
    ifF notebook notebookAtEnd notebookSelectFirstPage notebookNextPage

-- | Select previous page.
notebookSelectPrevPage :: NotebookClass notebook => notebook -> IO ()
notebookSelectPrevPage notebook = 
    ifF notebook notebookAtStart notebookSelectLastPage notebookPrevPage

-- | Select first page.
notebookSelectFirstPage :: NotebookClass notebook => notebook -> IO ()
notebookSelectFirstPage notebook = 
    notebookSetCurrentPage notebook
        =<< notebookFirstIndex notebook

-- | Select last page.
notebookSelectLastPage :: NotebookClass notebook => notebook -> IO ()
notebookSelectLastPage notebook = 
    notebookSetCurrentPage notebook
        =<< notebookLastIndex notebook

-- | At start.
notebookAtStart :: NotebookClass notebook => notebook -> IO Bool
notebookAtStart notebook = 
    liftM2 (==) (notebookGetCurrentPage notebook) (notebookFirstIndex notebook)

-- | At End.
notebookAtEnd :: NotebookClass notebook => notebook -> IO Bool
notebookAtEnd notebook = 
    liftM2 (==) (notebookGetCurrentPage notebook) (notebookLastIndex notebook)
  
-- | Get tab first index.
notebookFirstIndex :: NotebookClass notebook => notebook -> IO Int
notebookFirstIndex _ = return 0

-- | Get tab last index.
notebookLastIndex :: NotebookClass notebook => notebook -> IO Int
notebookLastIndex = (<<<=) pred notebookGetNPages

-- | Set name with given index.
notebookSetTabName :: NotebookClass notebook => notebook -> Int -> String -> IO ()
notebookSetTabName notebook index name =
  notebookGetNthPage notebook index
    >?>= \child -> notebookSetTabLabelText notebook child name