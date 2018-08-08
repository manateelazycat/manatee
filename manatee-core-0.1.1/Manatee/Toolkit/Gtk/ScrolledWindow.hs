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

module Manatee.Toolkit.Gtk.ScrolledWindow where

import Graphics.UI.Gtk

data AdjustmentDirection = AdjustmentVertical
                         | AdjustmentHorizontal

-- | Like `scrolledWindowNew', except the scrollbar is present only if needed.
scrolledWindowNew_ :: IO ScrolledWindow
scrolledWindowNew_ = do
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAutomatic
  return scrolledWindow

-- | Scroll to begin vertically.
scrolledWindowScrollToTop, scrolledWindowScrollToLeft :: ScrolledWindowClass sw => sw -> IO ()
scrolledWindowScrollToTop = 
  scrolledWindowScrollToLower AdjustmentVertical 
scrolledWindowScrollToLeft =
  scrolledWindowScrollToLower AdjustmentHorizontal

-- | Scroll to end vertically.
scrolledWindowScrollToBottom, scrolledWindowScrollToRight :: ScrolledWindowClass sw => sw -> IO ()
scrolledWindowScrollToBottom = 
  scrolledWindowScrollToUpper AdjustmentVertical
scrolledWindowScrollToRight =
  scrolledWindowScrollToUpper AdjustmentHorizontal

-- | Scroll page vertically.
scrolledWindowScrollVerticalPage, scrolledWindowScrollHorizontalPage :: ScrolledWindowClass sw => Bool -> sw -> IO ()
scrolledWindowScrollVerticalPage = 
  scrolledWindowScrollPage AdjustmentVertical 
scrolledWindowScrollHorizontalPage =
  scrolledWindowScrollPage AdjustmentHorizontal

scrolledWindowScrollVerticalStep, scrolledWindowScrollHorizontalStep :: ScrolledWindowClass sw => Bool -> sw -> IO ()
scrolledWindowScrollVerticalStep = 
  scrolledWindowScrollStep AdjustmentVertical
scrolledWindowScrollHorizontalStep =
  scrolledWindowScrollStep AdjustmentHorizontal

-- | At top.
scrolledWindowIsAtTop, scrolledWindowIsAtLeft :: ScrolledWindowClass sw => sw -> IO Bool  
scrolledWindowIsAtTop = 
  scrolledWindowIsAtLower AdjustmentVertical
scrolledWindowIsAtLeft = 
  scrolledWindowIsAtLower AdjustmentHorizontal
  
-- | At bottom.  
scrolledWindowIsAtBottom, scrolledWindowIsAtRight :: ScrolledWindowClass sw => sw -> IO Bool
scrolledWindowIsAtBottom = 
  scrolledWindowIsAtUpper AdjustmentVertical
scrolledWindowIsAtRight =
  scrolledWindowIsAtUpper AdjustmentHorizontal
  
-- | Get adjustment.
scrolledWindowGetAdjustment :: ScrolledWindowClass sw => sw -> AdjustmentDirection -> IO Adjustment
scrolledWindowGetAdjustment sw AdjustmentVertical = 
    scrolledWindowGetVAdjustment sw
scrolledWindowGetAdjustment sw AdjustmentHorizontal = 
    scrolledWindowGetHAdjustment sw
  
-- | Scroll to begin vertically.
scrolledWindowScrollToLower :: ScrolledWindowClass sw => AdjustmentDirection -> sw -> IO ()
scrolledWindowScrollToLower direction sw = do
  aj <- scrolledWindowGetAdjustment sw direction
  lo <- adjustmentGetLower aj
  adjustmentSetValue aj lo

-- | Scroll to end vertically.
scrolledWindowScrollToUpper :: ScrolledWindowClass sw => AdjustmentDirection -> sw -> IO ()
scrolledWindowScrollToUpper direction sw = do
  aj <- scrolledWindowGetAdjustment sw direction
  up <- adjustmentGetUpper aj
  ps <- adjustmentGetPageSize aj
  adjustmentSetValue aj (up - ps)

-- | Scroll page vertically.
scrolledWindowScrollPage :: ScrolledWindowClass sw => AdjustmentDirection -> Bool -> sw -> IO ()
scrolledWindowScrollPage direction isDown sw = do
  aj <- scrolledWindowGetAdjustment sw direction
  ps <- adjustmentGetPageSize aj
  lo <- adjustmentGetLower aj
  up <- adjustmentGetUpper aj
  cr <- adjustmentGetValue aj
  adjustmentSetValue aj (if isDown
                            then min (up - ps) (cr + ps)
                            else max lo (cr - ps))

-- | Scroll page vertically.
scrolledWindowScrollStep :: ScrolledWindowClass sw => AdjustmentDirection -> Bool -> sw -> IO ()
scrolledWindowScrollStep direction isDown sw = do
  aj <- scrolledWindowGetAdjustment sw direction
  si <- adjustmentGetStepIncrement aj
  ps <- adjustmentGetPageSize aj
  lo <- adjustmentGetLower aj
  up <- adjustmentGetUpper aj
  cr <- adjustmentGetValue aj
  adjustmentSetValue aj (if isDown
                            then min (up - ps) (cr + si)
                            else max lo (cr - si))

-- | At top.
scrolledWindowIsAtLower :: ScrolledWindowClass sw => AdjustmentDirection -> sw -> IO Bool  
scrolledWindowIsAtLower direction sw = do
  aj <- scrolledWindowGetAdjustment sw direction
  lo <- adjustmentGetLower aj
  cr <- adjustmentGetValue aj
  return (cr <= lo)
  
-- | At bottom.  
scrolledWindowIsAtUpper :: ScrolledWindowClass sw => AdjustmentDirection -> sw -> IO Bool
scrolledWindowIsAtUpper direction sw = do
  aj <- scrolledWindowGetAdjustment sw direction
  ps <- adjustmentGetPageSize aj
  up <- adjustmentGetUpper aj
  cr <- adjustmentGetValue aj
  return (cr >= up - ps)
  
-- | Get value.  
scrolledWindowGetValue :: ScrolledWindowClass sw => sw -> IO (Double, Double)  
scrolledWindowGetValue sw = do
  hAdj <- scrolledWindowGetHAdjustment sw
  vAdj <- scrolledWindowGetVAdjustment sw
  hValue <- adjustmentGetValue hAdj
  vValue <- adjustmentGetValue vAdj
  return (hValue, vValue)

-- | Set value.
scrolledWindowSetValue :: ScrolledWindowClass sw => sw -> (Double, Double) -> IO ()
scrolledWindowSetValue sw (x, y) = do
  hAdj <- scrolledWindowGetHAdjustment sw
  vAdj <- scrolledWindowGetVAdjustment sw
  adjustmentSetValue hAdj x
  adjustmentSetValue vAdj y
  
-- | Get max size.  
scrolledWindowGetSize :: ScrolledWindowClass sw => sw -> IO (Double, Double)  
scrolledWindowGetSize sw = do
  hAdj <- scrolledWindowGetHAdjustment sw
  vAdj <- scrolledWindowGetVAdjustment sw
  hSize <- adjustmentGetUpper hAdj
  vSize <- adjustmentGetUpper vAdj
  return (hSize, vSize)

-- | Set upper.
scrolledWindowSetUpper :: ScrolledWindowClass sw => sw -> (Double, Double) -> IO ()  
scrolledWindowSetUpper sw (w, h) = do
  hAdj <- scrolledWindowGetHAdjustment sw
  vAdj <- scrolledWindowGetVAdjustment sw
  adjustmentSetUpper hAdj w
  adjustmentSetUpper vAdj h
