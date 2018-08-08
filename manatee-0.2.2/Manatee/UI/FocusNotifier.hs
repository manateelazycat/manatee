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

-- | FocusNotifier use Cairo draw highlight frame when widget is focus.
module Manatee.UI.FocusNotifier where

import Control.Concurrent.STM 
import Control.Monad
import Control.Monad.Trans
import Data.Ord
import Data.Set (Set)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk 
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.Set
import Manatee.Toolkit.Gtk.Cairo
import Manatee.Toolkit.Gtk.Gtk

import qualified Data.Function as Fun
import qualified Data.Set as Set

data FocusNotifier =
    FocusNotifier {focusNotifierId          :: FocusNotifierId
                  ,focusNotifierBody        :: Widget
                  ,focusNotifierFocus       :: Bool}

type FocusNotifierId = Int

data FocusNotifierList = FocusNotifierList (Set FocusNotifier) (Maybe FocusNotifierId)

instance Eq FocusNotifier where
    (==) = (==) `Fun.on` focusNotifierId

instance Ord FocusNotifier where
    compare = comparing focusNotifierId

-- | The frame size for notifier.
focusNotifierSize :: Int
focusNotifierSize = 2

-- | The color of notifier.
focusNotifierColor :: Color
focusNotifierColor = Color 61440 30464 17920

-- | The alpha value of color.
focusNotifierAlpha :: Double
focusNotifierAlpha = 0.7

-- | Create new FocusNotifier.
focusNotifierNew :: WidgetClass widget => FocusNotifierId -> widget -> TVar FocusNotifierList -> IO FocusNotifier
focusNotifierNew frameId widget focusNotifierList = do
  -- Create new FocusNotifier.
  let focusNotifier = FocusNotifier frameId (toWidget widget) False

  -- Add FocusNotifier to list.
  modifyTVarIO focusNotifierList $ \(FocusNotifierList setList currentFocusId) -> 
      (FocusNotifierList (Set.insert focusNotifier setList ) currentFocusId)

  -- Try draw highlight frame when widget is focus.
  -- Don't use : `on` exposeEvent, 
  -- otherwise child widget can't work (such as notebook)
  widget `after` exposeEvent $ tryEvent $ liftIO $ 
    -- If current widget is focus, draw highlight frame abound widget.
    whenM (focusNotifierIsFocus frameId focusNotifierList) $ do
      (Rectangle x y w h) <- widgetGetAllocation widget -- get widget size.
      frameWin <- widgetGetDrawWindow widget            -- get draw window
      renderWithDrawable frameWin $                    -- draw highlight frame
        focusNotifierDraw (i2d x) (i2d y) (i2d w) (i2d h) (i2d focusNotifierSize)

  -- Delete FocusNotifier when widget is destroy.
  widget `on` destroyEvent $ tryEvent $ liftIO $ 
    modifyTVarIO focusNotifierList $ \(FocusNotifierList setList currentFocusId) -> 
        -- Delete FocusNotifier from list.
        let newSetList = Set.delete focusNotifier setList
            newFocusId = case currentFocusId of
                           -- Change focus id if it equal delete one.
                           Just i -> if i == focusNotifierId focusNotifier
                                       then Nothing 
                                       else Just i
                           Nothing -> Nothing

        -- Update list.
        in (FocusNotifierList newSetList newFocusId)

  -- Return.
  return focusNotifier

-- Widget is focus?
focusNotifierIsFocus :: FocusNotifierId -> TVar FocusNotifierList -> IO Bool
focusNotifierIsFocus frameId focusNotifierList = do
  focusNotifier <- focusNotifierGetWithId frameId focusNotifierList
  return $ case focusNotifier of
             Just fn -> focusNotifierFocus fn 
             Nothing -> False

-- | Hide focus notifier.
focusNotifierHide :: TVar FocusNotifierList -> IO ()
focusNotifierHide focusNotifierList = 
  modifyTVarIOM focusNotifierList $ \list@(FocusNotifierList sfSet focusId) -> 
      case focusId of
        Just fi -> do 
          -- Get match one.
          match <- focusNotifierGetWithId fi focusNotifierList
          case match of
            -- Hide focus highlight frame.
            Just m@(FocusNotifier sfId widget _) -> do
              -- Redraw widget to hide highlight frame.
              focusNotifierErase widget
              -- Update list.
              let newSet = Set.insert (FocusNotifier sfId widget False) $ Set.delete m sfSet
              return (FocusNotifierList newSet Nothing)
            Nothing -> return $ FocusNotifierList sfSet Nothing
        Nothing -> return list

-- | Show focus notifier.
focusNotifierShow :: FocusNotifierId -> TVar FocusNotifierList -> IO ()
focusNotifierShow frameId focusNotifierList = do
  let showFun = 
        modifyTVarIOM focusNotifierList $ \list@(FocusNotifierList sfSet _) -> do
            match <- focusNotifierGetWithId frameId focusNotifierList
            case match of
              Just m@(FocusNotifier sfId widget _) -> do
                -- Redraw widget to show highlight frame.
                focusNotifierErase widget
                -- Update list.
                let newSet = Set.insert (FocusNotifier sfId widget True) $ Set.delete m sfSet
                return (FocusNotifierList newSet (Just sfId))
              Nothing -> return list

  (FocusNotifierList _ focusId) <- readTVarIO focusNotifierList
  -- Just redraw when focus widget is change.
  case focusId of
    Just fi -> unless (frameId == fi) $ do
                 -- Hide old one.
                 focusNotifierHide focusNotifierList
                 -- Focus new one.
                 showFun
    Nothing -> showFun

-- | Draw highlight frame around widget.
focusNotifierDraw :: Double -> Double -> Double -> Double -> Double -> Render ()   
focusNotifierDraw x y w h lw = do
  -- Set style.
  let (rv, gv, bv) = colorToRGB focusNotifierColor
  setSourceRGBA rv gv bv focusNotifierAlpha
  setLineWidth lw

  -- Draw highlight frame smaller than widget, 
  -- otherwise, widget's redraw can't hide highlight frame.
  roundRectangle (x + 1) (y + 1) (w - 2) (h - 2) pi
  stroke

-- | Get notifier with id. 
focusNotifierGetWithId :: FocusNotifierId -> TVar FocusNotifierList -> IO (Maybe FocusNotifier) 
focusNotifierGetWithId frameId focusNotifierList = do
  (FocusNotifierList sfSet _) <- readTVarIO focusNotifierList
  return $ maybeFindMin sfSet (\fn -> focusNotifierId fn == frameId)

-- | Erase highlight frame around widget.
focusNotifierErase :: WidgetClass self => self -> IO ()
focusNotifierErase widget = do
  (Rectangle x y w h) <- widgetGetAllocation widget
  --`widgetRedrawRectangleFrame` just redraw special rectangle
  -- is more efficient than `widgetQueueDraw` (redraw entire area).
  widgetRedrawRectangleFrame widget 
                             x y w h                 -- bigger than highlight frame area
                             (focusNotifierSize + 1) -- make sure erase highlight frame
