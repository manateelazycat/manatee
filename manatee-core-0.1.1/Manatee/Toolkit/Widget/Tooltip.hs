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

module Manatee.Toolkit.Widget.Tooltip where

import Control.Applicative
import Control.Monad.Trans
import Data.Maybe
import Graphics.UI.Gtk hiding (Tooltip)
import Manatee.Toolkit.Gtk.Gtk

data Tooltip =
    Tooltip {tooltipWindow           :: Window -- window for contain widget
            ,tooltipDependWindow     :: Window -- depend window for control display
            ,tooltipLabel            :: Label  -- label to container text
            ,tooltipTimeout          :: Int    -- timeout for display (in millisecond)
            ,tooltipFocusInConnectId :: ConnectId Window
            ,tooltipFocusOutConnectId:: ConnectId Window
            }

-- | Align size.
tooltipAlignSize :: Int
tooltipAlignSize = 10

-- | Default tooltip timeout.
tooltipDefaultTimeout :: Int
tooltipDefaultTimeout = 3000

-- | Default foreground color.
tooltipDefaultForegroundColor :: Color
tooltipDefaultForegroundColor = Color 0 0 0

-- | Default background color.
tooltipDefaultBackgroundColor :: Color
tooltipDefaultBackgroundColor = Color 65335 65335 0

-- | New tooltip.
tooltipNew :: WidgetClass widget 
           => widget -- ^ top-level parent window
           -> String             -- ^ text 
           -> Maybe Point        -- ^ point, 'Nothing' will set right-top corner of screen
           -> Maybe Int          -- ^ timeout
           -> Maybe Color        -- ^ foreground color
           -> Maybe Color        -- ^ background color
           -> IO Tooltip
tooltipNew widget text position time fColor bColor = do
  -- Get window.
  win <- castToWindow <$> widgetGetToplevel widget
  let timeout = case time of
                  Just t  -> t
                  Nothing -> tooltipDefaultTimeout

  -- Set color.
  let foregroundColor = fromMaybe tooltipDefaultForegroundColor fColor
      backgroundColor = fromMaybe tooltipDefaultBackgroundColor bColor

  -- Create tooltip window.
  window <- windowNewPopup
  windowSetDecorated window False
  windowSetOpacity window 0.8   -- this function need window-manager support Alpha channel in X11
  widgetModifyBg window StateNormal backgroundColor

  -- Create frame.
  frame <- frameNew
  set frame [frameShadowType := ShadowEtchedIn]

  -- Create alignment.
  alignment <- alignmentNew 0.5 0.5 1 1
  alignmentSetPadding alignment tooltipAlignSize tooltipAlignSize tooltipAlignSize tooltipAlignSize

  -- Create label.
  label <- labelNew $ Just text
  labelSetAttributes label [AttrForeground 0 (length text) foregroundColor
                           ,AttrBackground 0 (length text) backgroundColor]

  -- Wrap line with word bound.
  labelSetLineWrap label True
  labelSetLineWrapMode label WrapPartialWords

  -- Show tooltip.
  alignment `containerAdd` label
  frame     `containerAdd` alignment
  window    `containerAdd` frame
  widgetShowAll window

  -- Adjust tooltip position.
  (screenWidth, screenHeight) <- widgetGetScreenSize win
  (Rectangle _ _ width height) <- widgetGetAllocation window
  let (tooltipX, tooltipY) =  
        case position of
          Just (x, y) -> 
            let adjustX 
                 -- Set screen right when x bigger than screen width.
                 | x > screenWidth 
                     = screenWidth - width - tooltipAlignSize
                 -- Set left of x when tooltip width bigger than screen width.
                 | x + width > screenWidth
                     = x - width - tooltipAlignSize
                 | otherwise
                     = x
                adjustY 
                 -- Set screen bottom when y bigger than screen height.
                 | y > screenHeight
                     = screenHeight - height - tooltipAlignSize
                 -- Set up of y when tooltip height bigger than screen height.
                 | y + height > screenHeight
                     = y - height - tooltipAlignSize
                 | otherwise
                     = y
            in (adjustX, adjustY)
          Nothing -> (screenWidth - width - tooltipAlignSize, tooltipAlignSize)

  -- Move window.
  windowMove window tooltipX tooltipY

  -- Show tooltip when parent window show.
  focusInConnectId <- 
    win `on` focusInEvent $ tryEvent $ do
        liftIO $ do
          widgetShowAll window
          windowMove window tooltipX tooltipY
        stopEvent

  -- Hide tooltip when parent window hide.
  focusOutConnectId <- 
    win `on` focusOutEvent $ tryEvent $ do
        liftIO $ widgetHideAll window
        stopEvent

  -- Create tooltip.
  let tooltip = Tooltip window win label timeout focusInConnectId focusOutConnectId

  -- Destroy tooltip when out of timeout.
  timeoutAdd (tooltipExit tooltip >> return False) timeout

  return tooltip

-- | Exit tooltip.
tooltipExit :: Tooltip -> IO ()
tooltipExit Tooltip {tooltipWindow              = window
                    ,tooltipFocusInConnectId    = focusInConnectId
                    ,tooltipFocusOutConnectId   = focusOutConnectId
                    } 
            = do
  -- Disconnect signal first.
  signalDisconnect focusInConnectId
  signalDisconnect focusOutConnectId

  -- Destroy tooltip.
  widgetDestroy window
