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

{-# LANGUAGE OverloadedStrings #-}
module Manatee.Toolkit.Gtk.Event where

import Control.Applicative hiding (empty)
import Control.Monad.Trans
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text.Lazy (Text, pack, singleton, empty, append, snoc)
import Graphics.UI.Gtk

import qualified Data.Map as M
import qualified Data.Text.Lazy as DTL

-- | Get keystroke value as a text.
eventKeystoke :: EventM EKey Text
eventKeystoke = do
  -- liftIO $ putStrLn "Debug test" 

  -- Get key modifier and name.
  keyModifier <- eventModifier
  keyName     <- eventKeyName'

  -- Debug.
  -- liftIO $ do
  --   putStrLn $ "Key modifier: " ++  show keyModifier
  --   putStrLn $ "Key name: " ++ show keyName

  case M.lookup keyName eventModifierMap of
    -- When user just press modifier keys.
    Just m  -> do
      -- Return all modifier alias list.
      let modifierList = (:) m keyModifier
      return $ eventModifierAliasList modifierList
    -- Otherwise.
    Nothing -> do
      -- Remove `Shift` modifier from modifier list if key name is corresponding Unicode character.
      removeShift <- isUnicodeKey
      let modifierList = (if removeShift
                          then filter (/= Shift)
                          else id) keyModifier
                     
      -- Get key name.
      shortName <- eventKeyName_

      -- Return result.
      return $ eventModifierAliasList modifierList `append` shortName

-- | Get key char.
eventKeyChar :: EventM EKey (Maybe Char)
eventKeyChar = keyToChar <$> eventKeyVal

-- | Like `eventKeyName` just return Text.
eventKeyName' :: EventM EKey Text
eventKeyName' = fmap (pack . eventKeyNameAlias) eventKeyName

-- | Like `eventKeyName` return key name, 
-- but return symbol when key is corresponding Unicode character.
eventKeyName_ :: EventM EKey Text
eventKeyName_ = do
  -- Get event char.
  keyChar <- eventKeyChar

  case keyChar of
    -- Return character symbol when key is corresponding Unicode character.
    Just c  -> return $ singleton c
    -- Otherwise return key name.
    Nothing -> eventKeyName'

-- | Modifier alias.
eventModifierAlias :: Modifier -> Text
eventModifierAlias = singleton . aliasMatch
    where
      aliasMatch Control = 'C'
      aliasMatch Shift   = 'S'
      aliasMatch Alt     = 'M'
      aliasMatch Meta    = 'M'
      aliasMatch Super   = 'P'

-- | Event key name alias
eventKeyNameAlias :: String -> String
eventKeyNameAlias "Page_Down" = "PageDown"
eventKeyNameAlias "Page_Up"   = "PageUp"
eventKeyNameAlias key         = key

-- | Concat modifier list as alias. 
eventModifierAliasList :: [Modifier] -> Text
eventModifierAliasList []   = empty
eventModifierAliasList list = DTL.concat $ map (`snoc` '-') (nub $ sort $ map eventModifierAlias list)

-- | Modifier name map.
eventModifierMap :: Map Text Modifier
eventModifierMap = 
    M.fromList
         [("Control_L",        Control)
         ,("Control_R",        Control)
         ,("Shift_L",          Shift)
         ,("Shift_R",          Shift)
         ,("Alt_L",            Alt)
         ,("Alt_R",            Alt)
         ,("Meta_L",           Alt)
         ,("Meta_R",           Alt)
         ,("Super_L",          Super)
         ,("Super_R",          Super)]

-- | Whether key is Unicode character.
isUnicodeKey :: EventM EKey Bool
isUnicodeKey = isJust <$> eventKeyChar

-- | Get size of event window.
eventWindowSize :: EventM EExpose (Double, Double)      
eventWindowSize = do
    dr    <- eventWindow
    (w,h) <- liftIO $ drawableGetSize dr
    return $ if w * h > 1
               then (fromIntegral w, fromIntegral h)
               else (1,1)
