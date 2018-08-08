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

module Manatee.UI.Window where

import Control.Applicative hiding (empty)
import Control.Concurrent.STM 
import Control.Monad.State
import Data.List
import Data.Maybe
import Graphics.UI.Gtk hiding (Window, windowNew, get)
import Manatee.Types
import Manatee.UI.FocusNotifier
import Manatee.UI.WindowNode
import Manatee.Toolkit.Data.ListZipper hiding (length, delete, get)
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.State
import Manatee.Toolkit.Gtk.Container
import Manatee.Toolkit.Gtk.Gtk

import qualified Manatee.Toolkit.Data.ListZipper as LZ

-- | Create new window.
windowNew :: WindowNodeType -> WindowNodeDirection -> Maybe WindowNode 
          -> Container 
          -> TVar FocusNotifierList 
          -> WindowListTuple 
          -> IO (Window, WindowListTuple)
windowNew vnType direction parentNode container focusNotifierList (windowList, windowNodeList) = do
  -- New window node.
  (node, newWindowNodeList) <- windowNodeNew parentNode vnType direction (windowNodeList, container)

  -- New window.
  window <- windowNewWithNode node focusNotifierList

  -- Add window to window list.
  vnType <- windowNodeGetType node
  let newWindowList = windowListAddWindow windowList window vnType

  return (window, (newWindowList, newWindowNodeList))

-- | New window with give node.
windowNewWithNode :: WindowNode -> TVar FocusNotifierList -> IO Window
windowNewWithNode node focusNotifierList = do
  -- New window.
  notebook <- notebookNew
  let window = Window node notebook

  -- Set notebook attributes.
  set notebook [notebookScrollable := True] -- enable scrolling arrow when too many tabs fit the notebook

  -- Add notify frame.
  notifierFrame <- frameNewWithShadowType Nothing
  windowGetContainer window `containerAdd` notifierFrame
  notifierFrame `containerAdd` notebook
  focusNotifierNew (windowGetId window) notifierFrame focusNotifierList

  -- Show current window.
  widgetShowAll (windowNodePaned node)

  return window

-- | New root window.
windowRootNew :: WindowArgs -> TVar FocusNotifierList -> IO (Window, WindowListTuple)
windowRootNew (windowList, windowNodeList, container) focusNotifierList = 
  runStateT_ (windowList, windowNodeList) $ do
  -- Create new window.
  window <- modifyM_ (windowNew TNodeRoot DVertical Nothing container focusNotifierList) snd fst

  -- Focus window.
  modifyFst (\(wList, _) -> windowListFocus wList window)

  return window

-- | Init window.
windowInit :: WindowArgs -> TVar FocusNotifierList -> IO (WindowList, WindowNodeList)
windowInit (windowList, windowNodeList, container) focusNotifierList = 
    runStateT' (windowList, windowNodeList) $ 
    -- Just create first window when window list is empty.
    when (isEmpty windowList) $  
         modifyM (\(wList, nList) -> do
                    -- Remove all children from container.
                    containerRemoveAll container

                    -- Build root window and add to container.
                    snd <$> windowRootNew (wList, nList, container) focusNotifierList)

-- | Split window with indicate direction.
windowSplitInternal :: WindowNodeDirection 
                    -> Window 
                    -> TVar WindowList 
                    -> TVar WindowNodeList 
                    -> TVar FocusNotifierList 
                    -> Container 
                    -> IO (Window, Window) 
windowSplitInternal direction windowParent windowList windowNodeList focusNotifierList container = do
  -- Get old value.
  oldWindowList <- readTVarIO windowList
  oldWindowNodeList <- readTVarIO windowNodeList

  -- Split.
  ((wc1, wc2), (newWindowList, newWindowNodeList)) <- runStateT_ (oldWindowList, oldWindowNodeList) $ do
    -- Get split node.
    parentNode <- modifyM_ (\(wList, nList) -> do
                              (pNode, nnList) <- windowNodeGetSplitContainer 
                                                (windowNode windowParent) 
                                                direction 
                                                (nList, container)
                              return (pNode, (wList, nnList))) snd fst

    -- Create window children.
    windowChild1 <- modifyM_ (windowNew TNodeLeft direction (Just parentNode) container focusNotifierList) snd fst
    windowChild2 <- modifyM_ (windowNew TNodeRight direction (Just parentNode) container focusNotifierList) snd fst
    
    -- Remove old window from window list.
    modifyFst (\(wList, _) -> windowListRemoveWindow windowParent wList)
    
    -- Show all widgets, this step is NECESSARY!
    lift $ widgetShowAll (windowNodePaned parentNode)

    -- Set size request for window child.
    nodeList <- gets snd
    lift $ 
      -- Get size of toplevel container.
      containerApplySize container $ \width height -> do
          windowNodeSetSizeRequest (windowNode windowChild1) nodeList (width, height)
          windowNodeSetSizeRequest (windowNode windowChild2) nodeList (width, height)
    
    -- Return child windows.
    return (windowChild1, windowChild2)

  -- Update new value.
  writeTVarIO windowList newWindowList
  writeTVarIO windowNodeList newWindowNodeList

  return (wc1, wc2)

-- | Remove window.
windowRemoveInternal :: (Window, (Container, (WindowList, WindowNodeList))) -> IO WindowListTuple
windowRemoveInternal (window, (container, (windowList, windowNodeList))) = 
    runStateT' (windowList, windowNodeList) $ 
      case windowListGetSize windowList of
        -- Haven't any window exist. 
        0 -> lift $ putStrLn "Haven't any window exist."
        -- Otherwise remove window.
        _ -> do
          -- Remove window node.
          modifySndM (\(_, nList) -> 
                       windowNodeRemove (windowNode window) (nList, container) True)

          -- Remove from windowList.
          modifyFst (\(wList, _) -> windowListRemoveWindow window wList)

-- | Remove others windows except current window.
windowRemoveOthers :: Window -> WindowArgs -> IO WindowListTuple
windowRemoveOthers window (windowList, windowNodeList, container) =  
  runStateT' (windowList, windowNodeList) $ 
  case windowListGetSize windowList of
    -- Don't delete window when haven't or just only one window exist.
    0 -> lift $ putStrLn "Haven't any window exist."
    1 -> lift $ putStrLn "Haven't others window exist."
    -- Otherwise delete others window except current one.
    _ -> do
      -- Remove other window nodes.
      modifySndM (\(_, nList) -> windowNodeRemoveOthers (windowNode window) (nList, container))

      -- Remove other window from window list.
      modifyFst (\(wList, _) -> windowListRemoveOthersWindow wList)

      -- Focus window.
      modifyFst (\(wList, _) -> windowListFocus wList window)

-- | Get top window container.
windowGetContainer :: Window -> Paned
windowGetContainer = windowNodePaned . windowNode

-- | Get window id.
windowGetId :: Window -> WindowId
windowGetId = windowNodeId . windowNode

-- | Create window list.
windowListNew :: WindowList
windowListNew = empty

-- | Add window to window list.
windowListAddWindow :: WindowList -> Window -> WindowNodeType -> WindowList
windowListAddWindow windowList window TNodeLeft = insertLeft window windowList
windowListAddWindow windowList window _         = insertRight window windowList

-- | Remove others window except current window from window list.
windowListRemoveOthersWindow :: WindowList -> WindowList
windowListRemoveOthersWindow windowList = 
    fromMaybe windowList (deleteOthers windowList)

-- | Get current focus window.
windowListGetFocusWindow :: TVar WindowList -> TVar WindowNodeList -> TVar FocusNotifierList -> Container -> IO (Maybe Window) 
windowListGetFocusWindow windowList windowNodeList focusNotifierList container = do
  -- Get old value.
  wList <- readTVarIO windowList
  wnList <- readTVarIO windowNodeList

  -- When window list is empty, create root window.
  (newWindowList, newWindowNodeList) <- windowInit (wList, wnList, container) focusNotifierList

  -- Update window list.
  writeTVarIO windowList newWindowList
  writeTVarIO windowNodeList newWindowNodeList

  -- Return current window.
  return $ getCurrent newWindowList

-- | Ge window with special id.
windowListGetWindow :: WindowId -> WindowList -> Maybe Window
windowListGetWindow id windowList = 
  find (\v -> windowNodeId (windowNode v) == id) (windowListGetList windowList)

-- | Apply window with special id.
windowListApplyWithId :: WindowList -> WindowId -> (Window -> IO ()) -> IO ()
windowListApplyWithId windowList id f =
    windowListGetWindow id windowList ?>= f

-- | Get list.
windowListGetList :: WindowList -> [Window]
windowListGetList = toList

-- | Show window list.
windowListShow ::WindowList -> IO ()
windowListShow = print . show . windowListGetList

-- | Get window list size.
windowListGetSize :: WindowList -> Int
windowListGetSize = LZ.length

-- | Apply window.
windowListApplyWindow :: (Window -> IO ()) -> WindowList -> IO ()
windowListApplyWindow f windowList = mapM_ f $ windowListGetList windowList

-- | Select window with given direction.
windowListSelect :: WindowListSelectDirection -> Bool -> WindowList -> WindowList
windowListSelect direction circular windowList = newWindowList
  -- Get next focus function.
  where selectFunction = 
            case direction of
              VLeft  -> if circular
                          then getLeftCircular
                          else getLeft
              VRight -> if circular
                          then getRightCircular
                          else getRight 
        -- Move next focus if have it.
        newWindowList = case selectFunction windowList of
                          Just x  -> windowListFocus windowList x
                          Nothing -> windowList

-- | Next window in window list.
windowListNext :: WindowList -> WindowList
windowListNext = windowListSelect VRight False

-- | Previous window in window list.
windowListPrev :: WindowList -> WindowList
windowListPrev = windowListSelect VLeft False

-- | Next window circular in window list.
windowListNextCircular :: WindowList -> WindowList
windowListNextCircular = windowListSelect VRight True

-- | Previous window circular in window list.
windowListPrevCircular :: WindowList -> WindowList
windowListPrevCircular = windowListSelect VLeft True

-- | Focus window.
windowListFocus :: WindowList -> Window -> WindowList
windowListFocus windowList window = 
    fromMaybe windowList (focusNode window windowList)

-- | Focus window id.
windowListFocusId :: WindowList -> WindowId -> WindowList
windowListFocusId windowList windowId = 
  case windowListGetWindow windowId windowList of
    Just w  -> windowListFocus windowList w
    Nothing -> windowList

-- | Remove current window from window list.
windowListRemoveCurrentWindow :: WindowList -> WindowList
windowListRemoveCurrentWindow windowList = newWindowList
  where 
        -- Get next focus.
        nextFocus = getRightCircular windowList
        -- Remove from window list.
        wList = fromMaybe windowList (LZ.delete windowList)
        -- Get new window list.
        newWindowList = case nextFocus of
                          Just x  -> windowListFocus wList x
                          Nothing -> wList

-- | Remove window from window list.
windowListRemoveWindow :: Window -> WindowList -> WindowList
windowListRemoveWindow window windowList = do
  let newWindowList = fromMaybe windowList $ LZ.deleteNode window windowList

  -- We need make sure focus some window 
  -- if have any window exist in window list.
  case LZ.getCurrent newWindowList of
    Just _  -> newWindowList
    Nothing -> 
        case LZ.getRightCircular newWindowList of
          Just x  -> windowListFocus newWindowList x
          Nothing -> newWindowList
    
      
