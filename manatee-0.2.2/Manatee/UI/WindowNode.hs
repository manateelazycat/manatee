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

module Manatee.UI.WindowNode where

import Control.Applicative
import Control.Concurrent.STM 
import Control.Monad.State
import Data.Maybe
import Graphics.UI.Gtk hiding (on, get)
import Manatee.Types
import Manatee.Toolkit.Data.SetList
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.General.State
import Manatee.Toolkit.Gtk.Container
import Manatee.Toolkit.Gtk.Gtk
import Text.Printf

-- | The default zoom size.
zoomDefaultSize :: Int 
zoomDefaultSize = 10

-- | Create new window node.
windowNodeNew :: Maybe WindowNode -> WindowNodeType -> WindowNodeDirection -> WindowNodeArgs -> IO (WindowNode, WindowNodeList)
windowNodeNew parentNode nType direction = 
    windowNodeNewInternal (Nothing, maybeApply parentNode windowNodeId, Nothing, Nothing, nType, direction)

-- | Internal function for create window node.  
windowNodeNewInternal :: WindowNodeAttr -> WindowNodeArgs -> IO (WindowNode, WindowNodeList)
windowNodeNewInternal (vId, pId, clId, crId, typ, direction) (windowNodeList, container) = 
  runStateT_ nodeList $ do
    -- Create new window node.
    windowNode <- 
      lift $ WindowNode id
                 <$> windowNodePanedNew direction
                 <*> newTVarIO pId
                 <*> newTVarIO clId
                 <*> newTVarIO crId
                 <*> newTVarIO typ
                 <*> pure direction

    -- Connect current node parent.
    lift $ windowNodeConnectToParent windowNode (nodeList, container)

    -- Add to window node list.
    modify (`setListAddNode` windowNode)

    return windowNode
  -- Init value.
  where (id, nodeList) = 
          case vId of
            Just vi -> (vi, windowNodeList)
            Nothing -> setListGetNewCounter windowNodeList

-- | Remove window node. 
windowNodeRemove :: WindowNode -> WindowNodeArgs -> Bool -> IO WindowNodeList
windowNodeRemove windowNode (windowNodeList, container) removeOtioseNode = 
  runStateT' windowNodeList $ do
    -- Get type.
    vType <- lift $ windowNodeGetType windowNode

    -- Remove window node from window node list first.
    modify (`setListRemoveNode` windowNode)
    
    -- Remove window node paned.
    getM (\x -> windowNodePanedRemove windowNode (x, container))
    
    -- Clean otiose nodes.
    unless (vType == TNodeRoot) $ do
        parentNode <- getM (windowNodeGetParentNode windowNode)
        case parentNode of
          Just n -> do
            -- Remove current node from parent node.
            lift $ windowNodeRemoveFromParentNode windowNode n
    
            -- Remove otiose node.
            when removeOtioseNode $ do
                -- Is have child node?
                haveChildNode <- lift $ windowNodeIsHaveChildNode n
    
                -- Clean otiose node when haven't children node under current node.
                unless haveChildNode $ modifyM (\x -> windowNodeRemove n (x, container) True)

-- | Remove others window nodes except current window node.
windowNodeRemoveOthers :: WindowNode -> WindowNodeArgs -> IO WindowNodeList
windowNodeRemoveOthers windowNode (windowNodeList, container) = do
  -- Change window node to root window node.
  windowNodeChangeToRoot windowNode (windowNodeList, container)
  containerRemoveAll container                          -- remove all child from root container
  windowNodeConnectToRootContainer windowNode container -- add window node to root container

  -- Remove others window node from window node list.
  return $ setListRemoveOthersNode windowNodeList windowNode

-- | Get parent node of current node.
windowNodeGetParentNode :: WindowNode -> WindowNodeList -> IO (Maybe WindowNode)
windowNodeGetParentNode windowNode windowNodeList = 
    readTVarIO (windowNodeParentId windowNode) >>= 
                   (?>=> (return . windowNodeListGetNode windowNodeList))

-- | Get split container for child window node.
windowNodeGetSplitContainer :: WindowNode -> WindowNodeDirection -> WindowNodeArgs -> IO (WindowNode, WindowNodeList)
windowNodeGetSplitContainer windowNode direction (windowNodeList, container) = 
    runStateT_ windowNodeList $ 
    if windowNodeDirection windowNode == direction
       -- Return current window node if child node is same split direction.
       then do
         lift $ containerRemoveAll $ windowNodePaned windowNode
         return windowNode
       -- Otherwise, create new container node from parent node of current node.
       else do
         -- Remove current node first.
         modifyM (\x -> windowNodeRemove windowNode (x, container) False)

         -- Create new window node from parent node.
         parentNode <- getM (windowNodeGetParentNode windowNode)
         vType      <- lift $ windowNodeGetType windowNode
         modifyM_ (\x -> windowNodeNew parentNode vType direction (x, container)) snd fst
    
-- | Change window node to root node.
windowNodeChangeToRoot :: WindowNode -> WindowNodeArgs -> IO ()
windowNodeChangeToRoot windowNode (windowNodeList, container) = do
  -- Remove window paned from parent container.
  windowNodePanedRemove windowNode (windowNodeList, container)

  -- Change window node attribute.
  writeTVarIO (windowNodeParentId windowNode) Nothing
  writeTVarIO (windowNodeChildLeftId windowNode) Nothing
  writeTVarIO (windowNodeChildRightId windowNode) Nothing
  writeTVarIO (windowNodeType windowNode) TNodeRoot 

-- | Add window node paned to parent container.
windowNodeConnectToParent :: WindowNode -> WindowNodeArgs -> IO ()
windowNodeConnectToParent windowNode (windowNodeList, container) = do
  vType <- windowNodeGetType windowNode
  case vType of
    -- Add window node to root container if it is root node.
    TNodeRoot  -> windowNodeConnectToRootContainer windowNode container
    TNodeLeft  -> windowNodeConnectToParentNodeLeft windowNode windowNodeList
    TNodeRight -> windowNodeConnectToParentNodeRight windowNode windowNodeList

-- | Add window node to root container.
windowNodeConnectToRootContainer :: ContainerClass container => WindowNode -> container -> IO ()    
windowNodeConnectToRootContainer windowNode  =
  (`containerAdd` windowNodePaned windowNode)

-- | Add window node to parent node.
windowNodeConnectToParentNodeLeft :: WindowNode -> WindowNodeList -> IO ()
windowNodeConnectToParentNodeLeft windowNode windowNodeList =
    windowNodeGetParentNode windowNode windowNodeList >>=
    (?>= (\n -> do
            -- Add current node to other window node.
            let vParentPaned = windowNodePaned n
                vPaned = windowNodePaned windowNode
                id = windowNodeId windowNode
            panedPack1 vParentPaned vPaned True True
            writeTVarIO (windowNodeChildLeftId n) (Just id)))

-- | Add window node to parent node right.
windowNodeConnectToParentNodeRight :: WindowNode -> WindowNodeList -> IO ()
windowNodeConnectToParentNodeRight windowNode windowNodeList =
  windowNodeGetParentNode windowNode windowNodeList >>=
  (?>= (\n -> do
          -- Add current node to other window node.
          let vParentPaned = windowNodePaned n
              vPaned = windowNodePaned windowNode
              id = windowNodeId windowNode
          panedPack2 vParentPaned vPaned True True
          writeTVarIO (windowNodeChildRightId n) (Just id)
       ))

-- | Get window node paned with indicate split direction.
windowNodePanedNew :: WindowNodeDirection -> IO Paned
windowNodePanedNew DVertical 
    = castToPaned <$> vPanedNew
windowNodePanedNew _ 
    = castToPaned <$> hPanedNew

-- | Remove node paned from parent container.
windowNodePanedRemove :: WindowNode -> WindowNodeArgs -> IO ()
windowNodePanedRemove windowNode (windowNodeList, container) = do
  let vPaned = windowNodePaned windowNode
  vType <- windowNodeGetType windowNode

  case vType of
    -- Just remove from root container if current node is root node.
    TNodeRoot -> container `containerRemove` vPaned
    -- Otherwise, remove from parent window node.
    _ -> 
        -- Remove paned from parent node paned.
        windowNodeGetParentNode windowNode windowNodeList >>=
          (?>= (\n -> windowNodePaned n `containerRemove` vPaned))

-- | Is haven't any child node.
windowNodeIsHaveChildNode :: WindowNode -> IO Bool
windowNodeIsHaveChildNode windowNode = do
  childLeftId  <- windowNodeGetChildLeftId windowNode
  childRightId <- windowNodeGetChildRightId windowNode
  
  return $ isJust childLeftId || isJust childRightId

-- | Remove from parent node.
windowNodeRemoveFromParentNode :: WindowNode -> WindowNode -> IO ()  
windowNodeRemoveFromParentNode windowNode n = do
  vnType <- windowNodeGetType windowNode
  case vnType of
    TNodeLeft  -> writeTVarIO (windowNodeChildLeftId n) Nothing
    TNodeRight -> writeTVarIO (windowNodeChildRightId n) Nothing
    _          -> return ()

-- | Get type.
windowNodeGetType :: WindowNode -> IO WindowNodeType
windowNodeGetType = 
  readTVarIO . windowNodeType 

-- | Get parent id.
windowNodeGetParentId :: WindowNode -> IO (Maybe WindowNodeId)
windowNodeGetParentId =                      
  readTVarIO . windowNodeParentId
  
-- | Get child left id.  
windowNodeGetChildLeftId :: WindowNode -> IO (Maybe WindowNodeId)
windowNodeGetChildLeftId =                         
  readTVarIO . windowNodeChildLeftId
  
-- | Get child right id  
windowNodeGetChildRightId :: WindowNode -> IO (Maybe WindowNodeId)
windowNodeGetChildRightId =
  readTVarIO . windowNodeChildRightId

-- | Request node size.
-- If vertical split times is `vn`, the request height is `height / (2 ^ vn)`.
-- If horizontal split times is `hn`, the request width is `width / (2 ^ hn)`.
windowNodeSetSizeRequest :: WindowNode -> WindowNodeList -> (Int, Int) -> IO ()
windowNodeSetSizeRequest currentNode nodeList (width, height) = do
  -- Get split times.
  (vSplitTimes, hSplitTimes) <- windowNodeGetSplitTimes currentNode nodeList (0, 0)

  -- Compute size along with split times.
  let requestHeight = height `div` (^) 2 vSplitTimes
      requestWidth  = width `div` (^) 2 hSplitTimes

  -- Set size request.
  widgetSetSizeRequest (windowNodePaned currentNode) requestWidth requestHeight

-- | Get split times of current window node.
windowNodeGetSplitTimes :: WindowNode -> WindowNodeList -> (Int, Int) -> IO (Int, Int)  
windowNodeGetSplitTimes windowNode nodeList (vSplit, hSplit) = do
  -- Get parent node.
  parentNode <- windowNodeGetParentNode windowNode nodeList
  case parentNode of
    Just pn -> do
      let direction = windowNodeDirection pn
          splitTimes =
            if direction == DVertical
               -- Tick vertical split times.
               then (vSplit + 1, hSplit)
               -- Tick horizontal split times.
               else (vSplit, hSplit + 1)

      nodeType <- readTVarIO $ windowNodeType pn
      case nodeType of
        -- Return split times if current node is root node.
        TNodeRoot -> return splitTimes
        -- Otherwise, search upper node.
        _ -> windowNodeGetSplitTimes pn nodeList splitTimes
    
-- | Show window node.
windowNodeShow :: WindowNode -> IO ()
windowNodeShow windowNode = do
  let id = windowNodeId windowNode
  parentId <- windowNodeGetParentId windowNode
  leftId   <- windowNodeGetChildLeftId windowNode
  rightId  <- windowNodeGetChildRightId windowNode
  printf "Node (%s) <ParentId: %s, LeftCId: %s, RightCId: %s>\n" (show id) (show parentId) (show leftId) (show rightId)

-- | New window node.
windowNodeListNew :: WindowNodeList
windowNodeListNew = setListNew

-- | Get window node with given id.
windowNodeListGetNode :: WindowNodeList -> WindowNodeId -> Maybe WindowNode
windowNodeListGetNode windowNodeList id = 
    setListGetNode windowNodeList (\y -> windowNodeId y == id)

-- | Show window node list.
windowNodeListShow :: WindowNodeList -> IO ()
windowNodeListShow windowNodeList = 
    mapM_ windowNodeShow $ setListGetList windowNodeList

-- | Get zoom direction of window node.
windowNodeGetZoomDirection :: WindowNode -> WindowNode -> IO ZoomDirection
windowNodeGetZoomDirection node parentNode = do
  nodeType <- readTVarIO $ windowNodeType node
  let nodeDirection = windowNodeDirection parentNode
  return $ if nodeType == TNodeRight
              then if nodeDirection == DVertical then ZDown else ZRight
              else if nodeDirection == DVertical then ZUp else ZLeft

-- | Get match zoom direction.
windowNodeGetMatchZoomDirectionSize :: ZoomDirection -> Bool -> (ZoomDirection, Int)
windowNodeGetMatchZoomDirectionSize zoomDirection isEnlarge
    | zoomDirection == ZUp     = (ZDown,  if isEnlarge then -zoomDefaultSize else zoomDefaultSize)
    | zoomDirection == ZDown   = (ZUp,    if isEnlarge then zoomDefaultSize  else -zoomDefaultSize)
    | zoomDirection == ZLeft   = (ZRight, if isEnlarge then -zoomDefaultSize else zoomDefaultSize)
    | otherwise                = (ZLeft,  if isEnlarge then zoomDefaultSize  else -zoomDefaultSize)
  
-- | Zoom window.
windowNodeZoom :: WindowNodeList -> WindowNode -> ZoomDirection -> Bool -> IO ()
windowNodeZoom nodeList node enlargeDirection isEnlarge = do
  let (matchDirection, adjustSize) = windowNodeGetMatchZoomDirectionSize enlargeDirection isEnlarge
  nodeType <- readTVarIO $ windowNodeType node
  case nodeType of
    -- Don't do anything when reach root node.
    TNodeRoot -> return ()
    _ -> 
      readTVarIO (windowNodeParentId node)
          >?>= \ parentNodeId -> 
              setListGetNode nodeList (\x -> windowNodeId x == parentNodeId) 
                  ?>= \ parentNode -> do
                    direction <- windowNodeGetZoomDirection node parentNode
                    if direction == matchDirection
                       -- Enlarge node when node direction match enlarge direction.
                       then panedAdjustSize (windowNodePaned parentNode) adjustSize
                       -- Otherwise, search parent node to enlarge.
                       else windowNodeZoom nodeList parentNode enlargeDirection isEnlarge
