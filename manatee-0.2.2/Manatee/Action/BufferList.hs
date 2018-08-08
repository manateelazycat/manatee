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

module Manatee.Action.BufferList where

import Data.Sequence (Seq)
import Manatee.Core.Types
import Manatee.Toolkit.Data.Unique
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Seq
import Manatee.Toolkit.General.String
import System.Posix.Types (ProcessID)

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Sequence as Seq

-- | Replace path.
bufferListReplacePath :: PageModeName -> PageId -> String -> BufferList -> BufferList
bufferListReplacePath pageModeName pageId newPath bl@(BufferList bufferMap) = 
    case bufferListGetBufferInfo pageModeName pageId bl of
      Nothing -> bl
      Just (oldBuffer, (bi, bufferSeq)) -> 
          let newBuffer = oldBuffer {bufferPath = newPath}
              newBufferSeq = Seq.update bi newBuffer bufferSeq
          in BufferList $ M.insert pageModeName newBufferSeq bufferMap

-- | Replace tab name. 
bufferListReplaceName :: PageModeName -> PageId -> String -> BufferList -> BufferList
bufferListReplaceName pageModeName pageId newTabName bl@(BufferList bufferMap) = 
    case bufferListGetBufferInfo pageModeName pageId bl of
      Nothing -> bl
      Just (oldBuffer, (bi, bufferSeq)) -> 
          let newBuffer = oldBuffer {bufferName = newTabName}
              newBufferSeq = Seq.update bi newBuffer bufferSeq
          in BufferList $ M.insert pageModeName newBufferSeq bufferMap

-- | Strip tab name with special length.
bufferListStripName :: PageModeName -> PageId -> String -> BufferList -> BufferList
bufferListStripName pageModeName pageId newPath bl@(BufferList bufferMap) = 
    case bufferListGetBufferInfo pageModeName pageId bl of
      Nothing -> bl
      Just (oldBuffer, (bi, bufferSeq)) -> 
          let newBuffer = oldBuffer {bufferName = stripFormat newPath 25}
              newBufferSeq = Seq.update bi newBuffer bufferSeq
          in BufferList $ M.insert pageModeName newBufferSeq bufferMap

-- | Unique buffer name.
bufferListUniqueName :: PageModeName -> BufferList -> BufferList 
bufferListUniqueName modeName (BufferList bufferMap) = 
  let matchBuffer = findMinMatch bufferMap (\ mName _ -> mName == modeName)
  in BufferList $
     case matchBuffer of
       Nothing -> bufferMap
       Just (name, bufferSeq) ->                            
           let buffers = F.toList bufferSeq
               bufferPathList = map bufferPath buffers
               newBuffers = map (\ (buffer, newName) -> 
                                     buffer {bufferName = newName}
                                ) (zip buffers (map (uncurry formatBufferName) $ unique bufferPathList))
           in M.insert name (Seq.fromList newBuffers) bufferMap

-- | Remove buffer.
bufferListRemoveBuffer :: PageModeName -> Int -> BufferList -> BufferList          
bufferListRemoveBuffer modeName index (BufferList bufferMap) =
    BufferList $ M.filter (not . Seq.null) -- remove item from BufferList if 'Seq Buffer' is empty
               $ M.mapWithKey 
                   (\ pageModeName buffers ->
                        if pageModeName == modeName
                           -- Delete buffer from 'Seq Buffer' when find match
                           then deleteAt index buffers
                           -- Otherwise keep unchange.
                           else buffers 
                   ) bufferMap

-- | Swap buffer.
bufferListSwapBuffer :: PageModeName -> Int -> Int -> BufferList -> BufferList
bufferListSwapBuffer modeName currentIndex targetIndex (BufferList bufferMap) = 
    BufferList $ M.mapWithKey 
                   (\ pageModeName buffers ->
                        if pageModeName == modeName
                           then swap currentIndex targetIndex buffers
                           else buffers 
                   ) bufferMap

-- | Add buffer.
bufferListAddBuffer :: (PageModeName, ProcessID, PageId, PageType, String) -> BufferList -> BufferList
bufferListAddBuffer (pageModeName, processId, pageId, pType, path) (BufferList bufferMap) = 
  let matchBuffer = findMinMatch bufferMap (\ modeName _ -> modeName == pageModeName) 
      newBuffer = Buffer processId pageId pType path ""
      sequence  = case matchBuffer of
                    -- Add new tab if haven't same mode found.
                    Nothing -> Seq.singleton newBuffer
                    -- Replace or add new tab.
                    Just (_, seq) -> replaceOrAdd (\x -> bufferPageId x == pageId) newBuffer seq
  in BufferList $ M.insert pageModeName sequence bufferMap

-- | Is buffer exist.
bufferListGetBufferIndex :: BufferList -> PageModeName -> FilePath -> Maybe Int
bufferListGetBufferIndex (BufferList bufferMap) pageModeName path = index
  where matchBuffer = findMinMatch bufferMap (\ modeName _ -> modeName == pageModeName) 
        index = case matchBuffer of
                  Just (_, bufferSeq) -> Seq.findIndexL (\x -> bufferPath x == path) bufferSeq
                  Nothing -> Nothing

-- | Is buffer exist.
bufferListGetBufferIndexWithId :: BufferList -> PageModeName -> PageId -> Maybe Int
bufferListGetBufferIndexWithId (BufferList bufferMap) pageModeName pId = index
  where matchBuffer = findMinMatch bufferMap (\ modeName _ -> modeName == pageModeName) 
        index = case matchBuffer of
                  Just (_, bufferSeq) -> Seq.findIndexL (\x -> bufferPageId x == pId) bufferSeq
                  Nothing -> Nothing

-- | Find match buffer.
bufferListGetBufferInfo :: PageModeName -> PageId -> BufferList -> Maybe (Buffer, (Int, Seq Buffer))
bufferListGetBufferInfo pageModeName pageId (BufferList bufferMap) = 
    let matchBuffer = findMinMatch bufferMap (\ modeName _ -> modeName == pageModeName)
    in case matchBuffer of
         Nothing -> Nothing
         Just (_, bufferSeq) ->  
             fmap (\bi -> 
                       let oldBuffer = Seq.index bufferSeq bi 
                       in (oldBuffer, (bi, bufferSeq))                  
                  ) $ Seq.findIndexL (\x -> bufferPageId x == pageId) bufferSeq

-- | Get buffer with given mode name and page id.
bufferListGetBuffer :: BufferList -> PageModeName -> PageId -> Maybe Buffer
bufferListGetBuffer bl pageModeName pId = 
    fmap fst (bufferListGetBufferInfo pageModeName pId bl)

-- | Format unique name.
formatBufferName :: FilePath -> FilePath -> FilePath
formatBufferName file dir
    | null dir  = file
    | otherwise = file ++ "<" ++ dir ++ ">"

-- | Return 'Ture' if have any buffer exist,   
-- Otherwise return 'False'
bufferListHaveBufferExist :: BufferList -> Bool
bufferListHaveBufferExist (BufferList bufferMap) =
    not $ M.null bufferMap

