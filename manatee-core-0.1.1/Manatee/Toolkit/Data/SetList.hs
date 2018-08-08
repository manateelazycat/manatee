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

module Manatee.Toolkit.Data.SetList where

import Data.Set (Set)
import Manatee.Toolkit.General.Set

import qualified Data.Set as Set

data Ord a => SetList a =
    SetList {listCounter :: Int  -- counter for build unique id use in Set
            ,listSet     :: Set a}

-- | New SetList.
setListNew :: Ord a => SetList a
setListNew = SetList 0 Set.empty

-- | Get counter for SetList.
setListGetCounter :: Ord a => SetList a -> Int 
setListGetCounter = listCounter

-- | Increase counter and then get new counter.
-- Return new setList and new id. 
setListGetNewCounter :: Ord a => SetList a -> (Int, SetList a)
setListGetNewCounter sl = (newId, newSetList)
    where newSetList = setListIncCounter sl
          newId      = setListGetCounter newSetList

-- | Set counter.
setListSetCounter :: Ord a => SetList a -> Int -> SetList a
setListSetCounter sl c = sl {listCounter = c}

-- | Increase counter.
setListIncCounter :: Ord a => SetList a -> SetList a 
setListIncCounter sl@(SetList {listCounter = c}) = sl {listCounter = succ c}

-- | Add new node in SetList.
setListAddNode :: Ord a => SetList a -> a -> SetList a
setListAddNode sl@(SetList {listSet = s}) e = sl {listSet = Set.insert e s}

-- | Remove node from SetList.
setListRemoveNode :: Ord a => SetList a -> a -> SetList a
setListRemoveNode sl@(SetList {listSet = s}) e = sl {listSet = Set.delete e s}

-- | Remove others node except current one.
setListRemoveOthersNode :: Ord a => SetList a -> a -> SetList a
setListRemoveOthersNode sl = setListAddNode (setListEmptySet sl)

-- | Find some node and return.
setListGetNode :: Ord a => SetList a -> (a -> Bool) -> Maybe a
setListGetNode = maybeFindMin . listSet

-- | Like `setListGetNode`, but use Monad wrap search condition.
setListGetNodeM :: (Ord a, Monad m) => SetList a -> (a -> m Bool) -> m (Maybe a)
setListGetNodeM = maybeFindMinM . listSet
  
-- | Empty List, but keep counter.                  
setListEmptySet :: Ord a => SetList a -> SetList a
setListEmptySet sl = sl {listSet = Set.empty}

-- | Convert from SetList to list.
setListGetList :: Ord a => SetList a -> [a]
setListGetList (SetList {listSet = s}) = Set.toList s
