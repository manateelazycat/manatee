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

module Manatee.Toolkit.General.Set where

import Control.Monad hiding (filterM, mapM)
import Data.Set 

import qualified Control.Monad as CM
import qualified Data.Set as Set

-- | Find min element match in 
maybeFindMin :: Ord a => Set a -> (a -> Bool) -> Maybe a
maybeFindMin s f 
    | Set.null set 
        = Nothing
    | otherwise    
        = Just $ findMin set
    where
      set = Set.filter f s

-- | maybeFindMinM.
maybeFindMinM :: (Ord a, Monad m) => Set a -> (a -> m Bool) -> m (Maybe a)
maybeFindMinM s f = do
  set <- filterM f s
  return $
         if Set.null set
            then Nothing
            else Just $ findMin set
  
-- | filterM for Data.
filterM :: (Ord a, Monad m) => (a -> m Bool) -> Set a -> m (Set a)
filterM f xs = fromList `liftM` CM.filterM f (toList xs)

-- | mapM for Data.Set
mapM :: (Ord a, Ord b, Monad m) => (a -> m b) -> Set a -> m (Set b)
mapM f sa = fromList `liftM` CM.mapM f (toList sa)

-- | Next element
nextElement :: (Ord a) => Set a -> a -> Maybe a
nextElement set a  
    | hasA 
        = if Set.null nextSet
             then Nothing
             else Just $ findMin nextSet 
    | otherwise 
        = Nothing
  where (_, hasA, nextSet) = splitMember a set 

-- | Prev element
prevElement :: (Ord a) => Set a -> a -> Maybe a
prevElement set a 
    | hasA
        = if Set.null prevSet
             then Nothing
             else Just $ findMax prevSet
    | otherwise 
        = Nothing
  where (prevSet, hasA, _) = splitMember a set 

-- | Try find min element.
tryFindMin :: (Ord a) => Set a -> Maybe a
tryFindMin set 
    | Set.null set = Nothing
    | otherwise    = Just $ findMin set

-- | Try find min element.
tryFindMax :: (Ord a) => Set a -> Maybe a
tryFindMax set 
    | Set.null set = Nothing
    | otherwise    = Just $ findMax set


