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

module Manatee.Toolkit.Data.ListZipper where

import Data.Maybe
import Manatee.Toolkit.General.List

import qualified Data.List as List

data ListZipper a = ListZipper ![a] ![a]

instance Show a => Show (ListZipper a) where
    show (ListZipper ls rs) = show (reverse ls) ++ show rs

instance Functor ListZipper where
    fmap f (ListZipper ls rs) = ListZipper (map f ls) (map f rs)

-- | Create an empty ListZipper. 
empty :: ListZipper a
empty = ListZipper [] []

-- | Create a ListZipper with a single element.
singleton :: a -> ListZipper a
singleton a = ListZipper [] [a]

-- | Create a ListZipper from list, and focus focus to first element in list.
fromList :: [a] -> ListZipper a
fromList = ListZipper []

-- | Create a ListZipper from list end, and focus focus to end element in list.
fromListEnd :: [a] -> ListZipper a
fromListEnd as = ListZipper (reverse as) []

-- | Convert ListZipper to List.
toList :: ListZipper a -> [a]
toList (ListZipper ls rs) = reverse ls ++ rs

-- | Whether is first element.
atStart :: ListZipper a -> Bool
atStart (ListZipper [] _ ) = True
atStart _                  = False
-- | Whether is last element.
atEnd :: ListZipper a -> Bool
atEnd (ListZipper _  []) = True
atEnd _                  = False
-- | Whether is empty.
isEmpty :: ListZipper a -> Bool
isEmpty (ListZipper [] []) = True
isEmpty _                  = False

-- | Get current node, return Nothing if haven't found anything at current node.
-- This function won't change current focus node.
getCurrent :: ListZipper a -> Maybe a
getCurrent list = get list $ currentIndex list
-- | Get left node, return Nothing if haven't found anything at left node.
-- This function won't change current focus node.
getLeft :: ListZipper a -> Maybe a
getLeft    list = get list $ leftIndex list
-- | Get right node, return Nothing if haven't found anything at right node.
-- This function won't change current focus node.
getRight :: ListZipper a -> Maybe a
getRight   list = get list $ rightIndex list
-- | Get first node, return Nothing if haven't found anything at first node.
-- This function won't change current focus node.
getFirst :: ListZipper a -> Maybe a
getFirst   list = get list $ firstIndex list
-- | Get last node, return Nothing if haven't found anything at last node.
-- This function won't change current focus node.
getLast :: ListZipper a -> Maybe a
getLast    list = get list $ lastIndex list

-- | Get left node, return Nothing if haven't found anything at left node.
-- If reach first node, circular to end node.
-- This function won't change current focus node.
getLeftCircular :: ListZipper a -> Maybe a
getLeftCircular  list
    | prevIndex < firstIndex list 
        = get list $ lastIndex list
    | otherwise 
        = get list prevIndex
    where
      prevIndex = leftIndex list
-- | Get right node, return Nothing if haven't found anything at right node.
-- If reach last node, circular to first node.
-- This function won't change current focus node.
getRightCircular :: ListZipper a -> Maybe a
getRightCircular list
    | nextIndex > lastIndex list 
        = get list $ firstIndex list
    | otherwise 
        = get list nextIndex
    where
      nextIndex = rightIndex list

-- | Get node with given index.
-- This function won't change current focus node.
get :: ListZipper a -> Int -> Maybe a
get = (?!) . toList 

-- | Focus left node.
focusLeft :: ListZipper a -> Maybe (ListZipper a)
focusLeft  (ListZipper (a:ls) rs) = Just $ ListZipper ls (a:rs)
focusLeft  _                      = Nothing
-- | Focus right node.
focusRight :: ListZipper a -> Maybe (ListZipper a)
focusRight (ListZipper ls (a:rs)) = Just $ ListZipper (a:ls) rs
focusRight _                      = Nothing
-- | Focus first node.
focusFirst :: ListZipper a -> Maybe (ListZipper a)
focusFirst list                   = focus (firstIndex list) list
-- | Focus last node.
focusLast :: ListZipper a -> Maybe (ListZipper a)
focusLast  list                   = focus (lastIndex list) list

-- | Focus node with given index.
focus :: Int -> ListZipper a -> Maybe (ListZipper a)
focus n list
     | n < 0 
         = Nothing
     | n >= lenList 
         = Nothing
     | n == currIndex        
         = Just list
     | n < currIndex         
         = (=<<) (focus n) leftList
     | otherwise             
         = (=<<) (focus n) rightList
     where
       lenList   = Manatee.Toolkit.Data.ListZipper.length list
       currIndex = currentIndex list
       leftList  = focusLeft list
       rightList = focusRight list

-- | Focus node.
focusNode :: Eq a => a -> ListZipper a -> Maybe (ListZipper a)
focusNode a list = 
    (=<<) (`focus` list) index
    where
      index = List.findIndex (a ==) (toList list)

-- | Insert node to right side and keep current focus.
insertLeft :: a -> ListZipper a -> ListZipper a
insertLeft  a (ListZipper ls rs)     = ListZipper (a:ls) rs
-- | Insert node to left side and keep current focus.
-- Except it will focus first element when list is empty.
insertRight :: a -> ListZipper a -> ListZipper a
insertRight a (ListZipper ls (r:rs)) = ListZipper ls (r:a:rs)
insertRight a (ListZipper ls _)      = ListZipper ls [a]
-- | Insert node to first and keep current focus.
insertFirst :: a -> ListZipper a -> ListZipper a
insertFirst a (ListZipper (l:ls) rs) = ListZipper ((l:ls) ++ [a]) rs
insertFirst a (ListZipper _ rs)      = ListZipper [a] rs
-- | Insert node to last end and keep current focus.
insertLast :: a -> ListZipper a -> ListZipper a
insertLast  a (ListZipper ls (r:rs)) = ListZipper ls ((r:rs) ++ [a])
insertLast  a (ListZipper ls _)      = ListZipper ls [a]

-- | Delete current node and focus focus right node.
delete :: ListZipper a -> Maybe (ListZipper a)
delete       (ListZipper ls (_:rs))   = Just $ ListZipper ls rs
delete       _                        = Nothing
-- | Delete left node and keep current focus.
deleteLeft :: ListZipper a -> Maybe (ListZipper a)
deleteLeft   (ListZipper (_:ls) rs)   = Just $ ListZipper ls rs
deleteLeft   _                        = Nothing 
-- | Delete right node and keep current focus.
deleteRight :: ListZipper a -> Maybe (ListZipper a)
deleteRight  (ListZipper ls (r:_:rs)) = Just $ ListZipper ls (r:rs)
deleteRight  _                        = Nothing
-- | Delete first node and keep current focus.
deleteFirst :: ListZipper a -> Maybe (ListZipper a)
deleteFirst  (ListZipper (l:ls) rs)   = Just $ ListZipper (init (l:ls)) rs
deleteFirst  (ListZipper [] (_:rs))   = Just $ ListZipper [] rs
deleteFirst  _                        = Nothing
-- | Delete last node and keep current focus.
deleteLast :: ListZipper a -> Maybe (ListZipper a)
deleteLast   (ListZipper ls (r:rs))   = Just $ ListZipper ls (init (r:rs))
deleteLast   (ListZipper (_:ls) rs)   = Just $ ListZipper ls rs
deleteLast   _                        = Nothing
-- | Delete other node except current node.
deleteOthers :: ListZipper a -> Maybe (ListZipper a)
deleteOthers (ListZipper _ (r:_))     = Just $ ListZipper [] [r]
deleteOthers _                        = Nothing

-- | Delete specify node.
deleteNode :: Eq a => a -> ListZipper a -> Maybe (ListZipper a)
deleteNode a (ListZipper ls rs)
    | isJust leftIndex 
        = Just $ ListZipper (deleteAt (fromJust leftIndex) ls) rs
    | isJust rightIndex
        = Just $ ListZipper ls (deleteAt (fromJust rightIndex) rs)
    | otherwise
        = Nothing
    where
      leftIndex = List.findIndex (a ==) ls
      rightIndex = List.findIndex (a ==) rs

-- | Swap with left node, and focus swap node.
swapLeft :: ListZipper a -> Maybe (ListZipper a)
swapLeft  (ListZipper (l:ls) (r:rs)) = Just $ ListZipper (r:ls) (l:rs)
swapLeft  (ListZipper (l:ls) _)      = Just $ ListZipper ls [l]
swapLeft  _                          = Nothing
-- | Swap with right node, and focus swap node.
swapRight :: ListZipper a -> Maybe (ListZipper a)
swapRight (ListZipper ls (r:x:rs))   = Just $ ListZipper ls (x:r:rs)
swapRight _                          = Nothing

-- | Replace current node.
replace :: a -> ListZipper a -> Maybe (ListZipper a)
replace a (ListZipper ls (_:rs)) = Just $ ListZipper ls (a:rs)
replace _ _                      = Nothing

-- | Get length.
length :: ListZipper a -> Int
length       (ListZipper ls rs) = Prelude.length ls + Prelude.length rs 
-- | Get current index.
currentIndex :: ListZipper a -> Int
currentIndex (ListZipper ls _)  = Prelude.length ls
-- | Get left index.
leftIndex :: ListZipper a -> Int
leftIndex = pred . currentIndex 
-- | Get right index.
rightIndex :: ListZipper a -> Int
rightIndex = succ . currentIndex
-- | Get first index.
firstIndex :: ListZipper a -> Int
firstIndex _ = 0
-- | Get last index.
lastIndex :: ListZipper a -> Int
lastIndex = pred . Manatee.Toolkit.Data.ListZipper.length
