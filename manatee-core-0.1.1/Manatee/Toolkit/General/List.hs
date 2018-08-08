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

{-# LANGUAGE TransformListComp #-}
module Manatee.Toolkit.General.List where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Data.Monoid
import Manatee.Toolkit.General.Basic

-- | Return element of list with given index.
(?!) :: [a] -> Int -> Maybe a
[] ?! _ = Nothing
xs ?! n
    | n < 0 
        = Nothing
    | n >= length xs 
        = Nothing
    | otherwise               
        = listToMaybe . drop n $ xs

-- | Intersect element with list from end, don't including front or middle element.
-- Return null if haven't intersection element.
intersectEnd :: Ord a => [a] -> [a] -> [a]
intersectEnd xs ys = reverse $ intersectFront (reverse xs) (reverse ys)

-- | Intersect element with list from front, don't including end or middle element.
-- Return null if haven't intersection element.
intersectFront :: Ord a => [a] -> [a] -> [a]
intersectFront xs ys = [x | (x,y) <- zip xs ys, then takeWhile by x == y]

-- | Delay list with given start index.
delay :: Int -> [a] -> [Int]
delay n (_:xs) = n : delay (n + 1) xs
delay _ _      = []

-- | Index of list
listIndex :: [a] -> [Int]
listIndex = delay 0

-- | Pair with list index.
pairPred :: [a] -> [(a, Int)]
pairPred xs = zip xs $ listIndex xs

-- | Different two list, and two lists must have same length.
-- otherwise throw a error.
different :: Ord a => [a] -> [a] -> [a]
different (x:xs) (y:ys) 
    | x == y    = different xs ys
    | otherwise = x : different xs ys
different [] [] = []
different _ _   = error "different: lists not the same length!"

-- | Do action when list not empty.
unlessNull :: [a] -> IO () -> IO ()
unlessNull = unless . null

-- | not . null
has :: [a] -> Bool
has = not . null

-- | Head monad list.
headM :: Monad m => m [a] -> m a
headM = liftM head

-- | Last monad list.
lastM :: Monad m => m [a] -> m a
lastM = liftM last

-- | ConcatM.
concatM :: Monad m => m [a] -> m [a] -> m [a]
concatM = liftM2 (++) 

-- | Replace n'th element (count from 0) in `xs` to `x` 
replaceAt :: Int -> [a] -> a -> [a]
replaceAt n xs x = take n xs ++ x : drop (n + 1) xs

-- | Split list with given condition.
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = fst tuple : splitWith f (dropWhile f $ snd tuple)
  where tuple = break f xs

-- | concatMapM.
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat $ forM xs f

-- | Like find, but works with monadic computation instead of pure function.
-- In expression `find FUNCTION list`, if FUNCTION is "IO Bool", you can use
-- `findM FUNCTION list` to instead.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = ifM (f x) (return $ Just x) (findM f xs)

-- | Apply two monad function with list. 
-- And return new monad tuples list.
apply2M :: Monad m => [a] -> (a -> m b) -> (a -> m c) -> m [(b, c)]
apply2M xs f g = forM xs (\x -> zipM' (f x) (g x))

-- | Apply two function with list.
-- And return new tuples list.
apply2 :: [a] -> (a -> b) -> (a -> c) -> [(b, c)]
apply2 xs f g = map (f &&& g) xs

-- | Partition list.
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = 
    liftM2 mappend (ifM (f x) (return ([x], [])) (return ([], [x]))) (partitionM f xs)

-- | Like `init`, but accept empty list.
init_ [] = []
init_ xs = init xs

-- | Like `tail`, but accept empty list.
tail_ [] = []
tail_ xs = tail xs

-- | Like `foldl1`, but accept empty list. 
foldl1_ _ [] = []
foldl1_ f xs = foldl1 f xs

-- | Find next element.
findNext :: (a -> Bool) -> [a] -> Maybe a
findNext x = listToMaybe . drop 1 . dropWhile (not . x)

-- | Find next cycle.
findNextCycle :: (a -> Bool) -> [a] -> Maybe a
findNextCycle _ []   = Nothing
findNextCycle f list = 
  case dropWhile (not . f) list of
    []  -> Nothing               -- `Nothing` when element not found in list
    [_] -> Just $ head list      -- Get head when element at last of list
    ls  -> listToMaybe $ drop 1 ls -- Otherwise get next element in list
        
-- | Find previous cycle.
findPrevCycle :: (a -> Bool) -> [a] -> Maybe a
findPrevCycle _ []   = Nothing
findPrevCycle f list 
    | length dropList == length list  
        = Nothing              -- `Nothing` when element not found in list
    | null dropList     
        = Just $ last list     -- Get last when element at first position of list
    | otherwise         
        = Just $ last dropList -- Otherwise get previous element in list
    where dropList = takeWhile (not . f) list

-- | Delete at.
deleteAt :: Int -> [a] -> [a]
deleteAt _ [] = []
deleteAt i xs
    | i < 0 
      = []
    | i >= length xs
      = []
    | otherwise 
      = (\(a, b) -> a ++ tail b) (splitAt i xs)

-- | Get last one.
getLast :: [a] -> Maybe a 
getLast [] = Nothing
getLast xs = Just $ last xs

-- | Get first one.
getFirst :: [a] -> Maybe a
getFirst [] = Nothing
getFirst xs = Just $ head xs

-- | Zip with list index.
zipWithIndex :: [a] -> (a -> Int -> c) -> [c]
zipWithIndex xs f = zipWith f xs [0..]

-- | Zip with list index.
zipWithIndexM :: Monad m => [a] -> (a -> Int -> m c) -> m [c]
zipWithIndexM xs f = zipWithM f xs [0..]

-- | Zip with list index.
zipWithIndexM_ :: Monad m => [a] -> (a -> Int -> m c) -> m ()
zipWithIndexM_ xs f = zipWithM_ f xs [0..]

-- | Like 'concatMap', but don't concat last one.
addMap :: ([a] -> [a]) -> [[a]] -> [a]
addMap _ []  = []
addMap _ [x] = x
addMap f xs  = concatMap f (init xs) ++ last xs

-- | Like 'insert' but just insert unique element.
insertUnique :: Ord a => a -> [a] -> [a]
insertUnique x xs 
    | x `elem` xs = xs
    | otherwise   = insert x xs

