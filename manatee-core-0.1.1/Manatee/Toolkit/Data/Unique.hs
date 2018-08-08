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

module Manatee.Toolkit.Data.Unique where

import Control.Arrow
import Data.List
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.FilePath
import Manatee.Toolkit.General.List
import System.FilePath 

import qualified Data.Map as M                                                                     

-- | Unique file path list. 
unique :: [FilePath] -> [(FilePath, FilePath)]
unique filePathList = 
    -- Zip file name and different directory name.
    zip fnl
            -- Get file path.
            $ map snd
            -- To list.
            $ M.toList
            $ M.fromList
            -- Swap filepath and page index.
            $ map swap
            -- Get different directory name for unique.
            $ concatMap uniqueDiff
              -- Group page name with same filename.
              (uniqueGroup (pairPred fpl) uniquePick)
    -- Trailing path separator when filepath is directory name.
    where fpl = map dropTrailingPathSeparator filePathList
          fnl = map takeFileNameExceptRoot fpl

-- | Different file path.
uniqueDiff :: [(FilePath, Int)] -> [(FilePath, Int)]
-- Don't do different when just alone file path.
uniqueDiff (xs:[]) = [first (const "") xs]
-- Generate minimum different directory between filepathes.
uniqueDiff list = concatMap uniqueDiffInternal (uniqueGroup list uniquePick_)

-- | Internal function for different file path. 
uniqueDiffInternal :: [(FilePath, Int)] -> [(FilePath, Int)]
uniqueDiffInternal [] = []
-- Just add previous directory when alone file path. 
uniqueDiffInternal (xs:[]) = [first getUpperDirectoryName xs]
-- Generate minimum different directory between filepathes.
uniqueDiffInternal list = 
    map (\(a,b) -> (uniqueMinDiffDir a (uniqueCommon $ map fst list), b)) list

-- | Get min diff directory.
uniqueMinDiffDir :: FilePath -> FilePath -> FilePath
uniqueMinDiffDir filepath common = 
    addTrailingPathSeparator $ take (length filepath - length common) filepath
    
-- | Group file path with same file name.
uniqueGroup :: [(FilePath, Int)] -> ([(FilePath, Int)] -> [(FilePath, Int)]) -> [[(FilePath, Int)]]
uniqueGroup [] _ = []
uniqueGroup list@(_:[]) _ = group list
uniqueGroup list@(_:xs) f = pl : uniqueGroup ys f
    where
      pl = f list
      ys = if length pl == 1
           -- If pick list just have one element, get remain elements.
           then xs
           -- Otherwise delete pick list from current list.
           else (\\) list pl

-- | Pick same file name from list.
uniquePick :: [(FilePath, Int)] -> [(FilePath, Int)]
uniquePick [] = []
uniquePick (x:[]) = [x]
uniquePick (x@(a,_):y@(b,_):ys)
    | takeFileName a == takeFileName b 
        = x : uniquePick (y:ys)
    | otherwise 
        = uniquePick (x : ys)

-- | Pick same path part from list.
uniquePick_ :: [(FilePath, Int)] -> [(FilePath, Int)]
uniquePick_ [] = []
uniquePick_ (x:[]) = [x]
uniquePick_ (x@(a,_):y@(b,_):ys)
    | fileIntersectEnd a b == takeFileName a 
        = uniquePick_ (x : ys)
    | otherwise 
        = x : uniquePick_ (y:ys)

-- | Get common path with all file path.
uniqueCommon :: [FilePath] -> FilePath
uniqueCommon [] = []
-- Remove otiose characters before path separator. 
uniqueCommon (x:[]) = dropWhile (/= pathSeparator) x
-- Compare all element in list for get common part.
uniqueCommon (x:y:ys) = uniqueCommon (intersectEnd x y : ys)
