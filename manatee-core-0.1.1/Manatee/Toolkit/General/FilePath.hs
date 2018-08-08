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

module Manatee.Toolkit.General.FilePath where

import Data.Array
import Data.List
import Manatee.Toolkit.General.List
import System.FilePath
import System.Process
import Text.Regex.TDFA

-- | Intersect two file path from end.
fileIntersectEnd :: FilePath -> FilePath -> FilePath
fileIntersectEnd xs ys = 
  -- Remove otiose characters before path separator 
  -- if have previous directory.
  case findIndex (== pathSeparator) zs of
    Just i  -> drop (succ i) zs
    Nothing -> zs
  -- Get common string from end.
  where zs = intersectEnd xs ys 

-- | Get upper directory name.
getUpperDirectoryName :: FilePath -> FilePath
getUpperDirectoryName = takeFileName . dropTrailingPathSeparator . dropFileName

-- | Get upper directory path.
getUpperDirectory :: FilePath -> FilePath
getUpperDirectory = dropFileName . dropTrailingPathSeparator 

-- | Try to expand file path that start with backtick.
-- Just return original one when file path not start with backtick.
expandBacktickFilePath :: FilePath -> IO FilePath
expandBacktickFilePath filepath 
    | filepath =~ "^~.*" :: Bool
        = expandFilePath filepath
    | otherwise
        = return filepath

-- | Expand file name, 
-- including expand backtick and fill blank in filepath.
expandFileName :: FilePath -> IO FilePath
expandFileName = expandBacktickFilePath . fillFilePathBlank

-- | Expand file path.
expandFilePath :: FilePath -> IO FilePath
expandFilePath filepath = fmap (head . lines) (readProcess "bash" ["-c", "echo " ++ filepath] [])

-- | Fill blank in filepath with `\\ `.
fillFilePathBlank :: FilePath -> FilePath
fillFilePathBlank filepath = replaceRegex filepath "[^\\][ ]+" "\\ "
    where 
      -- Replace regex match string with target string. 
      replaceRegex string regex target = do
        -- Test match.
        let matchTextList = string =~ regex :: [MatchText String]
        if null matchTextList
           then 
           -- Return original string if match nothing.
               string
           else do
             -- Replace regex match sting with target string. 
             let indexList = map (snd . head . elems) matchTextList
             replaceMatchText indexList target string
      -- Replace match text.
      replaceMatchText indexList target string
          -- Return current string when list null.
          | null indexList 
              = string
          -- Continue replace.
          | otherwise      
              = matchBefore ++ newMatch ++ replaceMatchText newList target matchAfter
          where 
            (matchOffset, matchLength) = head indexList
            (matchBefore, afterString)  = splitAt matchOffset string
            (match, matchAfter) = splitAt matchLength afterString
            newMatch = head match : target
            adjust = matchOffset + matchLength
            newList = map (\(a,b) -> (a - adjust, b)) $ tail indexList

-- | Filter dot directory. 
filterDotDirectory :: [FilePath] -> [FilePath]
filterDotDirectory = filter notDotDirectory

-- | Filter dot directory apply list.
filterDotDirectoryApply :: (a -> FilePath) -> [a] -> [a]
filterDotDirectoryApply =
    filter . (notDotDirectory .)

-- | File not directory.
notDotDirectory :: FilePath -> Bool
notDotDirectory = (`notElem` [".", ".."])

-- | Like takeFileName, but won't return empty when filepath is root path '/'
-- Instead return '/'.          
takeFileNameExceptRoot :: FilePath -> FilePath
takeFileNameExceptRoot filepath 
    | filepath == "/"
      = filepath
    | otherwise 
      = takeFileName filepath

