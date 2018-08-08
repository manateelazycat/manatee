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

module Manatee.Toolkit.General.String where

import Data.Char
import Data.List (isSuffixOf)
import Control.Arrow

-- | Search forward word.
-- Return (Word, RestStr) if found word at beginning.
-- Otherwise return ("", AllStr).
searchForwardWord :: String -> (String, String)
searchForwardWord "" = ("", "")
searchForwardWord (x:xs) 
    | isAlphaNum x
      = let (word, rest) = searchForwardWord xs
        in (x : word, rest)
    | otherwise
      = ("", x : xs)

-- | Like `searchForwardWord` except reverse ordering. 
searchBackwardWord :: String -> (String, String)
searchBackwardWord str = 
  (reverse *** reverse) $ searchForwardWord (reverse str)

-- | Whether string is blank.
isBlankString :: String -> Bool
isBlankString [] = True
isBlankString (x:xs)    
    | isSpace x 
        = isBlankString xs
    | otherwise 
        = False
-- | Convert string to int.
stringToInt :: Int -> String -> Int
stringToInt base digits
    = sign * (foldl acc 0 $ concatMap digToInt digits1)
      where
      splitSign ('-' : ds) = ((-1), ds)
      splitSign ('+' : ds) = ( 1  , ds)
      splitSign ds         = ( 1  , ds)
      (sign, digits1)      = splitSign digits
      digToInt c
          | c >= '0' && c <= '9'
              = [ord c - ord '0']
          | c >= 'A' && c <= 'Z'
              =  [ord c - ord 'A' + 10]
          | c >= 'a' && c <= 'z'
              =  [ord c - ord 'a' + 10]
          | otherwise
              = []
      acc i1 i0
          = i1 * base + i0

-- | Convert a string of hexadecimal digits into an Int
hexStringToInt :: String -> Int
hexStringToInt = stringToInt 16

-- | Drop suffix.
dropSuffix :: [String] -> String -> String 
dropSuffix [] str = str
dropSuffix (x:xs) str 
    | x `isSuffixOf` str
      = take (length str - length x) str
    | otherwise 
      = dropSuffix xs str

-- | Strip and format.
stripFormat :: String -> Int -> String
stripFormat str limit 
    | limit < 0 
      = str
    | length str <= limit 
      = str
    | otherwise 
      = take limit str ++ "..."

-- | Unlines except last one.
unlinesExceptLast :: [String] -> String
unlinesExceptLast [] = ""
unlinesExceptLast list = init $ unlines list

