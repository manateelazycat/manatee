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

{-# LANGUAGE OverloadedStrings #-}
module Manatee.Toolkit.General.ByteString where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (isSpaceWord8, c2w)
import System.FilePath (pathSeparator)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B

-- | Like `System.FilePath.combine`, except for ByteString.
combine :: ByteString -> ByteString -> ByteString
combine dir file 
    | B.isSuffixOf separator dir
      = B.concat [dir, file]
    | otherwise
      = B.concat [dir, separator, file]
        where separator = B.singleton pathSeparator

-- | Is blank string.
isBlankByteString :: ByteString -> Bool
isBlankByteString =
  BS.all isSpaceWord8 

-- | Wraps a list of words to a list of lines of words of a particular width.
wrapLine :: Int -> [ByteString] -> [[ByteString]]
wrapLine width = wrap 0 []
  where wrap :: Int -> [ByteString] -> [ByteString] -> [[ByteString]]
        wrap 0   []   (w:ws)
          | B.length w + 1 > width
          = wrap (B.length w) [w] ws
        wrap col line (w:ws)
          | col + B.length w + 1 > width
          = reverse line : wrap 0 [] (w:ws)
        wrap col line (w:ws)
          = let col' = col + B.length w + 1
             in wrap col' (w:line) ws
        wrap _ []   [] = []
        wrap _ line [] = [reverse line]
    
-- | Like `words`, but just split with first space char, don't truncates rest space char. 
splitWords :: ByteString -> [ByteString]
splitWords bytestring = 
  map (BS.map (\x -> if isSpaceWord8 x then c2w ' ' else x)) $ BS.splitWith isSpaceWord8 bytestring
