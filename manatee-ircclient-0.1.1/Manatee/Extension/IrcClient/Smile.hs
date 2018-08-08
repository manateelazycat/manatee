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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Manatee.Extension.IrcClient.Smile where

import Control.Arrow
import Control.Monad
import Data.Array
import Data.ByteString (ByteString)
import Data.Map (Map)
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew, get, Language)
import Manatee.Toolkit.General.String
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import Paths_manatee_ircclient
import System.FilePath
import Text.Regex.Posix hiding (after)

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Map as M

-- | The size of smile.
smileSize :: Int
smileSize = 16

-- | Replace smile string with pixbuf, and return insert offset of pixbuf. 
smileMessage :: ByteString -> Map String Pixbuf -> IO (ByteString, [(Int, Pixbuf)])
smileMessage msg pixbufMap = do
    let -- Get UTF8 string, because insert offset use Unicode char width and not byte char.
        -- so we need convert ByteString to UTF8 string first.
        -- Otherwise we will get wrong insert offset when display UTF8 string (e.g. Chinese).
        msgString = UTF8.toString msg
        -- Get regexp matches.
        stringMatches = 
          -- Filter blank match.
          filter (\ (word, _) -> not $ isBlankString word)
          -- Get match.
          $ map (\x -> 
                     let (_ : (word, (matchOffset, matchLength)) : _) = elems x
                     in (word, (matchOffset, matchLength))) 
                (matchAllText 
                  (makeRegex ("\\s(x\\(|;\\)|:/|:'|:>|:x|:\\*|:z|:\\(|:D|:\\)|:o|:I|;p|:O|)" :: String) :: Regex) 
                  msgString)          
        -- Smile pixbuf.
        smilePixbuf str = 
          let (_, pixbuf) = maybeError
                            (findMinMatch pixbufMap (\ name _ -> name == str))
                            ("smileMessage : can't match smile " ++ str) 
          in pixbuf
        -- Get insert offset.
        getInsertOffset [] _ _ = []
        getInsertOffset [(_, (offset, _))] deleteLen pixbufNum =
          [offset - deleteLen + pixbufNum]
        getInsertOffset ((_, (offset, length)) : xs) deleteLen pixbufNum = 
          (offset - deleteLen + pixbufNum) : getInsertOffset xs (deleteLen + length) (pixbufNum + 1)
        -- Get new message.
        getNewMessage [] _ _ = ""
        getNewMessage [(_, (matchOffset, matchLen))] str deleteOffset = 
            let (fstStr, sndStr) = splitAt (matchOffset - deleteOffset) str
                (_, restStr) = splitAt matchLen sndStr
            in fstStr ++ restStr
        getNewMessage ((_, (matchOffset, matchLen)) : xs) str deleteOffset = 
            let (fstStr, sndStr) = splitAt (matchOffset - deleteOffset) str
                (_, restStr) = splitAt matchLen sndStr
            in fstStr ++ getNewMessage xs restStr (matchOffset + matchLen)
            
    if null stringMatches  
       -- Don't convert anything if no regexp match.
       then return (msg, [])
       else do
         let pixbufs    = map (\ (str, _) -> smilePixbuf str) stringMatches
             offsets    = getInsertOffset stringMatches 0 0
             newMessage = UTF8.fromString $ getNewMessage stringMatches msgString 0

         return (newMessage, zip offsets pixbufs)
    
-- | Create smile pixbufs.
createSmilePixbufs :: IO (Map String Pixbuf)
createSmilePixbufs = do
  dir <- getDataDir
  let imagePath imageName = dir </> "data/icons" </> (imageName ++ ".png")
      paths = map (second imagePath) 
              [("x(", "angry")
              ,(":/", "confused")
              ,(":'", "crying")
              ,(":>", "embarrassed")
              ,(":x", "inlove")
              ,(":*", "kiss")
              ,(":z", "sleepy")
              ,(":(", "sad")
              ,(":D", "laugh")
              ,(":)", "smile")
              ,(":o", "surprised")
              ,(":I", "tired")
              ,(";p", "tongue")
              ,(":O", "whistling")
              ,(";)", "wink")]  
  pixbufs <- 
    forM paths $ \ (smileStr, path) -> do
         pixbuf <- pixbufNewFromFileAtScale path smileSize smileSize True
         return (smileStr, pixbuf)
  return $ M.fromList pixbufs
