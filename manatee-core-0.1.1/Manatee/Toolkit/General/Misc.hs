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

{-# LANGUAGE FlexibleContexts #-}
module Manatee.Toolkit.General.Misc where

import Data.List.Split (splitEvery)
import Data.Text.Lazy (Text, pack)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty
import Manatee.Toolkit.General.Basic
import Text.Printf
import Text.Regex.TDFA

-- | Pretty show.
prettyShow :: Show a => a -> String
prettyShow = 
  (\s -> case parseExp s of
          ParseOk x -> prettyPrint x
          ParseFailed {} -> s) . show

-- | Like =~ , but don't care case sensitive.
(=~^) :: (RegexLike Regex source1, RegexMaker Regex CompOption ExecOption source) => source1 -> source -> Bool
source =~^ regex = matchTest (makeRegexOpts (defaultCompOpt {caseSensitive = False}) defaultExecOpt regex) source 

-- | Show to text.
showText :: (Show a) => a -> Text
showText = pack . show

-- | Format file size (bytes) to hummable size.
-- Defalut use 1 bit after dot if you don't set bit number.
formatFileSizeForDisplay :: Integer -> Int -> String
formatFileSizeForDisplay size bits
    | size < 2 ^ 10  = humanSize 1 ++ " B"
    | size < 2 ^ 20  = humanSize (2 ^ 10)  ++ " KB"
    | size < 2 ^ 30  = humanSize (2 ^ 20)  ++ " MB"
    | size < 2 ^ 40  = humanSize (2 ^ 30)  ++ " GB"
    | size < 2 ^ 50  = humanSize (2 ^ 40)  ++ " TB"
    | size < 2 ^ 60  = humanSize (2 ^ 50)  ++ " PB"
    | size < 2 ^ 70  = humanSize (2 ^ 60)  ++ " EB"
    | size < 2 ^ 80  = humanSize (2 ^ 70)  ++ " ZB"
    | size < 2 ^ 90  = humanSize (2 ^ 80)  ++ " YB"
    | size < 2 ^ 100 = humanSize (2 ^ 90)  ++ " NB"
    | size < 2 ^ 110 = humanSize (2 ^ 100) ++ " DB"
    where 
      formatStr = "%." ++ show bits ++ "f"
      humanSize base = printf formatStr (i2d size / base) :: String

-- | Format float with specify precision.
formatFloatN :: Double -> Int -> Double
formatFloatN n d =
  fromIntegral (floor $ n * 10 ^ d) / 10 ^ d

-- | splitInt 1000000 => "1,000,000"
splitInt :: Int -> String
splitInt xx = init $ concatMap (++ ",") $ reverse $ splitEvery 3 (reverse $ show xx)

