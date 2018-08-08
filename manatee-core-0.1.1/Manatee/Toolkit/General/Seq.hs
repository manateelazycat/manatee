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

module Manatee.Toolkit.General.Seq where

import Data.Sequence (Seq, (><), (<|), (|>), findIndexL, adjust)

import qualified Data.Sequence as Seq

-- | Filter min.
filterMin :: (a -> Bool) -> Seq a -> Maybe a
filterMin f seq
    | Seq.null filterSeq 
        = Nothing
    | otherwise 
        = Just $ Seq.index filterSeq 0
    where filterSeq = Seq.filter f seq 

-- | Maybe index.
maybeIndex :: Seq a -> Int -> Maybe a
maybeIndex seq index 
    | index < min || index > max 
        = Nothing
    | otherwise 
        = Just $ Seq.index seq index
  where 
    min = 0
    max = Seq.length seq - 1

-- | Insert element in sequence.
insertAt :: Int -> a -> Seq a -> Seq a
insertAt n x xs = ys >< (x <| zs)
    where
      (ys, zs) = Seq.splitAt n xs 

-- | Delete element in sequence.
deleteAt :: Int -> Seq a -> Seq a
deleteAt n xs = ys >< Seq.drop 1 zs
    where
      (ys, zs) = Seq.splitAt n xs 

-- | Swap two element in sequence.
swap :: Int -> Int -> Seq a -> Seq a      
swap deleteId insertId xs
    | deleteId < min || deleteId > max = xs
    | insertId < min || insertId > max = xs
    | otherwise = zs
  where 
    min = 0
    max = Seq.length xs - 1
    x   = Seq.index xs deleteId
    ys  = deleteAt deleteId xs
    zs  = insertAt insertId x ys

-- | Delete match element in sequence.
deleteMatch :: (a -> Bool) -> Seq a -> Seq a
deleteMatch f seq = newSeq
  where index  = findIndexL f seq
        newSeq = case index of
                   Just i  -> deleteAt i seq
                   Nothing -> seq

-- | Replace element if found match.
tryReplace :: (a -> Bool) -> a -> Seq a -> Seq a                   
tryReplace f a seq = 
  case findIndexL f seq of
    Just i  -> adjust (const a) i seq
    Nothing -> seq
    
-- | Replace or add.    
replaceOrAdd :: (a -> Bool) -> a -> Seq a -> Seq a
replaceOrAdd f a seq = 
  case findIndexL f seq of
    -- Replace old value if found match
    Just i  -> adjust (const a) i seq
    -- Otherwise add new value to end.
    Nothing -> (|>) seq a        
