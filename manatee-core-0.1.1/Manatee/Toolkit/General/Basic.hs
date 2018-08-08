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

module Manatee.Toolkit.General.Basic where

import Control.Arrow

import Control.Monad hiding (filterM, mapM)

-- | Swap tuple.
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- | Try to swap tuple.
ifSwap :: Bool -> (a, a) -> (a, a)
ifSwap b x =
    if b then swap x else x

-- | Duplicate current value with tuple.
dup :: a -> (a, a)
dup a = (a,a)

-- | Transform Int to Double
i2d :: Integral a => a -> Double
i2d v = 
  fromIntegral v :: Double

-- | Integer to Int.
integerToInt :: Integer -> Int
integerToInt t = 
  fromInteger t :: Int

-- | Flip >>
(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)

-- | Like LiftM2, but make function at middle of two arguments.
liftM2' :: Monad m => m a -> (a -> b -> c) -> m b -> m c
liftM2' xs f = liftM2 f xs

liftM2_ :: Monad m => (a -> b -> c) -> m a -> m b -> m ()
liftM2_ f x y = liftM2 f x y >> return ()

-- | If monad.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM f g h = f >>= \b -> if b then g else h

-- | If monad.
ifF :: Monad m => a -> (a -> m Bool) -> (a -> m b) -> (a -> m b) -> m b
ifF a f g h = f a >>= \b -> if b then g a else h a

-- | When monad.
whenM :: Monad m => m Bool -> m () -> m ()
whenM f g = f >>= \x -> when x g

-- | Unless monad.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM f g = f >>= \x -> unless x g

-- | Zip'
zip' :: a -> b -> (a, b)
zip' a b = (a, b)

-- | ZipM'
zipM' :: Monad m => m a -> m b -> m (a, b)
zipM' = liftM2 zip' 

-- | FirstMap.
firstMap :: (b -> c) -> [(b, d)] -> [(c, d)]
firstMap = map . first

-- | Zip with map.
zipMap :: (a -> b, a -> c) -> a -> (b, c)
zipMap (g, h) s = (g s, h s)

-- | Unzip with map.
unzipMap :: (a -> (b,c)) -> (a -> b, a -> c)
unzipMap h = (fst . h, snd . h)

-- | Trace.
trace :: ((a, c) -> (b, c)) -> a -> b
trace f b = c
    where (c, d) = f (b, d)

-- | Floor to multiple.
floorToMultiple :: Integral a => a -> a -> a
floorToMultiple x y = x - x `mod` y

-- | Like a switch statement, and less cluttered than if else if
-- 
-- > cond [ (t1,a1), (t2,a2), ... ]
cond :: Monad m => [(Bool, m ())] -> m ()
cond [] = return ()
cond ((True,action) : _) = action 
cond ((False,_) : rest) = cond rest

-- | Like a switch statement, and less cluttered than if else if 
-- 
-- > condM [ (t1,a1), (t2,a2), ... ]
condM :: Monad m => [(m Bool, m ())] -> m ()
condM [] = return ()
condM ((test,action) : rest) = test >>= \t -> if t then action else condM rest
