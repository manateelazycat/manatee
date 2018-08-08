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

module Manatee.Toolkit.General.Maybe where

import Control.Monad
import Data.Maybe

-- | Indicate error or return a.
-- This function for replace `fromJust`, expression `fromJust x` is bad
-- when `x` is `Nothing`, so `maybeError` allowed you customize error
-- information.
maybeError :: Maybe a -> String -> a
maybeError m str = fromMaybe (error $ "Just crash : " ++ str) m

-- | Maybe boolean.
maybeBool :: Maybe a -> Bool -> Bool
maybeBool (Just _) b = b
maybeBool Nothing b = not b

-- | Maybe alternative.
maybeAlternate :: Maybe a -> a -> a
maybeAlternate = flip fromMaybe

-- | Maybe alternative monad.
maybeAlternateM :: Monad m => Maybe a -> m a -> m a
maybeAlternateM x = maybeBranch x return

-- | Apply maybe.
maybeApply :: Maybe a -> (a -> b) -> Maybe b
maybeApply = flip fmap

-- | Apply maybe with monad.
maybeApplyM :: Monad m => Maybe a -> (a -> m b) -> m (Maybe b)
maybeApplyM m f = maybe (return Nothing) (liftM Just . f) m

-- | Maybe transform monad.
(?>=>) :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
m ?>=> f = maybe (return Nothing) f m

-- |
(>?>=>) :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
g >?>=> f = g >>= (?>=> f)

-- | Maybe tranform ().
(?>=) :: Monad m => Maybe a -> (a -> m ()) -> m () 
m ?>= f = maybe (return ()) f m

-- | 
(>?>=) :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
g >?>= f = g >>= (?>= f)

-- | Maybe branch.
maybeBranch :: Monad m => Maybe a -> (a -> m b) -> m b -> m b
maybeBranch (Just a) f _ = f a
maybeBranch Nothing _ g  = g

-- | Maybe head.
maybeHead :: [a] -> Maybe a
maybeHead = listToMaybe
