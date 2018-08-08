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

module Manatee.Toolkit.General.Either where

-- | Map left.
mapLeft :: (t1 -> a) -> Either t1 t -> Either a t
mapLeft _ (Right a) = Right a
mapLeft f (Left a)  = Left (f a)

-- | Map right.
mapRight :: (t1 -> b) -> Either t t1 -> Either t b
mapRight _ (Left a)  = Left a
mapRight f (Right a) = Right (f a)

-- | Apply either left.
applyEitherLeft :: (a -> c) -> Either a b -> Maybe c
applyEitherLeft f (Left l)  = Just $ f l
applyEitherLeft _ (Right _) = Nothing

-- | Apply either right.
applyEitherRight :: (b -> c) -> Either a b -> Maybe c
applyEitherRight _ (Left _)  = Nothing
applyEitherRight f (Right r) = Just $ f r

