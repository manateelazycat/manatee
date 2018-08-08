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

module Manatee.Toolkit.General.Typeable where

import Data.ByteString.Lazy.UTF8 (ByteString)
import Data.Dynamic
import Manatee.Toolkit.General.Maybe

import qualified Data.ByteString.Lazy.UTF8 as U

-- | Get type string of dynamic type.
dynTypeString :: Dynamic -> String
dynTypeString = tyConString . typeRepTyCon . dynTypeRep

-- | Type String.
typeString :: Typeable a => a -> String
typeString = tyConString . typeRepTyCon . typeOf

-- | Cast to Type with String. 
castType :: Typeable a => String -> Maybe a
castType str = cast $ mkTyCon str

-- | Type of byte string.
typeByteString :: Typeable a => a -> ByteString
typeByteString = U.fromString . tyConString . typeRepTyCon . typeOf

-- | Convert Type.
convertType :: (Typeable a, Typeable b) => a -> b
convertType a = maybeError (cast a) (typeString a)
