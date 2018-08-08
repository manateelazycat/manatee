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

module Manatee.Toolkit.Cabal.Utils where

import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Package
import System.FilePath
import Data.Version

-- | Get package's data directory.
-- Because Paths_<pkg>.hs just generate after parse Setup.hs,
-- So you can't import Paths_<pkg>.hs and use `getDataDir` in Setup.hs
-- You can use this function in Setup.hs, same as `getDataDir`. :)
getDataDir :: PackageDescription -> LocalBuildInfo -> FilePath
getDataDir pack_des lbi = dir
    where (PackageName name) = pkgName $ package pack_des
          version = pkgVersion $ package pack_des
          subdirName = name ++ "-" ++ showVersion version
          prefixDir = substPathTemplate (package pack_des) lbi (prefix $ installDirTemplates lbi)
          dir = prefixDir </> "share" </> subdirName
  
-- | Get file under datadir.
getDataFilePath :: PackageDescription -> LocalBuildInfo -> FilePath -> FilePath 
getDataFilePath pack_des lbi file =
  getDataDir pack_des lbi </> file