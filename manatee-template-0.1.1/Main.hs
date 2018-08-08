-- Author:     Your Name <YourMail>
-- Maintainer: Your Name <YourMail>
-- 
-- Copyright (C) 2010 Your Name, all rights reserved.
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

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative hiding (empty)
import Manatee.Core.Dynload
import Manatee.Core.Render
import Manatee.Core.Types
import Manatee.Extension.Template

-- | Template render process.
main :: IO ()
main = 
    -- This to wrap Core.Render.startupRender to embeded application code in Manatee render framework. 
    -- startupRender :: PageBufferNewFun -> CustomizeNewFun -> IO ()
    startupRender 

    -- Page buffer new function.
    -- type PageBufferNewFun = FilePath -> [String] -> Client -> PageId -> CustomizeWrap -> IO PageBufferWrap 
    (\ pagePath options client pageId customize -> 
         PageBufferWrap <$> templateBufferNew pagePath options client pageId customize)

    -- Default customize new function.
    -- emptyCustomizeNew :: IO CustomizeWrap
    emptyCustomizeNew
