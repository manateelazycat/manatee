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

module Manatee.Extension.Template.PageMode where

import Manatee.Core.Types

import qualified Data.Map as M

-- | Template mode.
-- Mode is id string to different application.
templateMode :: PageMode
templateMode = 
  PageMode {pageModeName        = "Template"       -- mode name
           ,pageModeRegexp      = "^*.*$"          -- regexp to match file name
           ,pageModeKeymap      = M.empty          -- mode keymap
           ,pageModeLoad        = \_ -> return ()} -- function loading in current mode
