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

module Manatee.Extension.Editor.PageMode where

import Manatee.Core.Types

import qualified Data.Map as M

-- | C mode.
cMode :: PageMode
cMode = 
  PageMode {pageModeName        = "C"
           ,pageModeRegexp      = "^*\\.(c|h)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Haskell mode.
haskellMode :: PageMode
haskellMode =
  PageMode {pageModeName        = "Haskell"
           ,pageModeRegexp      = "^*\\.([hg]s|hs-boot|chs)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Elisp
elispMode :: PageMode
elispMode =
  PageMode {pageModeName        = "Elisp"
           ,pageModeRegexp      = "^*\\.el$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Pascal
pascalMode :: PageMode
pascalMode =
  PageMode {pageModeName        = "Pascal"
           ,pageModeRegexp      = "^*\\.(p|pas)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Fortran
fortranMode :: PageMode
fortranMode =
  PageMode {pageModeName        = "Fortran"
           ,pageModeRegexp      = "^*\\.(f|F|for)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Lisp
lispMode :: PageMode
lispMode =
  PageMode {pageModeName        = "Lisp"
           ,pageModeRegexp      = "^*\\.(l|lsp|lisp|ml|asd)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Ada
adaMode :: PageMode
adaMode =
  PageMode {pageModeName        = "Ada"
           ,pageModeRegexp      = "^*\\.ad([abs]|[bs]?dg)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Sql
sqlMode :: PageMode
sqlMode =
  PageMode {pageModeName        = "Sql"
           ,pageModeRegexp      = "^*\\.sql$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Prolog
prologMode :: PageMode
prologMode =
  PageMode {pageModeName        = "Prolog"
           ,pageModeRegexp      = "^*\\.prolog$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Asm
asmMode :: PageMode
asmMode =
  PageMode {pageModeName        = "Asm"
           ,pageModeRegexp      = "^*\\.(asm|[sS]?)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Tcl
tclMode :: PageMode
tclMode =
  PageMode {pageModeName        = "Tcl"
           ,pageModeRegexp      = "^*\\.(i?tcl|exp|itk)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | F90
f90Mode :: PageMode
f90Mode =
  PageMode {pageModeName        = "F90"
           ,pageModeRegexp      = "^*\\.f90[05]?$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Textinfo
textinfoMode :: PageMode
textinfoMode =
  PageMode {pageModeName        = "Textinfo"
           ,pageModeRegexp      = "^*\\.(texinfo|te?xi)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Latex
latexMode :: PageMode
latexMode =
  PageMode {pageModeName        = "Latex"
           ,pageModeRegexp      = "^*\\.(ltx|sty|cl[so]|bbl)$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Text
textMode :: PageMode
textMode =
  PageMode {pageModeName        = "Text"
           ,pageModeRegexp      = "^*\\.txt$"
           ,pageModeKeymap      = M.empty
           ,pageModeLoad        = \_ -> return ()}

-- | Mode list.
sourceModeList :: [PageMode]
sourceModeList = 
  [cMode
  ,haskellMode
  ,elispMode
  ,pascalMode
  ,fortranMode
  ,lispMode
  ,adaMode
  ,sqlMode
  ,prologMode
  ,asmMode
  ,tclMode
  ,f90Mode
  ,textinfoMode
  ,latexMode
  ,textMode]

