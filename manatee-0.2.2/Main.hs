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

{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, TypeSynonymInstances, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Manatee.Daemon
import System.Environment

-- | Help string.
helpString :: String
helpString = 
     "Welcome to Manatee (The Haskell/Gtk+ Integrated Live Environment)\n\n"
  ++ "Options:\n"
  ++ "    --no-restate, -nr    don't restore state\n"
  ++ "    --help               display this help and exit\n\n"
  ++ "Please report bug to lazycat.manatee@gmail.com if you found any problem, thanks!"

-- | main entry.
main :: IO ()
main = do
  -- Get program arguments.
  args <- getArgs

  case args of
    [] ->
        daemonMain True
    ["--no-restore"] -> 
        daemonMain False
    ["-nr"] -> 
        daemonMain False
    _ ->
        putStrLn helpString
