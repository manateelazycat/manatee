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

module Manatee.Toolkit.General.Process where

import Control.Concurrent
import Distribution.Simple.Program
import Distribution.Verbosity
import System.Process
import Text.Printf

-- | Like `runProcessDefault`, but wait for child process terminate.
-- To avoid *zombie* child process.
-- You will got zombie child process when child process exist before
-- parent process, and parent process don't wait process. 
runProcess_ :: FilePath -> [String] -> IO ()
runProcess_ path args = 
  runProcessDefault path args 
  >>= \handle -> 
      forkIO (waitForProcess handle >> return ())
             >> return ()

-- | Like `runProcess`, but just accept path and args.
runProcessDefault :: FilePath -> [String] -> IO ProcessHandle
runProcessDefault path args = runProcess path args Nothing Nothing Nothing Nothing Nothing

-- | Like run `runCommand`, but return IO ()
runCommand_ :: String -> IO ()
runCommand_ command =
  runCommand command >> return ()

-- | Run external command and won't kill when parent process exit.
runExternalCommand :: String -> IO ()
runExternalCommand command =
    -- nohup for ignore all hup signal. 
    -- `> /dev/null 2>&1` redirect all stdout (1) and stderr (2) to `/dev/null`
    -- that make external command running quiet!
    runCommand_ $ printf "nohup %s > /dev/null 2>&1" command

-- | Check command whether exist before exectue it.
-- Return reason when not found command.
execute :: String -> String -> Bool -> IO (Maybe String)
execute command args isShutup = do
  path <- programFindLocation (simpleProgram command) normal
  case path of
    Just p -> do
      runCommand_ (printf args p 
                   ++ (if isShutup 
                       -- `> /dev/null 2>&1` redirect all stdout (1) and stderr (2) to `/dev/null`
                       -- that make external command running quiet!
                       then " > /dev/null 2>&1" 
                       else "")) 
      return Nothing
    Nothing ->
      return $ Just (printf "Cannot find command : %s" command)
    
