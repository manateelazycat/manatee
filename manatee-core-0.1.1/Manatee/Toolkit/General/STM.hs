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

module Manatee.Toolkit.General.STM where

import Control.Applicative hiding (empty)
import Control.Concurrent.STM 
import Manatee.Toolkit.General.Basic

-- | Read an STM variable, apply some transformation function to it, 
-- and write the transformed value back to the same variable.
modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar var f = do
  x <- readTVar var
  writeTVar var (f x)

-- | The IO version of `modifyTVar`.
modifyTVarIO :: TVar a -> (a -> a) -> IO ()    
modifyTVarIO var f = atomically $ modifyTVar var f

-- | Replace TVar field. 
replaceTVarField :: a -> a -> (a -> TVar b) -> IO ()
replaceTVarField old new field =
  readTVarIO (field new) >>= writeTVarIO (field old)

-- | The IO version of `modifyTVar`.
modifyTVarIOM :: TVar a -> (a -> IO a) -> IO ()    
modifyTVarIOM var f = do
  oldVar <- readTVarIO var
  newVar <- f oldVar
  writeTVarIO var newVar

-- | The IO version of `writeTVar`.
writeTVarIO :: TVar a -> a -> IO ()
writeTVarIO a b = atomically $ writeTVar a b

-- | The IO version of `writeTVar`.
writeTVarIOM :: TVar a -> IO a -> IO ()
writeTVarIOM a f = 
  f >>= \x -> atomically $ writeTVar a x

-- | Ticket TVar.
tickTVar :: TVar Int -> STM Int
tickTVar x = readTVar x << modifyTVar x succ

-- | Like `tickTVar` but output with io monad.
tickTVarIO :: TVar Int -> IO Int
tickTVarIO = atomically . tickTVar

-- | Crock TVar.
crockTVar :: TVar Int -> STM Int
crockTVar x = readTVar x << modifyTVar x pred

-- | Like `crockTVar` but output with io monad.
crockTVarIO :: TVar Int -> IO Int
crockTVarIO = atomically . crockTVar

-- | Query TVar with some function.
queryTVarIO :: TVar a -> (a -> b) -> IO b
queryTVarIO var f = f <$> readTVarIO var

-- | dupTChanIO
dupTChanIO :: TChan a -> IO (TChan a)
dupTChanIO = atomically . dupTChan

-- | readTChanIO
readTChanIO :: TChan a -> IO a
readTChanIO = atomically . readTChan

-- | writeTChanIO
writeTChanIO :: TChan a -> a -> IO ()
writeTChanIO tchan a = atomically $ writeTChan tchan a
