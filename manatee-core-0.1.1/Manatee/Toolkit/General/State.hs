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

{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Manatee.Toolkit.General.State where

import Control.Concurrent.STM 
import Control.Monad.State
import Manatee.Toolkit.General.STM

-- | Like `modify`, except use Monad wrap update function. 
modifyM :: (Monad m, MonadTrans t, MonadState a (t m)) => (a -> m a) -> t m ()
modifyM f = do    
  oldState <- get
  newState <- lift $ f oldState
  put newState

-- | Like `modifyM`, 
-- but add two functions filter new state and return value.
-- This is handy when function return result including state.
-- Example, state is (a,b), and function return (a,b,c),
-- then you can write: "result <- modifyM_ f fst snd".
modifyM_ :: (Monad m, MonadTrans t, MonadState a1 (t m)) => (a1 -> m a) -> (a -> a1) -> (a -> a2) -> t m a2
modifyM_ f g h = do
  oldState <- get
  newState <- lift $ f oldState
  put $ g newState
  return $ h newState

-- | Like `modifyM_`, except don't return value.
modifyM' :: (Monad m, MonadTrans t, MonadState a1 (t m)) => (a1 -> m a) -> (a -> a1) -> t m ()
modifyM' f g = do
  oldState <- get
  newState <- lift $ f oldState
  put $ g newState

-- | Modify fst state of tuple.
modifyFst :: (MonadState (t, t1) m) => ((t, t1) -> t) -> m ()
modifyFst f = do
  oldState@(_, b) <- get
  put (f oldState, b)

-- | Modify snd state of tuple.
modifySnd :: (MonadState (t, t1) m) => ((t, t1) -> t1) -> m ()
modifySnd f = do
  oldState@(a, _) <- get
  put (a, f oldState)
  
-- | Like `modifyFst`, except use moand wrap function.
modifyFstM :: (Monad m, MonadTrans t1, MonadState (a, t) (t1 m)) => ((a, t) -> m a) -> t1 m ()
modifyFstM f = do
  oldState@(_, b) <- get
  a' <- lift $ f oldState
  put (a', b)

-- | Like `modifySnd`, except use moand wrap function.
modifySndM :: (Monad m, MonadTrans t1, MonadState (t, a) (t1 m)) => ((t, a) -> m a) -> t1 m ()
modifySndM f = do
  oldState@(a, _) <- get
  b' <- lift $ f oldState
  put (a, b')

-- | Like `get`, except use Monad wrap function.
getM :: (MonadState a (t m), MonadTrans t, Monad m) => (a -> m a1) -> t m a1
getM f = get >>= lift . f

-- | Like `runStateT`, just reverse arguments order.
runStateT_ :: b -> StateT b m a -> m (a, b)
runStateT_ = flip runStateT

-- | Like `runStateT_`, but just return last argument.
-- It's useful that you just want use runStateT wrap *one* state.
runStateT' :: (Functor f) => b -> StateT b f a -> f b
runStateT' b f = fmap snd $ runStateT_ b f

-- | Use runStateT' wrap TVar.
runTVarStateT :: TVar a -> (a -> StateT a IO b) -> IO ()
runTVarStateT tvar fun = do
  state <- readTVarIO tvar
  newState <- runStateT' state (fun state)
  writeTVarIO tvar newState

-- | Like `runTVarStateT`, but handle tuple TVar.
runTVarTupeStateT :: (TVar a, TVar b) -> (a -> b -> StateT (a, b) IO c) -> IO ()
runTVarTupeStateT (tvarA, tvarB) fun = do
  stateA <- readTVarIO tvarA
  stateB <- readTVarIO tvarB
  (newStateA, newStateB) <- runStateT' (stateA, stateB) (fun stateA stateB)
  writeTVarIO tvarA newStateA
  writeTVarIO tvarB newStateB

