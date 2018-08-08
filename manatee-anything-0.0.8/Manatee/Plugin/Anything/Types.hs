-- Author:     Andy Stewart <lazycat.manatee@gmail.com>
-- Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
-- 
-- Copyright (C) 2010 Andy Stewart, all rights reserved.
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

{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
module Manatee.Plugin.Anything.Types where

import Control.Applicative hiding (empty)
import Control.Concurrent
import Control.Concurrent.STM 
import DBus.Client hiding (Signal)
import Data.Map (Map)
import Data.Text.Lazy (Text)
import Data.Typeable
import Graphics.UI.Gtk hiding (get)
import Manatee.Core.Types
import Manatee.Plugin.Anything.TitleWidget

-- | Anything
type AnythingName               = String
type AnythingInput              = String
type AnythingCompletion         = String
type AnythingList               = Map AnythingName Anything
type AnythingFilterRule         = AnythingInput -> String -> Bool
type AnythingCompletionRule     = AnythingInput -> AnythingCompletion
type AnythingColumnTitle        = String
type AnythingColumnFun          = AnythingCandidateWrap -> String
type AnythingCommandName        = String
type AnythingAction             = Client -> IO ()
type AnythingCommandFun         = AnythingInput 
                                -> AnythingCandidateWrap 
                                -> AnythingInteractiveType 
                                -> IO [(AnythingCommandName, AnythingAction)]
type AnythingSearchCache = 
    Map String (Either 
                (MVar [AnythingCandidateWrap], ThreadId, ThreadId)
                HandlerId)

data FocusNode = FocusFirstNode                       
               | FocusLastNode
                 deriving (Show, Ord, Eq)

class Typeable a => AnythingCandidate a where
    candidateCommandName       :: a -> String   -- name for command execute
    candidateFilterName        :: a -> String   -- name for input filter search
    candidateCompletionName    :: a -> AnythingInput -> String   -- name for completion search
    candidateExpandName        :: a -> String -> IO String -- name for expand

data AnythingCandidateWrap = forall candidate . AnythingCandidate candidate => 
                              AnythingCandidateWrap {acwCandidate :: candidate}

data AnythingSearch = forall candidate . AnythingCandidate candidate =>
                    AnythingSearch {anythingSearchFun :: AnythingInput -> Client -> IO [candidate]}

data Anything = 
    Anything {anythingColumnTitle       :: [AnythingColumnTitle] -- anything candidate column title
             ,anythingColumnFun         :: [AnythingColumnFun]   -- anything candidate column function
             ,anythingSearch            :: AnythingSearch        -- function for get search list
             ,anythingFilterRule        :: AnythingFilterRule    -- rule for filter search list
             ,anythingCompletionRule    :: AnythingCompletionRule -- rule to get *highlight* completion
             ,anythingInputDepend       :: Bool                   -- search candidate whether depend on user input?
             ,anythingCommandFun        :: AnythingCommandFun    -- command list depend on input and search candidates
             ,anythingCalculateDelay    :: Int -- delay millisecond before do really calcuate
             } 

instance AnythingCandidate String where
    candidateCommandName        = id
    candidateFilterName         = id
    candidateCompletionName a _ = a
    candidateExpandName a _     = return a

-- | AnythingView
data AnythingView =
    AnythingView {avNotebook                   :: Notebook
                 ,avCandidateBox               :: VBox
                 ,avCandidateScrolledWindow    :: ScrolledWindow
                 ,avCandidateTab               :: TVar AnythingCandidateTab
                 ,avCommandTab                 :: AnythingCommandTab
                 ,avCurrentCandidateIndex      :: TVar Int
                 ,avInteractiveType            :: TVar AnythingInteractiveType
                 }

data AnythingCandidateView =
    AnythingCandidateView {acavTreeView         :: TreeView
                          ,acavScrolledWindow   :: ScrolledWindow
                          ,acavListStore        :: AnythingCandidateStore
                          ,acavTitleWidget      :: TitleWidget
                          }

data AnythingCommandView =
    AnythingCommandView {acovTreeView         :: TreeView
                        ,acovListStore        :: AnythingCommandStore}

type AnythingCandidateStore = ListStore AnythingCandidateWrap
type AnythingCommandStore   = ListStore String
type AnythingCandidateTab   = (Map Int ((AnythingName, Int), AnythingCandidateView))
type AnythingCommandTab     = AnythingCommandView

-- | AnythingViewAction
data AnythingViewEnvironment = 
    AnythingViewEnvironment {aeView         :: AnythingView
                            ,aeClient       :: Client
                            ,aeCache        :: TVar AnythingSearchCache
                            ,aeInput        :: AnythingViewInput
                            ,aeKeyPressId   :: Int} 

data AnythingViewInput = 
    AnythingViewInput {aviAllText       :: String
                      ,aviUnselectText  :: String}

class AnythingViewActionArgs a where
    anythingViewExecute :: AnythingViewEnvironment -> IO a
    
data AnythingViewAction = forall a . AnythingViewActionArgs a =>
                        AnythingViewAction {anythingViewActionFun   :: a -> IO ()}

type AnythingViewKeymap = Map Text AnythingViewAction

instance (AnythingViewActionArgs a, AnythingViewActionArgs b) => AnythingViewActionArgs (a, b) where
    anythingViewExecute env = liftA2 (,) (anythingViewExecute env) (anythingViewExecute env)

instance AnythingViewActionArgs AnythingView where
    anythingViewExecute env = return $ aeView env 

instance AnythingViewActionArgs Client where
    anythingViewExecute env = return $ aeClient env 

instance AnythingViewActionArgs (TVar AnythingSearchCache) where
    anythingViewExecute env = return $ aeCache env 

instance AnythingViewActionArgs AnythingViewInput where
    anythingViewExecute env = return $ aeInput env 

instance AnythingViewActionArgs Int where
    anythingViewExecute env = return $ aeKeyPressId env 

