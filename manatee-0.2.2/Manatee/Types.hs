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

{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable, TypeSynonymInstances, RankNTypes, FlexibleInstances, GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Manatee.Types where

import Control.Applicative hiding (empty)
import Control.Concurrent.STM 
import DBus.Client hiding (Signal)
import Data.Binary
import Data.DeriveTH
import Data.Function
import Data.Map (Map)
import Data.Ord
import Data.Sequence (Seq)
import Data.Set
import Data.Text.Lazy (Text)
import Graphics.UI.Gtk hiding (on, get, Action, Statusbar, statusbarNew, Window, Frame, frameNew, Plug, Tooltip, WindowState)
import Manatee.Core.Types
import Manatee.Toolkit.Data.ListZipper hiding (length, delete, get)
import Manatee.Toolkit.Data.SetList
import Manatee.Toolkit.Widget.NotebookTab
import Manatee.UI.FocusNotifier
import Manatee.UI.Frame
import System.Posix.Types (ProcessID)
import Text.Printf

-- | Environment.
data Environment = 
    Environment {envFrame                  :: Frame
                ,envDaemonClient           :: Client
                ,envWindowList             :: TVar WindowList
                ,envWindowNodeList         :: TVar WindowNodeList
                ,envTabbar                 :: TVar Tabbar
                ,envTabbarSelect           :: TVar TabbarSelect
                ,envBufferList             :: TVar BufferList
                ,envSignalBoxList          :: TVar SignalBoxList
                ,envPageIdCounter          :: TVar PageId
                ,envSignalBoxIdCounter     :: TVar SignalBoxId
                ,envFocusNotifierList      :: TVar FocusNotifierList
                ,envTabCloseHistory        :: TVar TabCloseHistory
                ,envBufferHistory          :: TVar [BufferHistory]
                }

-- | Tab close history for undo close action.
data TabCloseHistory = 
    TabCloseHistory [(PageModeName, PageType, PagePath)]
                    deriving Show

-- | Keymap
-- | The design of `Keymap` is do a friendly, easy keymap interface.
type Keymap = Map Text Action

-- | Action just wrap to taken polymorphism function.
-- It won't store polymorphism function.
-- You can use 'Action' wrap any polymorphism functions that
-- input argument is instance of ActionInputArgs and 
-- output argument is instance of ActionOutputArgs.
data Action = forall a b . (ActionInputArgs a, ActionOutputArgs b) => 
              Action {actionFun :: a -> IO b}

-- | ActionInputArgs is class to implement polymorphism keymap function.
--
-- Example, you have functions: 
--
--      foo :: (Arg1, (Arg2, Arg3)) -> IO () 
--      bar :: (Arg1, (Arg2, (Arg3, Arg4))) -> IO ()
--
-- you can write your keymap with below:
--
--      keymap :: Keymap
--      keymap = M.fromList
--               ["M-1" ==> foo
--               ,"M-2" ==> bar]
--
-- See? It's so flexible.
class ActionInputArgs a where
    envGet :: Environment -> IO a

-- | ActionOutputArgs is class to implement polymorphism result. 
-- This class is very useful when you want transform pure function to `Keymap`.
-- You can return any result that instance of `ActionOutputArgs`,
-- then system will write those value back to `Environment`.
-- If you want update those value (such as TVar) yourself, just return `()`.
class ActionOutputArgs b where
    envPut :: Environment -> b -> IO ()

instance ActionInputArgs () where
    envGet _ = return ()

instance (ActionInputArgs a, ActionInputArgs b) => ActionInputArgs (a, b) where
    envGet env = liftA2 (,) (envGet env) (envGet env)

instance ActionOutputArgs () where
    envPut _ _ = return ()

instance (ActionOutputArgs a, ActionOutputArgs b) => ActionOutputArgs (a, b) where
    envPut env (a,b) = envPut env a >> envPut env b

-- | 'SignalBox' build when create socket.
-- Pick info when receive dbus signal.
-- Delete after create new page.
data SignalBox =
    SignalBox {signalBoxId        :: SignalBoxId
              ,signalBoxUIFrame   :: UIFrame
              ,signalBoxWindowId  :: WindowId
              }

instance Eq SignalBox where
    (==) = (==) `on` signalBoxId

instance Ord SignalBox where
    compare = comparing signalBoxId

type SignalBoxList = Set SignalBox

-- | WindowNode containe window node.
data WindowNode =
    WindowNode {windowNodeId                :: WindowNodeId
               ,windowNodePaned             :: Paned
               ,windowNodeParentId          :: TVar (Maybe WindowNodeId)
               ,windowNodeChildLeftId       :: TVar (Maybe WindowNodeId)
               ,windowNodeChildRightId      :: TVar (Maybe WindowNodeId)
               ,windowNodeType              :: TVar WindowNodeType
               ,windowNodeDirection         :: WindowNodeDirection}

type WindowNodeId = Int

data WindowNodeType = TNodeLeft
                    | TNodeRight
                    | TNodeRoot
                      deriving (Eq, Show, Read)

data WindowNodeDirection = DVertical
                         | DHortizontal
                           deriving (Eq, Show, Read)

data ZoomDirection = ZUp
                   | ZDown
                   | ZLeft
                   | ZRight
                     deriving (Eq, Show, Read)

instance Eq WindowNode where
    (==) = (==) `on` windowNodeId

instance Ord WindowNode where
    compare = comparing windowNodeId

type WindowNodeList = SetList WindowNode

-- | Window contain GtkNotebook for contain tabs.
data Window =
    Window {windowNode     :: WindowNode
           ,windowNotebook :: Notebook}

instance Eq Window where
    (==) = (==) `on` windowNode

instance Ord Window where
    compare = comparing windowNode

instance Show Window where
    show = printf "<#Window %s>" . show . windowNodeId . windowNode

type WindowList = ListZipper Window


data WindowListSelectDirection = VLeft
                               | VRight 
                                 deriving (Eq, Show, Read)
type WindowId = WindowNodeId

type WindowListTuple = (WindowList, WindowNodeList)

type WindowNodeArgs = (WindowNodeList, Container)

type WindowArgs = (WindowList, WindowNodeList, Container)

type WindowNodeAttr = (Maybe WindowNodeId
                      ,Maybe WindowNodeId
                      ,Maybe WindowNodeId
                      ,Maybe WindowNodeId
                      ,WindowNodeType
                      ,WindowNodeDirection)

-- | Tabbar
-- Contain all tab in windows.
-- For update tab information in windows.
-- PageId for synchronization with different window.
-- Tabbar corresponding Notebook status.
newtype Tabbar = 
    Tabbar (Map (WindowId, PageModeName) (Seq Tab)) 
           deriving Show

data Tab = 
    Tab {tabProcessId   :: ProcessID
        ,tabPageId      :: PageId
        ,tabSignalBoxId :: SignalBoxId
        ,tabSocketId    :: PageSocketId
        ,tabPlugId      :: PagePlugId
        ,tabUIFrame     :: UIFrame}
    deriving Show

-- | Select tab.
data TabbarSelect = 
    TabbarSelect (Map PageModeName Int)
                 deriving Show

-- | UIFrame.
data UIFrame =
    UIFrame {uiFrameBox                 :: VBox           -- box for contain `PageView'
            ,uiFrameNotebookTab         :: NotebookTab    -- notebook tab
            }
instance Show UIFrame where
  show _ = "UIFrame"

data EnvironmentState =
    EnvironmentState {envStateWindowNodeList    :: WindowNodeStateList
                     ,envStateWindowList        :: WindowStateList
                     ,envStateTabbar            :: TabbarState
                     ,envStateBufferList        :: BufferListState
                     }

data WindowNodeStateList = 
    WindowNodeStateList {wnslCounter    :: Int
                        ,wnslSet        :: Set WindowNodeState}

data WindowStateList = 
    WindowStateList {wslLeft    :: [WindowState]
                    ,wslRight   :: [WindowState]}

data WindowNodeState =
    WindowNodeState {windowNodeStateId          :: WindowNodeId
                    ,windowNodeStateParentId    :: Maybe WindowNodeId
                    ,windowNodeStateChildLeftId :: Maybe WindowNodeId
                    ,windowNodeStateChildRightId:: Maybe WindowNodeId
                    ,windowNodeStateType        :: WindowNodeType
                    ,windowNodeStateDirection   :: WindowNodeDirection
                    }

instance Eq WindowNodeState where
    (==) = (==) `on` windowNodeStateId

instance Ord WindowNodeState where
    compare = comparing windowNodeStateId
    
data WindowState = 
    WindowState {windowStateId         :: WindowId
                ,windowStateSize       :: (Int, Int)
                }

type TabSelectIndex = Int
     
data TabbarState =
    TabbarState {tabbarStateList                :: [((WindowId, PageModeName), [TabState], TabSelectIndex)]
                ,tabbarStatePageIdCounter       :: Int
                ,tabbarStateSignalBoxIdCounter  :: Int
                }

data TabState =     
    TabState {tabStatePageId            :: PageId
             ,tabStateSignalBoxId       :: SignalBoxId
             }

data BufferListState =
    BufferListState {bufferListStateForegroundPages  :: [(PageModeName, [PageId])]
                    ,bufferListStateBackgroundPages  :: [(PageModeName, [PageId])]}

$(derive makeBinary ''EnvironmentState)
$(derive makeBinary ''WindowNodeState)
$(derive makeBinary ''WindowNodeStateList)
$(derive makeBinary ''WindowState)
$(derive makeBinary ''WindowNodeType)
$(derive makeBinary ''WindowNodeDirection)
$(derive makeBinary ''WindowStateList)
$(derive makeBinary ''BufferListState)
$(derive makeBinary ''TabState)
$(derive makeBinary ''TabbarState)
