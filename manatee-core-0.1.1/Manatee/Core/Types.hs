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
module Manatee.Core.Types where

import Control.Concurrent.STM 
import DBus.Client hiding (Signal)
import DBus.Types
import Data.Binary
import Data.Derive.Ord
import Data.Derive.Read
import Data.DeriveTH
import Data.Function
import Data.Map (Map)
import Data.Ord
import Data.Sequence (Seq)
import Data.Set
import Data.Text.Lazy (Text)
import Data.Typeable
import GHC (HValue)
import Graphics.UI.Gtk hiding (on, get, Action, Statusbar, statusbarNew, Window, Frame, frameNew, Plug)
import Graphics.UI.Gtk.Gdk.SerializedEvent
import Language.Haskell.TH
import Manatee.Core.TH
import Manatee.Toolkit.Gtk.ScrolledWindow
import Manatee.Toolkit.Gtk.Struct
import Manatee.Toolkit.Widget.Interactivebar
import Manatee.Toolkit.Widget.Outputbar
import Manatee.Toolkit.Widget.Plug
import Manatee.Toolkit.Widget.CompletionWindow
import Manatee.Toolkit.Widget.KeymapWindow
import Manatee.Toolkit.Widget.Statusbar
import System.Posix.Types (ProcessID)

import qualified Graphics.UI.Gtk as Gtk

-- | Page is widget container for render view.
data Page = Page {pageId                :: PageId
                 ,pageSignalBoxId       :: SignalBoxId
                 ,pageType              :: PageType
                 ,pagePlug              :: Plug
                 ,pageView              :: PageViewWrap
                 }

type PagePlug = Plug

instance Eq Page where
    (==) = (==) `on` (plugId . pagePlug)

instance Ord Page where
    compare = comparing (plugId . pagePlug)

-- | PageList
-- Contain pages (different view for same buffer) in child process.
-- Build index for looking for corresponding page.
type PageList = Set Page

-- | PageType for different type page.
type PageType = String

data PageTypeRule = 
    PageTypeRule (Map PageType String)
    deriving (Show, Read, Eq, Ord, Typeable)

data FileOpenRule =
    FileOpenRule (Map FileMatch [(FileOpenName, PageType, FilePrefix)])
                 deriving (Show, Read, Eq, Ord, Typeable)

type FileOpenName = String
type FilePrefix = String

data PageModeRule =
    PageModeRule (Map PageType (Either 
                                     PageModeName 
                                     (Map String PageModeName)))
                 deriving (Show, Read, Eq, Ord, Typeable)

data PageModeDuplicateList = 
    PageModeDuplicateList [PageModeName]
                 deriving (Show, Read, Eq, Ord, Typeable)

data FileMatch = ContentTypeMatch String
               | RegexpMatch String
                 deriving (Show, Read, Eq, Ord, Typeable)

data ExtensionGloalKeymap = 
    ExtensionGloalKeymap (Map String (String, (String, String, [String])))
                         deriving (Show, Read, Eq, Ord, Typeable)

data WelcomeApplication = 
    WelcomeApplication (Map (String, FilePath) -- application name and image path
                            (PageType, -- page type
                             String,   -- page path
                             [String]) -- options
                       ) deriving (Show, Read, Eq, Ord, Typeable)

-- | BufferList
newtype BufferList = 
    BufferList (Map PageModeName (Seq Buffer)) 
               deriving Show

data Buffer =
    Buffer {bufferProcessId     :: ProcessID
           ,bufferPageId        :: PageId
           ,bufferPageType      :: PageType
           ,bufferPath          :: String
           ,bufferName          :: String
           }
    deriving (Show, Read, Eq, Ord)

data BufferInfo = 
    BufferInfo {bufferInfoMode  :: PageModeName
               ,bufferInfoPath  :: PagePath
               ,bufferInfoName  :: String
               ,bufferInfoId    :: PageId} 
    deriving (Show, Read, Eq, Ord, Typeable)

data BufferHistory =
    BufferHistory {bufferHistoryMode    :: PageModeName
                  ,bufferHistoryType    :: PageType
                  ,bufferHistoryPath    :: PagePath}
    deriving (Show, Read, Eq, Ord, Typeable)

data BrowseHistoryList = 
    BrowseHistoryList (Map String String)
    deriving (Show, Read, Eq, Ord, Typeable)

type BufferInfoList = [BufferInfo]
type BufferHistoryList = [BufferHistory]

-- | PageMode
type PageModeName   = String
type PageModeRegexp = String

data PageMode = 
    PageMode {pageModeName      :: PageModeName
             ,pageModeRegexp    :: PageModeRegexp
             ,pageModeKeymap    :: PageViewKeymap
             ,pageModeLoad      :: forall a . PageView a => a -> IO ()}

-- | PageBuffer
class Typeable a => PageBuffer a where
    pageBufferGetName           :: a -> IO String
    pageBufferSetName           :: a -> String -> IO ()
    pageBufferClient            :: a -> Client
    pageBufferCreateView        :: a -> PagePlugId -> IO PageViewWrap
    pageBufferMode              :: a -> PageMode
    pageBufferPackageName       :: a -> IO String
    pageBufferWriteState        :: a -> FilePath -> IO ()
    pageBufferWriteState _ _ = return ()
    pageBufferReadState         :: a -> FilePath -> IO ()
    pageBufferReadState _ _ = return ()

data PageBufferWrap = forall a . PageBuffer a => PageBufferWrap a

-- | Page buffer new function.
type PageBufferNewFun = FilePath -> [String] -> Client -> PageId -> CustomizeWrap -> IO PageBufferWrap 

-- | Interactive type.
data InteractiveType = IDefault
                     | IChar
                     | IDir
                     | IFile
                     | INum
                     | IString
                     -- | ICustomize (String -> [String])  -- customize interactive type
                       deriving (Show, Eq, Ord)

-- | Page frame.
data PageFrame =
    PageFrame {pageFrameBox                 :: VBox           -- box for contain `PageView'
              ,pageFrameInteractivebar      :: Interactivebar -- interactivebar
              ,pageFrameFrame               :: Gtk.Frame      -- frame
              ,pageFrameScrolledWindow      :: ScrolledWindow -- scrolled window
              ,pageFrameOutputbar           :: Outputbar      -- outputbar for display message
              ,pageFrameStatusbar           :: Statusbar      -- statusbar
               -- Completion window.
              ,pageFrameAutoCompletion      :: TVar Bool        -- whether auto completion? 
              ,pageFrameCompletionWindow    :: CompletionWindow -- completion window
               -- Interactive arguments.
              ,pageFrameInteractiveArgs     :: TVar [(InteractiveType, String, String)] -- arguments for interactive input
              ,pageFrameInteractiveType     :: TVar InteractiveType    -- interactive type
              ,pageFrameInteractiveResult   :: TVar [String]           -- interactive result
              ,pageFrameInteractiveFun      :: TVar ([String] -> IO ()) -- interactive function
              ,pageFrameKeymapWindow        :: KeymapWindow            -- keymap window
              }

-- | PageView class.
class Typeable a => PageView a where
    pageViewBuffer              :: a -> PageBufferWrap
    pageViewPlugId              :: a -> TVar PagePlugId
    pageViewFrame               :: a -> PageFrame
    pageViewLocalKeymap         :: a -> Map Text Text
    pageViewLocalCommandMap     :: a -> Map Text (a -> IO ())
    pageViewFocus               :: a -> IO ()       
    pageViewPropagateWidget     :: a -> Widget
    pageViewSaveState           :: a -> IO ()
    pageViewSaveState _ = return ()
    pageViewWriteState          :: a -> FilePath -> IO ()
    pageViewWriteState _ _ = return ()
    pageViewReadState           :: a -> FilePath -> IO ()
    pageViewReadState _ _ = return ()
    pageViewRestoreState        :: a -> IO ()
    pageViewRestoreState _ = return ()
    pageViewCopy                :: a -> IO Bool
    pageViewCopy _ = return False
    pageViewCut                 :: a -> IO Bool
    pageViewCut _ = return False
    pageViewPaste               :: a -> IO Bool
    pageViewPaste _ = return False
    pageViewScrollToTop         :: a -> IO ()
    pageViewScrollToTop = 
        scrolledWindowScrollToTop . pageFrameScrolledWindow . pageViewFrame
    pageViewScrollToBottom      :: a -> IO ()
    pageViewScrollToBottom =
        scrolledWindowScrollToBottom . pageFrameScrolledWindow . pageViewFrame
    pageViewScrollVerticalPage  :: Bool -> a -> IO ()
    pageViewScrollVerticalPage isDown a =
        scrolledWindowScrollVerticalPage isDown (pageFrameScrolledWindow $ pageViewFrame a)
    pageViewScrollVerticalStep  :: Bool -> a -> IO ()
    pageViewScrollVerticalStep isDown a = 
        scrolledWindowScrollVerticalStep isDown (pageFrameScrolledWindow $ pageViewFrame a)
    pageViewScrollToLeft        :: a -> IO ()
    pageViewScrollToLeft = 
        scrolledWindowScrollToLeft . pageFrameScrolledWindow . pageViewFrame
    pageViewScrollToRight       :: a -> IO ()
    pageViewScrollToRight =
        scrolledWindowScrollToRight . pageFrameScrolledWindow . pageViewFrame
    pageViewScrollHorizontalPage:: Bool -> a -> IO ()
    pageViewScrollHorizontalPage isDown a =
        scrolledWindowScrollHorizontalPage isDown (pageFrameScrolledWindow $ pageViewFrame a)
    pageViewScrollHorizontalStep:: Bool -> a -> IO ()
    pageViewScrollHorizontalStep isDown a = 
        scrolledWindowScrollHorizontalStep isDown (pageFrameScrolledWindow $ pageViewFrame a)

data PageViewWrap = forall a . PageView a => PageViewWrap a

type PageViewKeymap = forall a . PageView a => Map Text (a -> IO ())
type InteractiveKeymap = PageViewKeymap

class Typeable a => Customize a where
  customizeConfigFile   :: a -> FilePath
  customizeLoad         :: a -> [(String, HValue -> IO ())]

data CustomizeWrap = forall a . Customize a => CustomizeWrap a

type CustomizeNewFun = IO CustomizeWrap

-- | DBus
type PageId             = Int
type PageSocketId       = GWindowId
type PagePlugId         = GWindowId
type PagePath           = String
type SignalBoxId        = Int
type InteractiveString  = (PagePlugId, String)
type InteractiveResult  = [String]
type GlobalKeymapResult = (Rectangle, [(Text, Text)])

$(derive makeOrd ''Color)
$(derive makeRead ''Color)
$(derive makeRead ''Rectangle)

-- | Variant instance for serialize value to DBus system.
deriveVariable (conT ''ProcessID)
deriveVariable (conT ''GWindowId)
deriveVariable (conT ''Rectangle)
deriveVariable (conT ''Color)
deriveVariable (conT ''Maybe `appT` conT ''Color)
deriveVariable (conT ''Int)
deriveVariable (conT ''Maybe `appT` conT ''Char)
deriveVariable (conT ''Maybe `appT` conT ''Int)
deriveVariable (conT ''Maybe `appT` conT ''FilePath)
deriveVariable (conT ''SerializedEvent)
deriveVariable (conT ''BufferInfoList)
deriveVariable (conT ''BufferHistoryList)
deriveVariable (conT ''Maybe `appT` conT ''Point)
deriveVariable (conT ''InteractiveString)
deriveVariable (conT ''InteractiveResult)
deriveVariable (conT ''GlobalKeymapResult)

data DaemonMember = NewRenderPageConfirm   
                  | NewRenderProcessConfirm
                  | RenderProcessExit
                  | RenderProcessExitConfirm
                  | RenderProcessException 
                  | NewTab
                  | LocalInteractivebarExit
                  | SynchronizationPathName
                  | ChangeTabName
                  | SwitchBuffer
                  | LocalInteractiveReturn
                  | GlobalInteractiveReturn
                  | Ping
                    deriving (Show, Eq, Ord)

data DaemonSignalArgs = NewRenderPageConfirmArgs PageId PageType SignalBoxId PagePlugId ProcessID PageModeName String Bool Bool
                      | NewRenderProcessConfirmArgs PageId PageType ProcessID PageModeName String
                      | RenderProcessExitArgs PageId
                      | RenderProcessExitConfirmArgs PageId ProcessID
                      | RenderProcessExceptionArgs PageId
                      | NewTabArgs PageType PagePath [String]
                      | LocalInteractivebarExitArgs
                      | SynchronizationPathNameArgs PageModeName PageId String
                      | ChangeTabNameArgs PageModeName PageId String
                      | SwitchBufferArgs PageModeName PageId
                      | LocalInteractiveReturnArgs [String]
                      | GlobalInteractiveReturnArgs [String]
                      | PingArgs ProcessID
                        deriving (Show, Eq, Ord)

data RenderMember = CloneRenderPage
                  | ReparentRenderPage
                  | FocusRenderPage
                  | PageViewKeyPress
                  | DestroyRenderPage
                  | ExitRenderProcess
                  | UpdateConfig
                  | InstallConfig
                  | Pong
                  | RestoreRenderPageState
                    deriving (Show, Eq, Ord)

data RenderSignalArgs = CloneRenderPageArgs PageId SignalBoxId
                      | ReparentRenderPageArgs PageId PagePlugId SignalBoxId
                      | FocusRenderPageArgs PagePlugId
                      | PageViewKeyPressArgs PagePlugId Text SerializedEvent
                      | DestroyRenderPageArgs PagePlugId
                      | ExitRenderProcessArgs PageId Bool
                      | UpdateConfigArgs
                      | InstallConfigArgs
                      | PongArgs
                      | RestoreRenderPageStateArgs PagePlugId Bool
                        deriving (Show, Eq, Ord)

data PongStatus = WaitPongMessage
                | GotPongMessage
                  deriving (Show, Eq, Ord)

data DaemonBroadcastMember = ExitDaemonProcess
                           deriving (Show, Eq, Ord)

data DaemonBroadcastSignalArgs = ExitDaemonProcessArgs
                                 deriving (Show, Eq, Ord)

data GenericDaemonMember = Generic
    deriving (Show, Eq, Ord)

data GenericDaemonSignalArgs = GenericArgs String [String]
    deriving (Show, Eq, Ord)

data SpawnProcessArgs = SpawnRenderProcessArgs PageId PageType PagePath [String] [SignalBoxId] Bool
                        deriving (Show, Eq, Ord, Read)

$(derive makeBinary ''BrowseHistoryList)
$(derive makeBinary ''PageTypeRule)
$(derive makeBinary ''FileMatch)
$(derive makeBinary ''FileOpenRule)
$(derive makeBinary ''PageModeRule)
$(derive makeBinary ''PageModeDuplicateList)
$(derive makeBinary ''ExtensionGloalKeymap)
$(derive makeBinary ''WelcomeApplication)
