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

{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Manatee.Plugin.Anything.AnythingView where

import Control.Applicative hiding (empty)
import Control.Concurrent.STM 
import Control.Monad
import DBus.Client hiding (Signal)
import Data.List
import Data.Maybe
import GHC.Conc
import Graphics.UI.Gtk hiding (get)
import Manatee.Core.DBus
import Manatee.Core.Types 
import Manatee.Plugin.Anything.AnythingList
import Manatee.Plugin.Anything.TitleWidget
import Manatee.Plugin.Anything.Types
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.List
import Manatee.Toolkit.General.Map
import Manatee.Toolkit.General.Maybe
import Manatee.Toolkit.General.STM
import Manatee.Toolkit.Gtk.Box
import Manatee.Toolkit.Gtk.Concurrent
import Manatee.Toolkit.Gtk.Container
import Manatee.Toolkit.Gtk.Gtk
import Manatee.Toolkit.Gtk.ModelView

import qualified Data.Map as M
import qualified Control.Exception as Exc

-- | The max limit of rows in candidate view.
anythingCandidateViewLimit :: Int
anythingCandidateViewLimit = 10

-- | Default cell height, use when cell height from tree view is invalid.
anythingViewDefaultCellHeight :: Int
anythingViewDefaultCellHeight = 24

-- | Candidate tab index in notebook.
anythingCandidateTabIndex :: Int
anythingCandidateTabIndex = 0

-- | Command tab index in notebook.
anythingCommandTabIndex :: Int
anythingCommandTabIndex = 1

-- | Create new anything view.
anythingViewNew :: AnythingInteractiveType -> [String] -> IO AnythingView
anythingViewNew iType names = do
  -- Create notebook to contain views.
  notebook <- notebookNew
  notebookSetTabPos notebook PosBottom
  notebookSetShowTabs notebook False

  -- Create candidate box.
  candidateBox <- vBoxNew False 0

  -- Create candidate tab.
  candidateTab <- (<=<) newTVarIO anythingCandidateTabNew names

  -- Create candidate index.
  currentCandidateIndex <- newTVarIO 0

  -- Create candidate scrolled window.
  candidateScrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy candidateScrolledWindow PolicyNever PolicyNever
  scrolledWindowAddWithViewport candidateScrolledWindow candidateBox
  notebookInsertPage notebook candidateScrolledWindow "Candidate Tab" anythingCandidateTabIndex
  
  -- Create command view.
  (commandView, commandStore, commandWin) <- anythingViewCommandTabNew "Command"
  notebookInsertPage notebook commandWin "Command Tab" anythingCommandTabIndex
  let commandTab = AnythingCommandView commandView commandStore

  -- Create interactive type.
  typ <- newTVarIO iType

  -- Connect components.
  widgetShowAll notebook

  -- Create anything view.
  return $ AnythingView notebook candidateBox candidateScrolledWindow 
                        candidateTab commandTab currentCandidateIndex typ

-- | Create new candidate tab.
anythingCandidateTabNew :: [String] -> IO AnythingCandidateTab
anythingCandidateTabNew names = do
  let anythings = mapMaybe (`M.lookup` anythingList) names
      columnInfoList = map (\x -> zip (anythingColumnTitle x) (anythingColumnFun x)) anythings 
  liftM M.fromList $ zipWithIndexM columnInfoList $ \ columnInfo index -> do
        let name = fst $ head columnInfo
        (view, scrolledWin, store, titleWidget) <- anythingViewCandidateTabNew columnInfo
        return (index, ((name, 0), AnythingCandidateView view scrolledWin store titleWidget))

-- | Create anything sub-view.
anythingViewCandidateTabNew :: [(AnythingColumnTitle, AnythingColumnFun)] 
                            ->  IO (TreeView, ScrolledWindow, AnythingCandidateStore, TitleWidget)
anythingViewCandidateTabNew columnInfo = do
  -- Create list store.
  store <- listStoreNew []

  -- Create tree view.
  treeView  <- treeViewNewWithModel store
  selection <- treeViewGetSelection treeView
  treeSelectionSetMode selection SelectionBrowse -- set browse selection mode 
  
  -- Create scrolled window.
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyNever PolicyAutomatic -- just show vertically adjustment
  scrolledWindow `containerAdd` treeView

  -- Create title widget.
  titleWidget <- titleWidgetNew Nothing

  -- Add column.
  zipWithIndexM_ columnInfo $ \ (columnTitle, columnFun) index -> do
       -- Create tree view column
       treeViewColumn <- treeViewColumnNew
       treeViewAppendColumn treeView treeViewColumn

       -- Set title.
       if index == 0
          -- Use titleWidget if current column is first one.
          then do
            set treeViewColumn [treeViewColumnWidget := Just (titleWidgetBox titleWidget)]
            titleWidgetSetTitle titleWidget columnTitle 0
          else
            set treeViewColumn [treeViewColumnTitle := columnTitle]

       -- Add candidate renderer.
       cellRenderer <- cellRendererTextNew
       treeViewColumnPackStart treeViewColumn cellRenderer True -- add cell renderer to tree view column
       cellLayoutSetAttributes treeViewColumn cellRenderer store $ \candidates -> -- set cell attributes
           [cellText := columnFun candidates]

  return (treeView, scrolledWindow, store, titleWidget)

-- | Create anything sub-view.
anythingViewCommandTabNew :: String -> IO (TreeView, AnythingCommandStore, ScrolledWindow)
anythingViewCommandTabNew name = do
  -- Create list store.
  store <- listStoreNew []

  -- Create tree view.
  treeView <- treeViewNewWithModel store

  -- Create scrolled window.
  scrolledWindow <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolledWindow PolicyNever PolicyAutomatic -- just show vertically adjustment
  scrolledWindow `containerAdd` treeView

  -- Create tree view column.
  treeViewColumn <- treeViewColumnNew
  set treeViewColumn [ treeViewColumnTitle := name]
  treeViewAppendColumn treeView treeViewColumn

  -- Create cell renderer.
  cellRenderer <- cellRendererTextNew
  treeViewColumnPackStart treeViewColumn cellRenderer True -- add cell renderer to tree view column
  cellLayoutSetAttributes treeViewColumn cellRenderer store $ \text -> -- set cell attributes
      [ cellText := text ]

  return (treeView, store, scrolledWindow)

-- | Add candidates to view.
anythingViewSetCandidates :: AnythingView -> Client -> AnythingName -> [AnythingCandidateWrap] -> 
                             AnythingInput -> AnythingCompletionRule -> AnythingKeyPressId -> IO ()
anythingViewSetCandidates view client name candidates input rule keyPressId = do
  candidateTab <- readTVarIO $ avCandidateTab view
  findMinMatch candidateTab (\ _ v -> (fst . fst) v == name) 
           ?>= \ (index, ((candidateName, _)
                         ,candidateView@(AnythingCandidateView {acavListStore   = listStore
                                                               ,acavTitleWidget = titleWidget}))) -> do
    -- Get candidate number.
    let candidateNumber = length candidates

    -- Clear list data first.
    listStoreClear listStore                                                                                

    -- Update candidates.
    unless (null candidates) $ do
         -- Add candidates.
         forM_ candidates (listStoreAppend listStore)

         -- Set match number.
         titleWidgetSetTitle titleWidget name candidateNumber

    -- Update candidate number in 'avCandidateTab'
    modifyTVarIO (avCandidateTab view) (M.insert index ((candidateName, length candidates), candidateView))

    -- Adjust candidates.
    readTVarIO (avCandidateTab view) >>= \ candidateMap -> do
      -- Get hide list and redraw list.
      let (hideList, redrawList) =
              partition (\ (_, ((_, num), _)) -> num == 0) $ M.toList candidateMap

      -- Hide candidate view that haven't search result.
      forM_ hideList $ \ (_, (_, AnythingCandidateView {acavScrolledWindow = scrolledWin})) -> do
        widgetHideAll scrolledWin
        containerTryRemove (avCandidateBox view) scrolledWin

      -- Redraw candidate view.
      zipWithIndexM_ redrawList $ \ (_, ((name, num)
                                        ,AnythingCandidateView {acavTreeView       = treeView 
                                                               ,acavScrolledWindow = scrolledWin})) index -> do
          -- Show widget first.
          widgetShowAll scrolledWin

          -- Adjust packing status.
          -- Grow candidate when reach last one.
          let lastCandidate = index == length redrawList - 1
              packing = if lastCandidate then PackGrow else PackNatural
          boxTryPack (avCandidateBox view) scrolledWin packing (Just index) Nothing

          -- Adjust height.
          adjustHeight <- 
              if lastCandidate 
                 -- Grow max height when reach last one.
                 then screenHeight
                 -- Otherwise adjust feat value.
                 else do 
                   cellHeight   <- anythingViewGetCellHeight treeView
                   headerHeight <- treeViewGetHeaderHeight treeView
                   return $ headerHeight + (if num < anythingCandidateViewLimit
                                            then num
                                            else anythingCandidateViewLimit) * cellHeight
          widgetSetSizeRequest treeView (-1) adjustHeight

          -- Send AnythingViewOutput signal when reach first candidate.
          when (index == 0 && name == candidateName) $ do
                 let completions = map (\ (AnythingCandidateWrap a) -> candidateCompletionName a input) candidates
                     completion = anythingViewGetCompletion input completions rule
                 boxHeight <- anythingViewGetBoxHeight view 
                 mkDaemonSignal client AnythingViewOutput (AnythingViewOutputArgs input completion boxHeight keyPressId)

    -- Update selected information.
    candidateMap <- readTVarIO $ avCandidateTab view
    find (\ (_, ((_, count), _)) -> count > 0) (M.toList candidateMap) 
      ?>= \ (index, (_, AnythingCandidateView {acavTreeView = treeView})) -> do
          -- Update current candidate index.
          writeTVarIO (avCurrentCandidateIndex view) index

          -- Just focus first node if current treeView not selected.
          -- Otherwise keep user's current selected.
          treePath <- treeViewGetSelectedPath treeView
          when (isNothing treePath) $ do
              -- Focus first node of treeView.
              treeViewFocusFirstToplevelNode treeView
              -- Scroll candidate view to screen.
              anythingViewScrollSelectionCandidateToScreen view

-- | Focus next candidate view.
anythingViewFocusNextCandidateView :: AnythingView -> IO ()
anythingViewFocusNextCandidateView view = 
  anythingViewFindCurrentCandidateView view 
      >?>= \ AnythingCandidateView {acavTreeView = treeView} -> do
        -- Unselect all candidates.
        treeViewUnselectAll treeView

        nextCandidateView <- anythingViewGetNextCandidateView view
        case nextCandidateView of
          -- Focus first candidate of next candidate view.
          Just (nextIndex, AnythingCandidateView {acavTreeView = nextTreeView}) -> 
               anythingViewFocusCandidate view nextTreeView treeViewFocusFirstToplevelNode nextIndex
          -- Or focus first candidate of first candidate view.
          Nothing -> anythingViewFocusFirstCandidate view False
        -- Scroll current candidate view to top of visible area.
        anythingViewScrollCandidateViewToTop view

-- | Focus prev candidate view.
anythingViewFocusPrevCandidateView :: AnythingView -> IO ()
anythingViewFocusPrevCandidateView view = 
  anythingViewFindCurrentCandidateView view
    >?>= \ AnythingCandidateView {acavTreeView = treeView} -> do
      -- Unselect all candidates.
      treeViewUnselectAll treeView
      
      prevCandidateView <- anythingViewGetPrevCandidateView view
      case prevCandidateView of
        -- Focus first candidate of previous candidate view.
        Just (prevIndex, AnythingCandidateView {acavTreeView = prevTreeView}) -> 
            anythingViewFocusCandidate view prevTreeView treeViewFocusFirstToplevelNode prevIndex
        -- Focus first candidate of last candidate view.
        Nothing -> anythingViewFocusLastCandidate view FocusFirstNode False

      -- Scroll current candidate view to top of visible area.
      anythingViewScrollCandidateViewToTop view

-- | Scroll candidate view to top.
anythingViewScrollCandidateViewToTop :: AnythingView -> IO ()
anythingViewScrollCandidateViewToTop view = 
  anythingViewFindCurrentCandidateView view 
    >?>= \ AnythingCandidateView {acavTreeView = treeView} -> do
      treeViewY <- rectangleY <$> widgetGetAllocation treeView
      vAdjust   <- scrolledWindowGetVAdjustment $ avCandidateScrolledWindow view
      adjustmentSetValue vAdjust (i2d treeViewY)

-- | Focus default candidate.
anythingViewFocusFirstCandidate :: AnythingView -> Bool -> IO ()           
anythingViewFocusFirstCandidate view scrollToScreen = do
  candidateMap <- readTVarIO $ avCandidateTab view
  find (\ (_, ((_, count), _)) -> count > 0) (M.toList candidateMap) 
    ?>= \ (index, (_, AnythingCandidateView {acavTreeView = treeView})) -> do
    -- Focus first node of treeView.
    treeViewFocusFirstToplevelNode treeView
    -- Update current candidate index.
    writeTVarIO (avCurrentCandidateIndex view) index
    -- Scroll candidate view to screen.
    when scrollToScreen $ anythingViewScrollSelectionCandidateToScreen view

-- | Focus default candidate.
anythingViewFocusLastCandidate :: AnythingView -> FocusNode -> Bool -> IO ()           
anythingViewFocusLastCandidate view focusNode scrollToScreen = do
  candidateMap <- readTVarIO $ avCandidateTab view
  find (\ (_, ((_, count), _)) -> count > 0) (reverse $ M.toList candidateMap) 
    ?>= \ (index, (_, AnythingCandidateView {acavTreeView = treeView})) -> do
    -- Focus first or last node of treeView.
    case focusNode of 
      FocusFirstNode -> treeViewFocusFirstToplevelNode treeView
      FocusLastNode  -> treeViewFocusLastToplevelNode treeView
    -- Update current candidate index.
    writeTVarIO (avCurrentCandidateIndex view) index
    -- Scroll candidate view to screen.
    when scrollToScreen $ anythingViewScrollSelectionCandidateToScreen view

-- | Focus next candidate, focus next candidate view.
-- Move first candidate view and first candidate 
-- if reach last candidate in all candidate view.        
anythingViewFocusNextCandidate :: AnythingView -> IO ()
anythingViewFocusNextCandidate view = 
  anythingViewFindCurrentCandidateView view
      >?>= \ AnythingCandidateView {acavTreeView = treeView} -> do
          atLastNode <- treeViewAtLastToplevelNode treeView
          if atLastNode 
             -- Focus first node of other candidate view
             -- If have reach last candidate of current view.
             then do
               -- Unselect current view.
               treeViewUnselectAll treeView

               nextCandidateView <- anythingViewGetNextCandidateView view
               case nextCandidateView of
                 -- Focus first node of next candidate view,
                 -- when have next candidate.
                 Just (nextIndex, AnythingCandidateView {acavTreeView = nextTreeView}) -> 
                     anythingViewFocusCandidate view nextTreeView treeViewFocusFirstToplevelNode nextIndex
                 -- Otherwise focus first candidate view.
                 Nothing -> anythingViewFocusFirstCandidate view True
             -- Otherwise focus next node.
             else treeViewFocusNextToplevelNode treeView

          -- Scroll selection candidate to screen visible area.
          anythingViewScrollSelectionCandidateToScreen view

-- | Focus prev candidate, focus prev candidate view.
-- Move last candidate view and last candidate 
-- if reach first candidate in all candidate view.        
anythingViewFocusPrevCandidate :: AnythingView -> IO ()
anythingViewFocusPrevCandidate view = 
  anythingViewFindCurrentCandidateView view
      >?>= \ AnythingCandidateView {acavTreeView = treeView} -> do
          atFirstNode <- treeViewAtFirstToplevelNode treeView
          if atFirstNode
             -- Focus last node of other candidate view
             -- If have reach first candidate of current view.
             then do
               -- Unselect current view.
               treeViewUnselectAll treeView

               prevCandidateView <- anythingViewGetPrevCandidateView view
               case prevCandidateView of
                 -- Focus last node of prev candidate view,
                 -- when have prev candidate.
                 Just (prevIndex, AnythingCandidateView {acavTreeView = prevTreeView}) -> 
                    anythingViewFocusCandidate view prevTreeView treeViewFocusLastToplevelNode prevIndex
                 -- Otherwise focus last candidate view.
                 Nothing -> anythingViewFocusLastCandidate view FocusLastNode True
             -- Otherwise focus next node.
             else treeViewFocusPrevToplevelNode treeView

          -- Scroll selection candidate to screen visible area.
          anythingViewScrollSelectionCandidateToScreen view

-- | Scroll selection candidate to screen visible area.
anythingViewScrollSelectionCandidateToScreen :: AnythingView -> IO ()
anythingViewScrollSelectionCandidateToScreen view = 
  anythingViewFindCurrentCandidateView view
    >?>= \ AnythingCandidateView {acavTreeView = treeView} -> 
        -- Get widget coordinate of selection cell.
        treeViewGetSelectedWidgetCoordinate treeView
            
    >?>= \(Rectangle cx cy _ ch) ->  
        -- Transform treeView coordinate to avCandidateBox coordinate.
        widgetTranslateCoordinates treeView (avCandidateBox view) cx cy 
    >?>= \(_, by) -> do
      let candidateY    = i2d by
          candidateH    = i2d ch
          scrolledWindow = avCandidateScrolledWindow view

      -- Get vertical adjustment information. 
      vAdjust      <- scrolledWindowGetVAdjustment scrolledWindow
      currentValue <- adjustmentGetValue vAdjust
      pageSize     <- adjustmentGetPageSize vAdjust

      -- Adjust the value of vertical adjustment
      -- when selection cell escape to visible area.
      if candidateY < currentValue
         then do
           -- When current candidate is first one,
           -- need calculate the height of header, 
           -- make sure header can scroll to visible area of screen.
           atFirstToplevelNode <- treeViewAtFirstToplevelNode treeView
           adjustValue <- if atFirstToplevelNode 
                            then i2d <$> treeViewGetHeaderHeight treeView
                            else return 0
           adjustmentSetValue vAdjust (candidateY - adjustValue)
         else 
             when (candidateY + candidateH > currentValue + pageSize) $
                  adjustmentSetValue vAdjust (candidateY + candidateH - pageSize)

-- | Next candidate view.
anythingViewGetNextCandidateView :: AnythingView -> IO (Maybe (Int, AnythingCandidateView))  
anythingViewGetNextCandidateView view = do
  currentIndex <- readTVarIO $ avCurrentCandidateIndex view
  candidateMap <- readTVarIO $ avCandidateTab view
  let (_, matchList) = splitAt currentIndex (M.toList candidateMap)
  return $ 
    case matchList of
      []  -> Nothing
      [_] -> Nothing
      _ -> 
          let (_, list) = partition (\ (_, ((_, num), _)) -> num == 0) (tail matchList)
          in case list of
               [] -> Nothing
               _ -> 
                 let (index, (_, view)) = head list
                 in Just (index, view)

-- | Prev candidate view.
anythingViewGetPrevCandidateView :: AnythingView -> IO (Maybe (Int, AnythingCandidateView))  
anythingViewGetPrevCandidateView view = do
  currentIndex <- readTVarIO $ avCurrentCandidateIndex view
  candidateMap <- readTVarIO $ avCandidateTab view
  let (matchList, _) = splitAt currentIndex (M.toList candidateMap)
  return $ 
    case matchList of
      [] -> Nothing
      _ -> let (_, list) = partition (\ (_, ((_, num), _)) -> num == 0) (reverse matchList)
          in case list of
               [] -> Nothing
               _ -> let (index, (_, view)) = head list
                    in Just (index, view)

-- | Get completion.
anythingViewGetCompletion :: AnythingInput -> [String] -> AnythingCompletionRule -> AnythingCompletion
anythingViewGetCompletion input candidates rule = completion
  where prefix     = rule input
        common     = foldl1_ intersectFront candidates
        completion = if prefix `isPrefixOf` common 
                        then drop (length prefix) common
                        else ""

-- | Add commands to view.
anythingViewSetCommands :: AnythingView -> [String] -> IO ()
anythingViewSetCommands view commands = do
  let (AnythingCommandView treeView store) = avCommandTab view
  listStoreClear store
  forM_ commands (listStoreAppend store)
  treeViewFocusFirstToplevelNode treeView

-- | Focus next command.
anythingViewFocusNextCommand :: AnythingView -> IO ()
anythingViewFocusNextCommand view = do
  let (AnythingCommandView treeView _) = avCommandTab view
  ifM (treeViewAtLastToplevelNode treeView)
      -- Select first command if have reach last one.
      (treeViewFocusFirstToplevelNode treeView)
      -- Otherwise select next command.
      (treeViewFocusNextToplevelNode treeView)

-- | Focus prev command.
anythingViewFocusPrevCommand :: AnythingView -> IO ()
anythingViewFocusPrevCommand view = do
  let (AnythingCommandView treeView _) = avCommandTab view
  ifM (treeViewAtFirstToplevelNode treeView)
      -- Select last command if have reach first one.
      (treeViewFocusLastToplevelNode treeView)
      -- Otherwise select previous command.
      (treeViewFocusPrevToplevelNode treeView)

-- | Show candidates.
anythingViewShowCandidateTab :: AnythingView -> IO ()
anythingViewShowCandidateTab view =
  notebookSetCurrentPage (avNotebook view) anythingCandidateTabIndex

-- | Show commands.
anythingViewShowCommandTab :: AnythingView -> IO ()
anythingViewShowCommandTab view =
  notebookSetCurrentPage (avNotebook view) anythingCommandTabIndex

-- | Is candidates tab.
anythingViewIsFocusCandidateTab :: AnythingView -> IO Bool
anythingViewIsFocusCandidateTab view = 
  (anythingCandidateTabIndex ==) <$> notebookGetCurrentPage (avNotebook view)

-- | Is commands tab.
anythingViewIsFocusCommandTab :: AnythingView -> IO Bool
anythingViewIsFocusCommandTab view = 
  (anythingCommandTabIndex ==) <$> notebookGetCurrentPage (avNotebook view)

-- | Find AnythingCandidateView from given index.
anythingViewFindCandidateView :: AnythingView -> Int -> IO (Maybe AnythingCandidateView)
anythingViewFindCandidateView view index = do
  candidateTab <- readTVarIO $ avCandidateTab view
  findMinMatch candidateTab (\ i (_, _) -> i == index)
                   ?>=> \ (_, (_, view)) -> return (Just view)

-- | Find current anything name.
anythingViewFindCurrentAnythingName :: AnythingView -> IO (Maybe AnythingName)
anythingViewFindCurrentAnythingName view = do
  currentIndex <- readTVarIO $ avCurrentCandidateIndex view
  candidateTab <- readTVarIO $ avCandidateTab view
  findMinMatch candidateTab (\ i (_, _) -> i == currentIndex) 
    ?>=> \ (_, ((name, _), _)) -> return (Just name)

-- | Find AnythingCandidateView from given index.
anythingViewFindCurrentCandidateView :: AnythingView -> IO (Maybe AnythingCandidateView)          
anythingViewFindCurrentCandidateView view = do
  currentIndex <- readTVarIO $ avCurrentCandidateIndex view
  anythingViewFindCandidateView view currentIndex 

-- | Get selected candidate.
anythingViewGetSelectedCandidate :: AnythingView -> IO (Maybe AnythingCandidateWrap)
anythingViewGetSelectedCandidate view = 
    anythingViewFindCurrentCandidateView view
       >?>=> \ AnythingCandidateView {acavTreeView  = treeView
                                     ,acavListStore = listStore} ->
                treeViewGetSelection treeView
                    >>= \x -> treeSelectionGetSelected x
       >?>=> (\x -> Just <$> listStoreGetValue listStore (listStoreIterToIndex x))

-- | Focus candidate and update current candidate index.
anythingViewFocusCandidate :: TreeViewClass view => AnythingView -> view -> (view -> IO ()) -> Int -> IO ()
anythingViewFocusCandidate view treeView treeFocusAction index = do
  -- Focus candidate.
  treeFocusAction treeView
  -- Update current candidate index.
  writeTVarIO (avCurrentCandidateIndex view) index

-- | Expand first entry.
anythingViewExpandSelectedCandidate :: (Int, (TVar AnythingSearchCache, (AnythingViewInput, (AnythingView, Client)))) -> IO ()
anythingViewExpandSelectedCandidate (keyPressId, (cache, (input, (view, client)))) = do
  -- Make sure switch candidate view first.
  anythingViewShowCandidateTab view

  anythingViewGetSelectedCandidate view 
     >?>= \ candidateWrap -> 
         case candidateWrap of
           AnythingCandidateWrap a -> do
               expandName <- candidateExpandName a (aviUnselectText input)
               anythingViewCompletion client cache view expandName keyPressId

-- | Toggle tab.
anythingViewToggleTab :: (AnythingView, AnythingViewInput) -> IO ()
anythingViewToggleTab (view, input) = do
  isFocusCommandTab <- anythingViewIsFocusCommandTab view
  if isFocusCommandTab
     then anythingViewShowCandidateTab view
     else 
       anythingViewFindCurrentAnythingName view 
       >?>= \name -> 
           M.lookup name anythingList ?>= \(Anything {anythingCommandFun = commandFun}) -> 
               anythingViewGetSelectedCandidate view 
       >?>= \candidateWrap -> do
         iType <- readTVarIO $ avInteractiveType view
         commandList <- commandFun (aviUnselectText input) candidateWrap iType
         let commands = map fst commandList
         anythingViewSetCommands view commands
         anythingViewShowCommandTab view

-- | Run command.
anythingViewRunCommand :: Bool -> (AnythingView, (Client, AnythingViewInput)) -> IO ()
anythingViewRunCommand runSelected (view, (client, input)) = 
  anythingViewFindCurrentAnythingName view 
  >?>= \name -> 
          M.lookup name anythingList 
  ?>= \(Anything {anythingCommandFun = commandFun}) -> 
              anythingViewGetSelectedCandidate view 
  >?>= \candidateWrap -> 
      (if runSelected
       then treeViewGetSelectedPath (acovTreeView $ avCommandTab view) 
       else return $ Just [0]) 
  >?>= \commandPath -> do
        iType <- readTVarIO $ avInteractiveType view
        commandList <- commandFun (aviUnselectText input) candidateWrap iType
        commandList ?! head commandPath ?>= \ (_, action) -> do
            -- Execute action.
            action client
            -- Just need exit local interactivebar explicitly when interactive type is 'GlobalSearch'
            -- If is other type, exit interactivebar when user *complete* interactive progress.
            when (iType == GlobalSearch) $
                 mkDaemonSignal client LocalInteractivebarExit LocalInteractivebarExitArgs

-- | Run first command for current candidate.
anythingViewRunFirstCommand :: (AnythingView, (Client, AnythingViewInput)) -> IO ()
anythingViewRunFirstCommand = 
    anythingViewRunCommand False

-- | Run selected command for current candidate.
anythingViewRunSelectedCommand :: (AnythingView, (Client, AnythingViewInput)) -> IO ()
anythingViewRunSelectedCommand = 
    anythingViewRunCommand True

-- | Get match candidate for current input.
anythingViewGetMatches :: AnythingName
                       -> AnythingInput 
                       -> Client 
                       -> AnythingSearch 
                       -> AnythingFilterRule 
                       -> IO [AnythingCandidateWrap]
anythingViewGetMatches name path client (AnythingSearch searchFun) filterRule = do
  candidates <- 
      -- Protect search algorithm won't crash.
      -- If any unexpected exception throw, 
      -- just return [] to hide current search candidate.
      -- Avoid one search exception crash all process.
      Exc.catch 
               (map AnythingCandidateWrap <$> searchFun path client)
               (\ (_ :: Exc.IOException) -> do
                    putStrLn $ "anythingViewGetMatches: Catch exception when search candidate for `" 
                                 ++ name
                                 ++ "` please report this bug to author!"
                    return [])
  return $ filter (\ (AnythingCandidateWrap x) -> filterRule path (candidateFilterName x)) candidates
  
-- | Output completion candidates on TreeView.
-- And return highlight completion string for input.
anythingViewCompletion :: Client -> TVar AnythingSearchCache -> AnythingView -> FilePath -> AnythingKeyPressId -> IO ()
anythingViewCompletion client anythingSearchCache view path keyPressId = do
  -- Make sure switch candidate view first.
  anythingViewShowCandidateTab view

  -- Send AnythingViewOutput signal.
  boxHeight <- anythingViewGetBoxHeight view 
  mkDaemonSignal client AnythingViewOutput (AnythingViewOutputArgs path "" boxHeight keyPressId)

  -- Set candidates.
  cList <- liftM M.toList (readTVarIO (avCandidateTab view))
  forM_ cList $ \(_, ((name, _), candidateView)) -> 
      M.lookup name anythingList ?>= \(Anything {anythingSearch            = searchFun
                                                ,anythingCompletionRule    = completionRule
                                                ,anythingFilterRule        = filterRule
                                                ,anythingInputDepend       = inputDepend
                                                ,anythingCalculateDelay    = delay
                                                }) -> do
        searchCache <- readTVarIO anythingSearchCache 
        let 
            -- Calculate candidate to show, if this step need *long* time,
            -- please set bigger value in 'anythingCandidateDelay' 
            -- to avoid waste CPU time when you type so fast, default is 0 ms. 
            -- Example, if you set 'anythingCandidateDelay' with '200 ms' and 
            -- you type next char in 100 ms, 
            -- current calculation will stop and reset delay counter.
            getMatches = do
              -- Start spinner animation.
              titleWidgetStart (acavTitleWidget candidateView)
              anythingViewGetMatches name path client searchFun filterRule
            -- Update UI after candidates calculate finished. 
            setCandidates name cacheTVar candidates = do
              -- Stop spinner animation.
              titleWidgetStop (acavTitleWidget candidateView)
              -- Update UI.
              anythingViewSetCandidates view client name candidates path completionRule keyPressId
              -- Remove information from search cache to new search in next loop.
              modifyTVarIO cacheTVar (M.delete name)
            -- Start new search.
            startNewSearch name cacheTVar = do
              -- Calculate after delay reach.
              handlerId <- timeoutAdd
                          (do
                            -- Fork calculation avoid block GTK+ main thread.
                            state <- forkGuiIO getMatches (setCandidates name cacheTVar)
                            -- Update search cache with thread information.
                            modifyTVarIO cacheTVar (M.insert name (Left state))
                            -- Just run once.
                            return False) 
                           delay
              -- Update search cache with delay HandlerId.
              modifyTVarIO cacheTVar (M.insert name (Right handlerId))
              
        case M.lookup name searchCache of
          -- Start new search if not found in search cache.
          Nothing -> 
              startNewSearch name anythingSearchCache
          -- Start new search if calculation still not start.
          -- Type so fast? ;)
          Just (Right delayHandlerId) -> do
              timeoutRemove delayHandlerId
              startNewSearch name anythingSearchCache
          Just (Left (signal, calcThreadId, guiThreadId)) -> do
              -- Because have different filter result for different input,
              -- so always kill previous UI render thread if it still running.
              killThread guiThreadId
              
              tStatus <- threadStatus calcThreadId
              if not inputDepend && tStatus == ThreadRunning
                   -- Use previous calculation thread result 
                   -- when search list don't depend input and previous calculation thread is running.
                   then do
                     newGuiThreadId <- onGuiSignal signal (setCandidates name anythingSearchCache)
                     modifyTVarIO anythingSearchCache (M.insert name (Left (signal, calcThreadId, newGuiThreadId)))
                   -- Otherwise, start new calculation thread. 
                   else do 
                     -- Kill calculate thread first.
                     killThread calcThreadId
                     -- Start new search.
                     startNewSearch name anythingSearchCache

-- | Get box height, remove blank space in last candidate view.
anythingViewGetBoxHeight :: AnythingView -> IO (Maybe Int)               
anythingViewGetBoxHeight view = 
  ifM (anythingViewIsFocusCandidateTab view)
          (ifM (anythingViewHasCandidates view)
                 -- Return box minimized height when box contain candidate view.
                 (do
                   candidateMap <- readTVarIO $ avCandidateTab view
                   find (\ (_, ((_, count), _)) -> count > 0) (reverse $ M.toList candidateMap) 
                            ?>=> \ (_, (_, AnythingCandidateView {acavTreeView = treeView})) -> do
                              candidateBoxHeight <- rectangleH <$> widgetGetAllocation (avCandidateBox view)
                              visibleHeight      <- rectangleH <$> treeViewGetVisibleRect treeView
                              candidatesHeight   <- anythingViewGetCandidatesHeight treeView
                              return $ Just $ candidateBoxHeight - visibleHeight + candidatesHeight)
                 -- Return Nothing when nothing in box.
                 (return Nothing))
           (Just <$> anythingViewGetCandidatesHeight (acovTreeView $ avCommandTab view))

-- | Whether has candidates?
anythingViewHasCandidates :: AnythingView -> IO Bool
anythingViewHasCandidates =
  containerHasChildren . avCandidateBox

-- | Get view height.
anythingViewGetCandidatesHeight :: TreeViewClass treeView => treeView -> IO Int  
anythingViewGetCandidatesHeight treeView = do
  candidateNumber  <- treeViewGetToplevelNodeCount treeView
  cellHeight       <- anythingViewGetCellHeight treeView
  headerHeight     <- treeViewGetHeaderHeight treeView
  return $ headerHeight + (if candidateNumber < anythingCandidateViewLimit
                           then candidateNumber
                           else anythingCandidateViewLimit) * cellHeight

-- | Get cell height.
anythingViewGetCellHeight :: TreeViewClass treeView => treeView -> IO Int
anythingViewGetCellHeight treeView = do
  cellHeight <- treeViewGetDefaultCellHeight treeView
  return $ 
    case cellHeight of
      Just height -> (if height > 1 
                     then height 
                     -- Use default cell height replace when 
                     -- can't get valid cell height from tree view.
                     else anythingViewDefaultCellHeight)
      Nothing -> anythingViewDefaultCellHeight
