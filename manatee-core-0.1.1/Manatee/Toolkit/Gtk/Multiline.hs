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

module Manatee.Toolkit.Gtk.Multiline where

import Control.Monad
import Data.ByteString (ByteString)
import Graphics.UI.Gtk hiding (Statusbar, statusbarNew)
import Manatee.Toolkit.General.Basic
import Manatee.Toolkit.General.Functor
import Manatee.Toolkit.General.Maybe
import System.IO.Unsafe

-- | Wrap user action.
-- Then user can undo/redo this action.
textBufferWrapAction :: TextBufferClass self => self -> IO () -> IO ()
textBufferWrapAction tb action = do
  textBufferBeginUserAction tb  -- entry in action
  action                        -- execute action
  textBufferEndUserAction tb    -- entry out action


-- | Get text with given text tag.
textBufferGetTagTextWithIter :: TextBufferClass buffer => buffer
                             -> TextIter
                             -> TextTag
                             -> IO (Maybe String)
textBufferGetTagTextWithIter buffer iter tag = do
  startIter <- textIterCopy iter
  endIter   <- textIterCopy iter
  moveBackward <- textIterBackwardToTagToggle startIter (Just tag)
  moveForward  <- textIterForwardToTagToggle endIter (Just tag)
  if moveBackward && moveForward
     then liftM Just $ textBufferGetText buffer startIter endIter True
     else return Nothing

-- | Get insert text with given text tag.
textBufferGetTagText :: TextBufferClass buffer => buffer
                     -> TextTag
                     -> IO (Maybe String)
textBufferGetTagText buffer tag = do
  iter <- textBufferGetInsertIter buffer
  textBufferGetTagTextWithIter buffer iter tag

-- | Get bytestring with give text tag.
textBufferGetTagByteStringWithIter :: TextBufferClass buffer => buffer
                                   -> TextIter
                                   -> TextTag
                                   -> IO (Maybe ByteString)
textBufferGetTagByteStringWithIter buffer iter tag = do
  startIter <- textIterCopy iter
  endIter   <- textIterCopy iter
  moveBackward <- textIterBackwardToTagToggle startIter (Just tag)
  moveForward  <- textIterForwardToTagToggle endIter (Just tag)
  if moveBackward && moveForward
     then liftM Just $ textBufferGetByteString buffer startIter endIter True
     else return Nothing

-- | Get insert text with given text tag.
textBufferGetTagByteString :: TextBufferClass buffer => buffer
                           -> TextTag
                           -> IO (Maybe ByteString)
textBufferGetTagByteString buffer tag = do
  iter <- textBufferGetInsertIter buffer
  textBufferGetTagByteStringWithIter buffer iter tag

-- | Selection bound with line/row.
textBufferSelectBound :: TextBufferClass self => self -> (Int, Int) -> (Int, Int) -> IO ()
textBufferSelectBound tb (insertLine, insertRow) (selectLine, selectRow) = do
  -- Get temp iter.
  iter <- textBufferGetStartIter tb

  -- Create insert iter.
  insertIter <- textIterCopy iter
  textIterSetLine insertIter insertLine
  textIterSetLineOffset insertIter insertRow

  -- Create select iter.
  selectIter <- textIterCopy iter
  textIterSetLine selectIter selectLine
  textIterSetLineOffset selectIter selectRow

  -- Select bound.
  textBufferSelectRange tb insertIter selectIter

-- | Text buffer keep selection to execute action.
textBufferKeepSelection :: TextBufferClass self => self -> Bool -> Bool -> IO () -> IO ()           
textBufferKeepSelection tb sLeft eLeft action = do 
  -- Get selection range.
  (sIter, eIter) <- textBufferGetSelectionBounds tb

  -- Decide selection range.
  fromUpToDown <- textBufferIsEqualInsertIter tb eIter

  -- Record iter mark.
  sMark <- textBufferCreateMark tb Nothing sIter sLeft
  eMark <- textBufferCreateMark tb Nothing eIter eLeft

  -- Execute action.
  action

  -- Restore iter.
  startIter <- textBufferGetIterAtMark tb sMark
  endIter   <- textBufferGetIterAtMark tb eMark

  -- Restore selection range.
  if fromUpToDown
     then textBufferSelectRange tb endIter startIter
     else textBufferSelectRange tb startIter endIter

  -- Update selection iter.
  textBufferUpdateSelectionIter tb

-- | Move text, tags, and pixbufs between start and end (the order of start and end doesn't matter) to iter.
textBufferMoveRange :: TextBufferClass self => self -> TextIter -> TextIter -> TextIter -> IO ()
textBufferMoveRange tb iter start end = 
    -- Don't do anything when range len is 0.
    unlessM (textIterEqual start end) $ do
      -- Order range iter first.
      textIterOrder start end
      -- Move range.
      ifM (textIterInRange iter start end)
              -- In range.
              (do                              
                -- Record iter mark for restore.
                sMark <- textBufferCreateMark tb Nothing start True

                -- Get slice.
                slice <- textBufferGetSlice tb start end True

                -- Delete old range.
                textBufferDelete tb start end

                -- Insert new slice.
                textBufferInsertAtMark tb sMark slice)          
              -- Out range.
              (do                               
                -- Record iter mark for restore.
                sMark <- textBufferCreateMark tb Nothing start True
                eMark <- textBufferCreateMark tb Nothing end True

                -- Insert range.
                textBufferInsertRange tb iter start end

                -- Delete old range.
                sIter <- textBufferGetIterAtMark tb sMark
                eIter <- textBufferGetIterAtMark tb eMark
                textBufferDelete tb sIter eIter)          

-- | Cancel selection.
textBufferCancelSelection :: TextBufferClass self => self -> IO ()
textBufferCancelSelection tb = do
  iter <- textBufferGetInsertIter tb
  textBufferSelectRange tb iter iter

-- | Get text iter at cursor.
textBufferGetInsertIter :: TextBufferClass self => self -> IO TextIter
textBufferGetInsertIter tb =
    textBufferGetIterAtMark tb 
        =<< textBufferGetInsert tb

-- | Create an iterator at start position of special line.
textBufferGetLineStartIter :: TextBufferClass self => self -> Int -> IO TextIter  
textBufferGetLineStartIter = textBufferGetIterAtLine

-- | Create an iterator at start position of current line.
textBufferGetCurrentLineStartIter :: TextBufferClass self => self -> IO TextIter
textBufferGetCurrentLineStartIter tb = 
    textBufferGetLineStartIter tb 
        =<< textBufferGetLine tb

-- | Create an iterator line delimiter characters, 
-- If incLimitincDelimiter is True which will be either a newline, a carriage return, a carriage return/newline in sequence.
-- Otherwise, not include delimiter characters. 
textBufferGetLineEndIter :: TextBufferClass self => self -> Int -> Bool -> IO TextIter
textBufferGetLineEndIter tb line incDelimiter = do
  -- Get iter.
  iter <- textBufferGetInsertIter tb
  -- Set line.
  textIterSetLine iter line
  -- Get chars in line.
  chars <- (if incDelimiter 
           -- Get chars include delimiter.
           then textIterGetCharsInLine
           -- Get chars not include delimiter.
           else textIterGetCharsInLine_ ) iter
  -- Set line offset.
  textIterSetLineOffset iter chars
  return iter

-- | Create an iterator line delimiter characters, 
-- which will be either a newline, a carriage return, a carriage return/newline in sequence.
textBufferGetCurrentLineEndIter :: TextBufferClass self => self -> IO TextIter  
textBufferGetCurrentLineEndIter tb = do
  line <- textBufferGetLine tb
  textBufferGetLineEndIter tb line True

-- | Create an iterator line delimiter characters, 
textBufferGetCurrentLineEndIter_ :: TextBufferClass self => self -> IO TextIter  
textBufferGetCurrentLineEndIter_ tb = do
  line <- textBufferGetLine tb
  textBufferGetLineEndIter tb line False

-- | Get line from text buffer.
textBufferGetLine :: TextBufferClass self => self -> IO Int  
textBufferGetLine tb = 
    textIterGetLine 
        =<< textBufferGetInsertIter tb

-- | Get line offset from text buffer.
textBufferGetLineOffset :: TextBufferClass self => self -> IO Int  
textBufferGetLineOffset tb = 
    textIterGetLineOffset 
        =<< textBufferGetInsertIter tb

{-# NOINLINE textBufferSelectionIter #-}
-- Don't use `TextBufferClass` replace `TextBuffer`,
-- Otherwise, every time you use this attribute, 
-- it will be re-evaluate to the definition by compiler.
-- That's to say, you always `get` Nothing.  
textBufferSelectionIter :: Attr TextBuffer (Maybe TextIter)
textBufferSelectionIter = unsafePerformIO objectCreateAttribute

-- | Set text buffer selection iter.
textBufferSetSelectionIter :: TextBufferClass self => self -> TextIter -> IO ()       
textBufferSetSelectionIter tb ti = 
    set (toTextBuffer tb) [textBufferSelectionIter := Just ti]

-- | Remove text buffer selection iter.    
textBufferRemoveSelectionIter :: TextBufferClass self => self -> IO ()
textBufferRemoveSelectionIter tb =
    set (toTextBuffer tb) [textBufferSelectionIter := Nothing]

-- | Get text buffer selection iter.
textBufferGetSelectionIter :: TextBufferClass self => self -> IO (Maybe TextIter)
textBufferGetSelectionIter tb = 
    get (toTextBuffer tb) textBufferSelectionIter

-- | Update selection iter after buffer changed.
-- If selection active, just update SelectionIter, 
-- otherwise set Nothing.
textBufferUpdateSelectionIter :: TextBufferClass self => self -> IO ()
textBufferUpdateSelectionIter tb = 
    ifM (textBufferHasSelection tb)
            (textBufferSetSelectionIter tb 
                 =<< textBufferGetSelectionBoundIter tb)
            (textBufferRemoveSelectionIter tb)

-- | Get selection bound line.
textBufferGetSelectionBoundLines :: TextBufferClass self => self -> IO (TextIter, TextIter, Bool, Bool) 
textBufferGetSelectionBoundLines tb = do
  -- Whether active selection.
  hasSelect <- textBufferHasSelection tb
  if hasSelect
     -- Get iterator of selection bound.
     then do
       (sIter, eIter) <- textBufferGetSelectionBounds tb
       firstLine <- textIterIsFirstLine sIter
       lastLine  <- textIterIsLastLine eIter
       textIterBackwardToLineStart sIter
       chars <- textIterGetCharsInLine eIter
       textIterSetLineOffset eIter chars

       return (sIter, eIter, firstLine, lastLine)
     -- Get iterator of current line.
     else do
       sIter <- textBufferGetCurrentLineStartIter tb
       eIter <- textBufferGetCurrentLineEndIter tb
       firstLine <- textIterIsFirstLine sIter
       lastLine  <- textIterIsLastLine sIter

       return (sIter, eIter, firstLine, lastLine)

-- | Get bound iter of selection region.
textBufferGetSelectionBoundIter :: TextBufferClass self => self -> IO TextIter
textBufferGetSelectionBoundIter tb = (<=<) (textBufferGetIterAtMark tb) textBufferGetSelectionBound tb

-- | Get insert iter of selection region.
textBufferGetSelectionInsertIter :: TextBufferClass self => self -> IO TextIter  
textBufferGetSelectionInsertIter = textBufferGetInsertIter

-- | Is equal current InsertIter?
textBufferIsEqualInsertIter :: TextBufferClass self => self -> TextIter -> IO Bool
textBufferIsEqualInsertIter tb ti = 
  textIterEqual ti 
      =<< textBufferGetInsertIter tb
  
-- | Delete last char.
textBufferDeleteLastChar :: TextBufferClass self => self -> IO ()
textBufferDeleteLastChar tb = do
  -- Get start and end iter for last char.
  startIter <- textBufferGetEndIter tb
  endIter   <- textIterCopy startIter
  textIterBackwardChar startIter

  -- Delete last char.
  textBufferDelete tb startIter endIter

-- | Text buffer newline at mark.
textBufferInsertNewlineAtMark :: TextBufferClass self => self -> TextMark -> IO ()  
textBufferInsertNewlineAtMark tb mark = 
    textBufferInsertAtMark tb mark "\n"

-- | Text buffer insert at mark.
textBufferInsertAtMark :: TextBufferClass self => self -> TextMark -> String -> IO ()  
textBufferInsertAtMark tb mark str = do
  -- Get iter.
  iter <- textBufferGetIterAtMark tb mark

  -- Insert at mark.
  textBufferInsert tb iter str

-- | Text view place cursor.
-- Move insert to iter, then scroll text view to visible area of widget.
textViewPlaceCursor :: TextViewClass self => self -> TextIter -> IO ()
textViewPlaceCursor tv ti = do
  tb <- textViewGetBuffer tv
  textBufferPlaceCursor tb ti
  textViewScrollCursorOnscreen tv

-- | Place cursor with mark.
textViewPlaceCursorWithMark :: TextViewClass self => self -> TextMark -> IO ()  
textViewPlaceCursorWithMark tv tm = do
  tb <- textViewGetBuffer tv
  iter <- textBufferGetIterAtMark tb tm
  textViewPlaceCursor tv iter

-- | Get line count.
textViewGetLineCount :: TextViewClass self => self -> IO Int
textViewGetLineCount tv = do
  tb <- textViewGetBuffer tv
  textBufferGetLineCount tb

-- | Goto line.
-- If line number not in buffer bound, goto min/max line.
textViewGotoLine :: TextViewClass self => self -> Int -> IO ()
textViewGotoLine tv line = do
  tb <- textViewGetBuffer tv
  lineCount <- textBufferGetLineCount tb
  let lineNumber 
          | line <= 1
              = 1
          | line > lineCount
              = lineCount
          | otherwise 
              = line
  -- Line number count from 0, so need decreases '1'
  iter <- textBufferGetIterAtLine tb (lineNumber - 1)
  textViewPlaceCursor tv iter

-- | Goto column.
-- If column number not in buffer bound, goto start/end of line.
textViewGotoColumn :: TextViewClass self => self -> Int -> IO ()
textViewGotoColumn tv column = do
  tb <- textViewGetBuffer tv
  currentIter <- textBufferGetInsertIter tb
  columnCount <- textIterGetCharsInLine_ currentIter
  let columnNumber 
          | column <= 0
              = 0
          | column > columnCount
              = columnCount
          | otherwise
              = column
  currentLine <- textIterGetLine currentIter
  columnIter  <- textBufferGetIterAtLineOffset tb currentLine columnNumber
  textViewPlaceCursor tv columnIter

-- | Text view wrap user action.
textViewWrapAction :: TextViewClass self => self -> IO () -> IO ()
textViewWrapAction tv action = do
  tb <- textViewGetBuffer tv
  textBufferWrapAction tb action

-- | Text View load file.
textViewLoadFile :: TextViewClass self => self -> FilePath -> IO ()  
textViewLoadFile tv path = textViewSetText tv =<< readFile path

-- | Set text.
textViewSetText :: TextViewClass self => self -> String -> IO ()
textViewSetText tv str = do
  tb <- textViewGetBuffer tv
  textBufferSetText tb str

-- | Get text iter from text view.
textViewGetTextIter :: TextViewClass self => self -> IO TextIter
textViewGetTextIter tv = 
    textBufferGetInsertIter 
        =<< textViewGetBuffer tv

-- | Move text iter
textViewMoveTextIter :: (TextViewClass self, ScrolledWindowClass swc) => (TextIter -> IO Bool) -> self -> swc -> IO ()
textViewMoveTextIter move tv sw = do
  -- Action for text iter.
  ti <- textViewGetTextIter tv
  move ti

  -- Move cursor place.
  tb <- textViewGetBuffer tv
  textBufferPlaceCursor tb ti

  -- Scroll view to mark of iter.
  aj <- scrolledWindowGetVAdjustment sw
  vl <- adjustmentGetValue aj
  y  <- (<<<=) (i2d . fst) (textViewGetLineYrange tv) 
                               =<< textViewGetTextIter tv
  if y <= vl
     -- When mark below or equal current top y coordinate.
     -- just scroll page to avoid screen jump from top to end.
     then adjustmentSetValue aj y
     -- Otherwise scroll screen to including mark.
     else textViewScrollCursorOnscreen tv 

  return ()

-- | Forward line.
textViewForwardLine :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewForwardLine = 
    textViewMoveTextIter $ \iter -> do
      -- Whether at last line.
      lastLine <- textIterIsLastLine iter
      -- Do nothing when at last line.
      unless lastLine $ textIterForwardLine iter >> return ()
      return True 

-- | Backward line.
textViewBackwardLine :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewBackwardLine = textViewMoveTextIter textIterBackwardLine

-- | Forward char.
textViewForwardChar :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewForwardChar = textViewMoveTextIter textIterForwardChar

-- | Backward char.
textViewBackwardChar :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewBackwardChar = textViewMoveTextIter textIterBackwardChar

-- | Forward word.
textViewForwardWord :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewForwardWord = textViewMoveTextIter textIterForwardWordEnd

-- | Backward word.
textViewBackwardWord :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewBackwardWord = textViewMoveTextIter textIterBackwardWordStart

-- | Move to buffer beginning.
textViewBegin :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewBegin = 
    textViewMoveTextIter $ \t -> textIterBackwardToStart t >> return True

-- | Move to buffer end.
textViewEnd :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewEnd =
    textViewMoveTextIter $ \t -> textIterForwardToEnd t >> return True

-- | Get text.
textViewGetText :: TextViewClass self => self -> IO String
textViewGetText tv = do
  tb           <- textViewGetBuffer tv
  (start, end) <- textBufferGetBounds tb
  textBufferGetText tb start end True

-- | Get current line.
textViewGetCurrentLineContent :: TextViewClass self => self -> IO String
textViewGetCurrentLineContent tv = do
  -- Get text buffer.
  tb <- textViewGetBuffer tv
  
  -- Get line start and end iter.
  startIter <- textBufferGetCurrentLineStartIter tb
  endIter   <- textBufferGetCurrentLineEndIter_ tb

  -- Get current line.
  textBufferGetText tb startIter endIter True

-- | Get current line number.
textViewGetLine :: TextViewClass self => self -> IO Int
textViewGetLine = 
    (<<<=) succ ((<=<) textIterGetLine textViewGetTextIter)

-- | Get current column number
textViewGetColumn :: TextViewClass self => self -> IO Int
textViewGetColumn =
    (<=<) textIterGetLineOffset textViewGetTextIter

-- | Select all.
textViewSelectAll :: TextViewClass self => self -> IO ()
textViewSelectAll tv = do
  -- Get text buffer.
  tb           <- textViewGetBuffer tv

  -- Get start end iter.
  (start, end) <- textBufferGetBounds tb

  -- Selection all.
  textBufferSelectRange tb start end

  -- Update selection iter.
  textBufferUpdateSelectionIter tb

-- | Delete.
textViewDelete :: TextViewClass self => self -> Bool -> Bool -> IO Bool
textViewDelete tv interactive defaultEditable = do
  tb <- textViewGetBuffer tv
  textBufferDeleteSelection tb interactive defaultEditable

-- | Cut.
textViewCut :: TextViewClass self => self -> IO ()
textViewCut tv = do
  tb   <- textViewGetBuffer tv
  clip <- clipboardGet selectionClipboard
  textBufferCutClipboard tb clip True

-- | Copy.
textViewCopy :: TextViewClass self => self -> IO ()
textViewCopy tv = do
  tb   <- textViewGetBuffer tv
  clip <- clipboardGet selectionClipboard
  textBufferCopyClipboard tb clip

-- | Paste.
textViewPaste :: TextViewClass self => self -> IO ()
textViewPaste tv = do
  tb   <- textViewGetBuffer tv
  clip <- clipboardGet selectionClipboard
  textBufferPasteClipboardAtCursor tb clip True

-- | Delete forward char.
textViewDeleteForwardChar :: TextViewClass self => self -> Bool -> IO Bool
textViewDeleteForwardChar tv = 
    textViewDeleteBound tv textIterForwardChar

-- | Delete backward char.
textViewDeleteBackwardChar :: TextViewClass self => self -> Bool -> IO Bool
textViewDeleteBackwardChar tv = 
    textViewDeleteBound tv textIterBackwardChar

-- | Delete forward word.
textViewDeleteForwardWord :: TextViewClass self => self -> Bool -> IO Bool
textViewDeleteForwardWord tv = 
    textViewDeleteBound tv textIterForwardWordEnd

-- | Delete backward word.
textViewDeleteBackwardWord :: TextViewClass self => self -> Bool -> IO Bool
textViewDeleteBackwardWord tv = 
    textViewDeleteBound tv textIterBackwardWordStart

-- | Delete to line end.
textViewDeleteToLineEnd :: TextViewClass self => self -> Bool -> IO Bool
textViewDeleteToLineEnd tv = 
    textViewDeleteBound tv textIterForwardToLineEnd

-- | Delete to line start.
textViewDeleteToLineStart :: TextViewClass self => self -> Bool -> IO Bool
textViewDeleteToLineStart tv = 
    textViewDeleteBound tv (\t -> textIterBackwardToLineStart t >> return True)

-- | Delete bound.
textViewDeleteBound :: TextViewClass self => self -> (TextIter -> IO Bool) -> Bool -> IO Bool
textViewDeleteBound tv move deleteWhenEditable = do
  -- Get text buffer.
  tb <- textViewGetBuffer tv

  -- Get start iter.
  start <- textViewGetTextIter tv
  offset <- textIterGetOffset start

  -- Move iter.
  move start

  -- Get end iter.
  end <- textViewGetTextIter tv

  -- Build delete/restore action.
  let deleteAction = do
        textBufferDelete tb start end
        return True
      restoreAction = do
        textIterSetOffset end offset
        return False

  isEditable <- textIterBoundIsEditable (start, end)
  if deleteWhenEditable && not isEditable
     -- Restore if can't delete. 
     then restoreAction
     -- Otherwise delete bound.
     else deleteAction

-- | Duplicate lines.
-- Just duplicate current line to below if haven't selection anything.
-- Otherwise duplicate selection lines to below.
textViewDupLines :: TextViewClass self => self -> Bool -> IO ()
textViewDupLines tv dupBelow = do
  -- Get text buffer.
  tb <- textViewGetBuffer tv

  -- Get selection bound line.
  (sIter, eIter, _, lastLine) <- textBufferGetSelectionBoundLines tb

  if dupBelow
     -- Duplicate below.
     then textBufferKeepSelection tb True True $ 
          do
            -- Record end iter mark.
            eMark <- textBufferCreateMark tb Nothing eIter True -- make mark before insert
  
            -- Insert duplicate content.
            textBufferInsertRange tb eIter sIter eIter

            -- Add newline after end iterator when end iter at last line of buffer.
            when lastLine $ textBufferInsertNewlineAtMark tb eMark
     -- Duplicate above
     else textBufferKeepSelection tb False False $
          do 
            -- Record start iter mark.
            sMark <- textBufferCreateMark tb Nothing sIter False -- make mark after insert

            -- Insert duplicate content.
            textBufferInsertRange tb sIter sIter eIter

            -- Add newline before start iter when start iter at last line of buffer.
            when lastLine $ textBufferInsertNewlineAtMark tb sMark

textViewDupLinesBelow, textViewDupLinesAbove :: TextViewClass self => self -> IO ()
textViewDupLinesBelow tv = textViewDupLines tv True
textViewDupLinesAbove tv = textViewDupLines tv False

-- | Transposes lines.
-- Just transposes current line to below if haven't selection anything.
-- Otherwise transposes selection lines to below.
textViewTraLines :: TextViewClass self => self -> Bool -> IO ()
textViewTraLines tv traBelow = do
  -- Get text buffer.
  tb <- textViewGetBuffer tv
  
  -- Get selection bound line.
  (sIter, eIter, firstLine, lastLine) <- textBufferGetSelectionBoundLines tb
  
  -- Transposes lines.
  if traBelow
     then 
         unless lastLine $ textBufferKeepSelection tb False False $
                do
                  -- Record iter mark.
                  sMark <- textBufferCreateMark tb Nothing sIter False
                  eMark <- textBufferCreateMark tb Nothing eIter False

                  -- Get start and end iter to move.
                  trasStartIter <- textIterCopy eIter
                  trasEndIter   <- textIterCopy eIter
                  atLastLine    <- textIterIsLastLine eIter
                  chars         <- textIterGetCharsInLine eIter
                  textIterSetLineOffset trasEndIter chars

                  -- Move range.
                  textBufferMoveRange tb sIter trasStartIter trasEndIter

                  -- Adjust when reach last line.
                  when atLastLine $ do
                    -- Insert new line.
                    textBufferInsertNewlineAtMark tb sMark

                    -- Delete last char.
                    textBufferDeleteLastChar tb

                  -- Scroll screen to display iter.
                  textViewScrollMarkOnscreen tv eMark
     else 
         unless firstLine $ textBufferKeepSelection tb True True $
                do
                  -- Record iter mark.
                  sMark <- textBufferCreateMark tb Nothing sIter True
                  eMark <- textBufferCreateMark tb Nothing eIter True

                  -- Get start and end iter to move.
                  trasStartIter <- textIterCopy sIter
                  trasEndIter <- textIterCopy sIter
                  textIterBackwardLine trasStartIter

                  -- Move range.
                  textBufferMoveRange tb eIter trasStartIter trasEndIter

                  -- Adjust when reach last line.
                  when lastLine $ do
                    -- Insert newline.
                    textBufferInsertNewlineAtMark tb eMark

                    -- Delete last char.
                    textBufferDeleteLastChar tb

                  -- Scroll screen to display iter.
                  textViewScrollMarkOnscreen tv sMark

textViewTraLinesBelow, textViewTraLinesAbove :: TextViewClass self => self -> IO ()
textViewTraLinesBelow tv = textViewTraLines tv True
textViewTraLinesAbove tv = textViewTraLines tv False

-- | Delete lines.
-- Just delete current line to below if haven't selection anything.
-- Otherwise delete selection lines to below.
-- Return 'False' if in uneditable area.
textViewDelLines :: TextViewClass self => self -> IO Bool
textViewDelLines tv = do
  -- Get text buffer.
  tb <- textViewGetBuffer tv

  -- Get selection bound line.
  (sIter, eIter, _, lastLine) <- textBufferGetSelectionBoundLines tb

  isEditable <- textIterBoundIsEditable (sIter, eIter)
  if isEditable  
     -- Just delete lines when in editable area.
     then do
       -- Delete lines.
       textBufferDelete tb sIter eIter
       
       -- Adjust when reach last line.
       when lastLine $ do
         -- Delete last char.
         textViewDeleteBackwardChar tv False
       
         -- Move to line start.
         iter <- textViewGetTextIter tv
         textIterBackwardToLineStart iter
         textBufferPlaceCursor tb iter

       return True
     else return False

-- | Smart home.
textViewSmartHome :: TextViewClass self => self -> IO ()
textViewSmartHome tv = do
  -- Get iter information.
  ti <- textViewGetTextIter tv
  lo <- textIterGetLineOffset ti
  tb <- textViewGetBuffer tv

  -- Move to line end and got search limit.
  textIterForwardToLineEnd_ ti 
  limit <- textIterCopy ti

  -- Init to line start and search forward visible char.
  textIterBackwardToLineStart ti

  -- Move iter.
  ifM (textIterForwardFindChar ti (/= ' ') (Just limit))
           (do
             nlo <- textIterGetLineOffset ti
             if lo == nlo || nlo == 1
                -- Move to line start when iter at indent position
                -- or current line is empty line.
                then textIterBackwardToLineStart ti
                -- Otherwise move to line indent position.
                else textIterSetLineOffset ti nlo)
           -- Move to line start when search failed.
           (textIterBackwardToLineStart ti)

  -- Move cursor to iter place.
  textBufferPlaceCursor tb ti

-- | Smart end.
textViewSmartEnd :: TextViewClass self => self -> IO ()
textViewSmartEnd tv = do
  -- Get iter information.
  ti <- textViewGetTextIter tv
  lo <- textIterGetLineOffset ti
  tb <- textViewGetBuffer tv

  -- Move to line start and got search limit
  textIterBackwardToLineStart ti
  limit <- textIterCopy ti

  -- Init to line end and search backward visible char.
  textIterForwardToLineEnd_ ti

  -- Move iter.
  ifM (textIterBackwardFindChar ti (/= ' ') (Just limit))
          (do
            nlo <- textIterGetLineOffset ti
            let nl = nlo + 1 
            if lo == nl || nlo == 0
               -- Move to line end when iter at last visible char
               -- or current line is empty line.
               then textIterForwardToLineEnd_ ti
               -- Otherwise move to last visible char.
               else textIterSetLineOffset ti nl)
           -- Move to line end when search failed.
           (textIterForwardToLineEnd_ ti)

  -- Move cursor to iter place.
  textBufferPlaceCursor tb ti

-- | Scroll vertical.
textViewScrollVertical :: (TextViewClass tvc, ScrolledWindowClass swc) => tvc -> swc -> Double -> IO ()
textViewScrollVertical tv sw inc = do
  -- Get adjustment arguments.
  aj <- scrolledWindowGetVAdjustment sw
  ps <- adjustmentGetPageSize aj
  vl <- adjustmentGetValue aj
  ur <- adjustmentGetUpper aj
  lr <- adjustmentGetLower aj

  -- Get text iter arguments.
  ti <- textViewGetTextIter tv
  tb <- textViewGetBuffer tv
  (y, lnH) <- textViewGetLineYrange tv ti

  -- Adjust scroll value.
  let iv = vl + inc
      av = if inc >= 0
              -- Don't bigger than upper value when scroll up.
              then min iv (ur - i2d lnH)
              -- Don't less than lower value when scroll down.
              else max iv lr
  adjustmentSetValue aj av

  -- Adjust iter position.
  top <- adjustmentGetValue aj
  let bottom = top + ps
  if top >= i2d y
     -- Adjust iter to first line in view visual port 
     -- when iter is up out view visual port.
     then do
       (nti, _) <- textViewGetLineAtY tv (truncate top)
       textBufferPlaceCursor tb nti
     -- Adjust iter to last line in view visual port
     -- when iter is down out view virtual port.
     else when (bottom <= i2d (y + lnH)) $
          do
            (nti, _) <- textViewGetLineAtY tv (truncate (bottom - i2d lnH))
            textBufferPlaceCursor tb nti

-- | Toggle selection.
-- Return True if selection active.
-- Otherwise, return False. 
textViewToggleSelectionMark :: TextViewClass self => self -> IO Bool
textViewToggleSelectionMark tv = do
  -- Get text buffer.
  tb <- textViewGetBuffer tv

  -- Whether selection active.
  selectionActive <- textBufferHasSelection tb

  -- Toggle selection iter.
  if selectionActive 
     then do
       textViewCancelSelectionMark tv
       return False
     else do
       textViewSetSelectionMark tv
       return True

-- | Exchange selection mark.
textViewExchangeSelectionMark :: TextViewClass self => self -> IO ()  
textViewExchangeSelectionMark tv = do
  -- Get text buffer.
  tb <- textViewGetBuffer tv

  -- Whether selection active.
  selectionActive <- textBufferHasSelection tb

  -- Exchange selection mark when selection active.
  when selectionActive $ do
    -- Get insert and bound iter.
    (sIter, eIter) <- textBufferGetSelectionBounds tb
    -- Get new iter along with selection direction.
    selectionAbove <- textBufferIsEqualInsertIter tb sIter
    let (startIter, endIter) = 
            if selectionAbove
               then (eIter, sIter)
               else (sIter, eIter)
    -- Set selection mark.
    textBufferSetSelectionIter tb endIter
    -- Set selection range.
    textBufferSelectRange tb startIter endIter
    -- Move current cursor in screen.
    textViewScrollCursorOnscreen tv

-- | Scroll text view the minimum distance such that cursor is contained within the visible area of widget.
textViewScrollCursorOnscreen :: TextViewClass self => self -> IO ()
textViewScrollCursorOnscreen tv =
    textViewScrollMarkOnscreen tv 
        =<< (<=<) textBufferGetInsert textViewGetBuffer tv

-- | Set selection.
textViewSetSelectionMark :: TextViewClass self => self -> IO ()
textViewSetSelectionMark tv = do
  -- Get text buffer.
  tb <- textViewGetBuffer tv
  
  -- Cancel selection first.
  textBufferCancelSelection tb
  
  -- Set new selection iter.
  textBufferSetSelectionIter tb 
      =<< textViewGetTextIter tv
  
-- | Cancel selection.
textViewCancelSelectionMark :: TextViewClass self => self -> IO ()  
textViewCancelSelectionMark tv = do
  -- Get text buffer.
  tb <- textViewGetBuffer tv
  
  -- Cancel selection first.
  textBufferCancelSelection tb
  
  -- Cancel new selection iter.
  textBufferRemoveSelectionIter tb

-- | Apply selection.
textViewApplySelectionMark :: TextViewClass self => self -> IO ()  
textViewApplySelectionMark tv = do
  tb <- textViewGetBuffer tv
  textBufferGetSelectionIter tb >>= 
    (?>= (\markIter -> do
            -- Get insert iter.
            insertIter <- textBufferGetInsertIter tb
            -- Selection range.
            textBufferSelectRange tb insertIter markIter))

-- | Text view newline.
textViewNewLine :: TextViewClass self => self -> IO ()  
textViewNewLine tv = 
  textViewInsertAtCursor tv "\n"

-- | Insert string at cursor.
textViewInsertAtCursor :: TextViewClass self => self -> String -> IO ()  
textViewInsertAtCursor tv str = do
  -- Cancel selection first.
  tb <- textViewGetBuffer tv
  textBufferCancelSelection tb
  -- Insert string.
  textBufferInsertAtCursor tb str 
  -- Scroll cursor to screen.
  textViewScrollCursorOnscreen tv

-- | Move to next line and then opens a line.
textViewOpenNewlineBelow :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewOpenNewlineBelow tv swc = do
  -- Get text buffer and iter.
  tb   <- textViewGetBuffer tv
  iter <- textViewGetTextIter tv
  -- Whether in blank line.
  inBlankLine <- textIterInBlankLine iter
  -- Move iter to line end and insert newline.
  textIterForwardToLineEnd_ iter
  textBufferInsert tb iter "\n"
  -- Don't forward line at blank line.
  unless inBlankLine $ textViewForwardLine tv swc

-- | Move to previous line and then opens a line.
textViewOpenNewlineAbove :: (TextViewClass self, ScrolledWindowClass swc) => self -> swc -> IO ()
textViewOpenNewlineAbove tv swc = do
  -- Get text buffer and iter.
  tb   <- textViewGetBuffer tv
  iter <- textViewGetTextIter tv
  -- Move iter to line start and insert newline.
  textIterBackwardToLineStart iter
  textBufferInsert tb iter "\n"
  -- Backward line.
  textViewBackwardLine tv swc
  
-- | Get cursor align.
textViewGetCursorAlign :: TextViewClass tvc => tvc -> IO (Double, Double)
textViewGetCursorAlign tv = do
  -- Get adjustment value.
  hAdj <- textViewGetHadjustment tv
  vAdj <- textViewGetVadjustment tv
  hVal <- adjustmentGetValue hAdj
  vVal <- adjustmentGetValue vAdj
  hSize <- adjustmentGetPageSize hAdj
  vSize <- adjustmentGetPageSize vAdj

  -- Get cursor coordinate.
  ti <- textViewGetTextIter tv
  (Rectangle ix iy _ _) <- textViewGetIterLocation tv ti

  -- Get cursor alignment.
  let xAlign = if hSize == 0 then 0 else (i2d ix - hVal) / hSize
      yAlign = if vSize == 0 then 0 else (i2d iy - vVal) / vSize

  return (xAlign, yAlign)

-- | Set cursor align.
textViewSetCursorAlign :: TextViewClass tvc => tvc -> (Double, Double) -> IO ()
textViewSetCursorAlign tv align = do
  tm <- (<=<) textBufferGetInsert textViewGetBuffer tv
  textViewScrollToMark tv tm 0.0 (Just align)

-- | Move to `first iter` of buffer.
textIterBackwardToStart :: TextIter -> IO ()  
textIterBackwardToStart ti = do
  textIterSetLine ti 0
  textIterBackwardToLineStart ti

-- | Forward to line end, like textIterForwardToLineEnd, 
-- but don't move to next line when current line is empty.
textIterForwardToLineEnd_ :: TextIter -> IO ()
textIterForwardToLineEnd_ ti = 
    textIterSetLineOffset ti 
        =<< textIterGetCharsInLine_ ti

-- | In blank line.
textIterInBlankLine :: TextIter -> IO Bool
textIterInBlankLine =
    (<<<=) (1 ==) textIterGetCharsInLine 

-- | Is first line.
textIterIsFirstLine :: TextIter -> IO Bool  
textIterIsFirstLine =
    (<<<=) (0 ==) textIterGetLine

-- | Is last line.
textIterIsLastLine :: TextIter -> IO Bool
textIterIsLastLine ti = do
  line  <- textIterGetLine ti
  lines <- (<=<) textBufferGetLineCount textIterGetBuffer ti
  return $ line == (lines - 1)

-- | Backward to line start.
textIterBackwardToLineStart :: TextIter -> IO ()  
textIterBackwardToLineStart ti =
    textIterSetLineOffset ti 0

-- | Return number of characters in this line.
-- The return value not includes delimiters.
textIterGetCharsInLine_ :: TextIter -> IO Int
textIterGetCharsInLine_ ti = do
  -- Get number of characters in this line.
  chars <- textIterGetCharsInLine ti
  ifM (textIterIsLastLine ti)
      -- Return chars if at last line.
      (return chars)
      -- Otherwise not includes delimiter.
      (return $ chars - 1)

-- | Whether selection iter is editable.
textIterBoundIsEditable :: (TextIter, TextIter) -> IO Bool
textIterBoundIsEditable (startIter, endIter) = 
    liftM2 (&&) 
           (textIterEditable startIter True)
           (textIterEditable endIter True)

