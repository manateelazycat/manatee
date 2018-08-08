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

module Manatee.Toolkit.General.Time where

import Control.Applicative
import Control.Monad
import Manatee.Toolkit.Date.DateTime hiding (getCurrentTime)
import Data.Time
import System.Glib.GDateTime
import System.Locale
import System.Time

instance Show GTimeVal where
  show GTimeVal {gTimeValSec  = seconds
                ,gTimeValUSec = microseconds}
      = show seconds ++ " : " ++ show microseconds

-- | Get local time.
getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime

-- | Different two time with second.
diffUTCTimeWithSecond :: UTCTime -> UTCTime -> Integer
diffUTCTimeWithSecond t1 t2 = diffUTCTimeWithQuantify t1 t2 1

-- | Different two time with millisecond.
diffUTCTimeWithMillisecond :: UTCTime -> UTCTime -> Integer
diffUTCTimeWithMillisecond t1 t2 = diffUTCTimeWithQuantify t1 t2 (10 ^ 3)

-- | Different two time with microsecond.
diffUTCTimeWithMicrosecond :: UTCTime -> UTCTime -> Integer
diffUTCTimeWithMicrosecond t1 t2 = diffUTCTimeWithQuantify t1 t2 (10 ^ 6)

-- | Different two time with quantify.
diffUTCTimeWithQuantify :: UTCTime -> UTCTime -> NominalDiffTime -> Integer
diffUTCTimeWithQuantify t1 t2 quantify = truncate $ (quantify *) $ diffUTCTime t1 t2

-- | Convert GTimeVale to ClockTime.
gTimeValToClockTime :: GTimeVal -> ClockTime
gTimeValToClockTime GTimeVal {gTimeValSec  = seconds
                             ,gTimeValUSec = microseconds} = 
  TOD (toInteger seconds) (toInteger microseconds * 1000)

-- | Get local time stamp with your format!
getTimeStamp :: String -> IO String
getTimeStamp format = 
  formatTime <$> pure defaultTimeLocale 
             <*> pure format
             <*> (utcToLocalTime <$> getCurrentTimeZone 
                                 <*> getCurrentTime)

-- | Get diff time that seconds from 1970.
getSecondsTimeStamp :: Integer -> String -> IO String
getSecondsTimeStamp seconds format = 
  formatTime <$> pure defaultTimeLocale
             <*> pure format
             <*> (utcToLocalTime <$> getCurrentTimeZone 
                                 <*> pure (fromSeconds seconds))

-- | Convert seconds to (day/hour/min/second) time.
secondToDaytime :: Int -> String
secondToDaytime seconds =
       showTime day  "d "
    ++ showTime hour "h "
    ++ showTime min  "m "
    ++ showTime sec  "s"
    where sec  = seconds `mod` 60
          min  = seconds `mod` 3600  `div` 60
          hour = seconds `mod` 86400 `div` 3600
          day  = seconds `div` 86400
          showTime time unit =
              if time == 0 then "" else show time ++ unit

