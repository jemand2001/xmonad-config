module XMonad.Util.Time where


import Data.Time
import XMonad.Core

getTimeString :: String -> X String
getTimeString fmt = formatTime defaultTimeLocale fmt <$> now

now :: X LocalTime
now = io $ utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime
