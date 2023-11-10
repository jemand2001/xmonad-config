{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module XMonad.Actions.Countdown (notifyCountdowns) where

import System.IO

import XMonad.Core
import Data.Time.Calendar
import Data.Time

import XMonad.Actions.StatefulNotify
import Data.List.Split (splitOn)
import Data.Char (isSpace)

import XMonad.Util.Time

import XMonad.Util.Paths

type Countdown = (String, Day)

notifyCountdowns :: FilePath -> X ()
notifyCountdowns p = do
  path <- tildeExpand p
  replaceStateNotification "countdowns" "Countdowns" $ do
    countdowns <- io $ getCountdowns path
    today <- localDay <$> now
    return $ unlines [
        title ++ ": " ++ show (diffDays day today) ++ " days" |
        (title, day) <- countdowns
      , day >= today
      ]

getCountdowns :: FilePath -> IO [Countdown]
getCountdowns p = do
  f <- openFile p ReadMode
  contents <- hGetContents' f
  hClose f
  return $ map parseCountdown $ lines contents
  where
    parseCountdown :: String -> Countdown
    parseCountdown l = (rest, date)
      where
        [y, m, d] = map read $ splitOn "-" $ head $ words l
        date = fromGregorian y (fromInteger m) (fromIntegral d)
        rest = dropWhile isSpace $ dropWhile (not . isSpace) l
