{-# LANGUAGE LambdaCase, TupleSections #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-binds -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}

module XMonad.Actions.PulseAudio where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.List.Split
import Data.Maybe
import XMonad.Util.Run
import XMonad.Core
import Data.List

data SinkState = Suspended | Running

class IsId a where
  getId :: a -> String

instance IsId String where
  getId = id

instance IsId Int where
  getId = show

data Sink = Sink
  { index :: Int,
    state :: SinkState,
    name :: String,
    description :: String,
    channels :: [String],
    muted :: Bool,
    volume :: [(String, Double)]
  }

getSinks :: IO [Sink]
getSinks = do
  mapMaybe readSink . splitOn "Sink" <$> runProcessWithInput "pactl" ["list", "sinks"] ""

readSink :: String -> Maybe Sink
readSink s = do
  let (header : l) = map strip $ lines s
  let idx = read $ drop 6 header
  state <- getField "State" l $ \case
    "SUSPENDED" -> Just Suspended
    "RUNNING" -> Just Running
    _ -> Nothing
  name <- getField "Name" l Just
  description <- getField "Description" l Just
  channelMap <- getField "Channel Map" l $ Just . splitOn ","
  -- these checks are to make sure the important bits of the volume line aren't wrapped around
  guard $ length channelMap == 2
  guard $ not $ any ((> 15) . length) channelMap
  mute <- getField "Mute" l $ \case
    "no" -> Just False
    "yes" -> Just True
    _ -> Nothing
  volume <- getField "Volume" l $ \line -> mapM readVolume
    $ splitOn "," line
  return $ Sink idx state name description channelMap mute volume

getLinePrefixed :: Eq a => [a] -> [[a]] -> Maybe [a]
getLinePrefixed pref l = case filter (isPrefixOf pref) l of
  [x] -> Just x
  _ -> Nothing

getField :: String -> [String] -> (String -> Maybe a) -> Maybe a
getField fieldName l f = do
  getLinePrefixed (fieldName ++ ": ") l >>= f

strip, stripFront, stripBack :: String -> String
strip = stripFront . stripBack
stripFront = dropWhile isSpace
stripBack = reverse . stripFront . reverse

getDefaultSink :: IO String
getDefaultSink = do
  strip <$> runProcessWithInput "pactl" ["get-default-sink"] ""

getVolume :: IsId a => a -> IO Double
getVolume n = do
  output <- runProcessWithInput "pactl" ["get-sink-volume", getId n] ""
  let dropped = drop 8 output
  let stripped = strip dropped
  let channels = splitOn "," stripped
  let volumes = map readVolume channels
  let theOne = map (snd <$>) volumes
  return $ head $ catMaybes theOne ++ [0]

setVolume :: IsId a => a -> Double -> IO ()
setVolume sink x = do
  spawn $ unwords ["pactl", "set-sink-volume", getId sink, show x ++ "%"]

raiseVolume, lowerVolume :: MonadIO m => Double -> m Double
raiseVolume delta = liftIO $ do
  sinkName <- getDefaultSink
  vol <- getVolume sinkName
  let newVol = delta + vol
  setVolume sinkName newVol
  return newVol
lowerVolume = raiseVolume . negate

muteVolume :: MonadIO m => m ()
muteVolume = liftIO $ do
  sink <- getDefaultSink
  spawn $ unwords ["pactl", "set-sink-mute", sink, "toggle"]
  return ()

readVolume :: String -> Maybe (String, Double)
readVolume =
    ( \case
        [channel, vol] -> case splitOn "/" vol of
          [_, percentage, _] -> Just $ (channel,) $ read $ filter (/= '%') percentage
          _ -> Nothing
        _ -> Nothing
    )
      . splitOn ":"
      . strip
