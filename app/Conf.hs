{-|
Module     : Conf
Description: Configurable variables

This module defines some variables which are used elsewhere
and may be adjusted to suit your needs.

Variables declared with 'Maybe' mark optional features;
if you do not wish to use any of these you can set them to Nothing
-}
module Conf where
import Graphics.X11.Types (mod4Mask, KeyMask)

-- |the key that activates most wm shortcuts
--
-- mod4 is the "windows key"
modKey :: KeyMask
modKey = mod4Mask

-- |the path to your preferred wallpaper
backgroundImage :: String
backgroundImage = "~/Pictures/red-space.jpg"

-- |a script to be run at startup, as well as whenever you press mod+b
autorun :: String
autorun = "~/programme/bin/autorun.sh"

-- |your installation of the desktop zoom app `boomer`
--
-- see also https://github.com/tsoding/boomer
boomerInstall :: Maybe String
boomerInstall = Just "/usr/bin/boomer"

-- |your preferred screen locker
screenLock :: Maybe String
screenLock = Just "XSECURELOCK_PASSWORD_PROMPT=time xsecurelock"

-- |your preferred terminal
terminal :: String
terminal = "terminator"

-- |the font to be used in xmonad's prompt system
xPromptFont :: String
xPromptFont = "xft:Liberation Sans:pixelsize=12"

-- |the file automatically run programs will log into
autorunLog :: Maybe String
autorunLog = Just "~/.autorun.log"

-- |your system tray executable
systemTray :: Maybe String
systemTray = Just "stalonetray"

-- |the command to restart xmonad
restartCommand :: String
restartCommand = "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

-- |a command to centrally control media playback
--
-- must support play, stop, next, and prev
-- for example, liskin-media from https://github.com/liskin/dotfiles/blob/15c2cd83ce7297c38830053a9fd2be2f3678f4b0/bin/liskin-media
mediaController :: Maybe String
mediaController = Just "liskin-media"

-- |the names of your workspaces
--
-- my setup automatically shifts some programs to the named workspaces
workspaces :: [String]
workspaces = ["Browser", "Chat", "Code", "Games"] ++ map show [5 :: Int .. 9]

-- |a way to get the file that contains your countdowns (see 'XMonad.Actions.Countdown.notifyCountdowns')
--
-- the format is: 1 countdown per line, each line as "YYYY-MM-DD <description>"
countdownFile :: Maybe String
countdownFile = Just "~/.countdowns"
