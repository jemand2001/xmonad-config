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
boomerInstall = Just "~/programme/bin/boomer"

-- |your preferred screen locker
screenLock :: Maybe String
screenLock = Just "xsecurelock"

-- |your preferred terminal
terminal :: String
terminal = "terminator"

-- |the font to be used in xmonad's prompt system
xPromptFont :: String
xPromptFont = "xft:Liberation Sans:pixelsize=12"
