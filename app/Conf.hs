module Conf where
import Graphics.X11.Types (mod4Mask, KeyMask)

modKey :: KeyMask
modKey = mod4Mask

backgroundImage :: String
backgroundImage = "~/Pictures/red-space.jpg"

autorun :: String
autorun = "~/programme/bin/autorun.sh"

boomerInstall :: Maybe String
boomerInstall = Just "~/programme/bin/boomer"

screenLock :: Maybe String
screenLock = Just "xsecurelock"

terminal :: String
terminal = "terminator"

xPromptFont :: String
xPromptFont = "xft:Liberation Sans:pixelsize=12"
