{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import Data.Monoid
import System.IO
import Data.Bits
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Control.Monad

import Graphics.X11.Types
import Graphics.X11.ExtraTypes

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings, removeKeys)
import XMonad.Util.Run (spawnPipe, safeSpawn, runProcessWithInput)
import XMonad.Util.Cursor
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows
import qualified XMonad.Actions.CycleWS as Cycle
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.Minimize
import XMonad.Actions.Minimize
import XMonad.Layout.BoringWindows
import XMonad.Actions.FloatSnap
import XMonad.Actions.WithAll

import XMonad.Prompt.Window
import XMonad.Prompt.MyWindow
import XMonad.Prompt

import XMonad.Util.Ignore

import XMonad.Actions.MyNotify
import qualified XMonad.Util.ExtensibleState as XS
import qualified DBus.Notify as N

import XMonad.Actions.CycleRecentWS
import qualified XMonad.Actions.MyFlexibleResize as Flex
import XMonad.Actions.CopyWindow

import XMonad.Actions.StatefulNotify

import qualified Data.Map as M

import XMonad.Util.Maybe
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.MyManageHelpers
import qualified DBus.Client as C
import XMonad.Hooks.RefocusLast (refocusLastLogHook)

modKey :: KeyMask
modKey = mod4Mask

backgroundImage :: String
backgroundImage = "~/Pictures/red-space.jpg"

theTerminal :: String
theTerminal = "terminator"

myWorkspaces :: [String]
myWorkspaces = ["Chrome", "Discord", "3", "Steam"] ++ map show [5 .. 9]

myLayout = boringAuto $ minimize $ Full ||| tiled ||| Mirror tiled
  where
    tiled = Tall nmaster delta ratio
    -- see XMonad.Config.layout
    nmaster = 1
    delta = 3/100
    ratio = 1/2

main :: IO ()
main = do
  xmonad $
    withUrgencyHook LibNotifyUrgencyHook $
    ewmh $
    docks
      def
        { layoutHook = myLayout
        , modMask = modKey -- Rebind Mod to the Windows key
        , terminal = theTerminal
        , borderWidth = 1
        , normalBorderColor = "#449944"
        , focusedBorderColor = "#994444"
        , workspaces = myWorkspaces
        , XMonad.startupHook = Main.startupHook
        , manageHook = composeAll [manageHook def, switchToWS, floatIt, closeSteamFriends, sinkIt]
        , logHook = unhideLogHook <+> logHook def
        , clickJustFocuses = False
        }
      `removeKeys` badKeys
      `additionalKeys` [
          ((0,                    xK_Print  ), spawn "flameshot gui")
        , ((shiftMask,            xK_Print  ), spawn "flameshot full -c")

        , ((modKey,               xK_Return ), spawn theTerminal)
        , ((modKey,               xK_r      ), spawn "rofi -show drun")
        , ((modKey .|. shiftMask, xK_r      ), spawn "rofi -show run")

        , ((modKey,               xK_Right  ), nextWS)
        , ((modKey,               xK_Left   ), prevWS)
        , ((modKey .|. shiftMask, xK_t      ), withFocused toggleFloat)
        , ((modKey,               xK_space  ), sinkAll >> sendMessage NextLayout)
        , ((modKey,               xK_b      ), runAutorun)
        , ((modKey,               xK_n      ), withFocused minimizeWindow)
        , ((modKey,               xK_grave  ), withFirstMinimized maximizeWindowAndFocus)
        , ((modKey .|. shiftMask, xK_grave  ), withLastMinimized maximizeWindowAndFocus)

        , ((modKey,               xK_w      ), spawn "rofi -show windowcd")
        , ((modKey .|. shiftMask, xK_w      ), spawn "rofi -show window")

        , ((modKey,               xK_s      ), notifyWS)
        , ((modKey,               xK_t      ), notifyTime)

        , ((modKey .|. controlMask, xK_t    ), toggleSystray)

        , ((modKey,               xK_l      ), spawn "xsecurelock")

        , ((modKey,               xK_j      ), withMinimized $ windows . focusDownIgnoring)
        , ((modKey,               xK_k      ), withMinimized $ windows . focusUpIgnoring)

        , ((modKey,               xK_i      ), windows copyToAll)   -- "mark sticky"
        , ((modKey .|. shiftMask, xK_i      ), killAllOtherCopies)  -- "unmark sticky"

        , ((modKey,               xK_z      ), spawn "~/programme/bin/boomer")  -- desktop zoom app, https://github.com/tsoding/boomer

        , ((modKey .|. shiftMask, xK_n      ), withFocused $ void <$> (getName >=> \n -> notifySendIcon "window title" (show n) [] Nothing))

        , ((modKey,               xK_u      ), withFocused maximizeWindowAndFocus)  -- "unminimize" the focused window; useful when you have a notification from a minimized window

        , ((modKey,               xK_q      ), restartXMonad)


        -- bluetooth media controls, see https://work.lisk.in/2020/05/06/linux-media-control.html
        , ((0,             xF86XK_AudioPlay ), spawn "liskin-media play")
        , ((0,             xF86XK_AudioPause), spawn "liskin-media play")
        , ((0,             xF86XK_AudioStop ), spawn "liskin-media stop")
        , ((0,             xF86XK_AudioNext ), spawn "liskin-media next")
        , ((0,             xF86XK_AudioPrev ), spawn "liskin-media prev")
        ]
      `additionalKeys` [
          ((modKey,               k         ), windows (W.view ws) >> notifyWS)
          | (k, ws) <- zip [xK_1..] myWorkspaces
        ]
      `additionalMouseBindings` [
        ((modKey,                 button1), \w -> focus w >> mouseMoveWindow w)
      , ((modKey,                 button2), \w -> focus w >> snapMagicMouseResize 0.5 Nothing Nothing w)
      , ((modKey,                 button3), \w -> focus w >> Flex.mouseResizeEdgeWindow 0.5 w)
      , ((modKey .|. shiftMask,   button4), const prevWS)
      , ((modKey .|. shiftMask,   button5), const nextWS)
      ]

badKeys :: [(KeyMask, KeySym)]
badKeys = [
    (modKey .|. shiftMask, xK_Return)
  , (modKey .|. shiftMask, xK_space )
  , (modKey,               xK_Tab   )
  , (modKey .|. shiftMask, xK_Tab   )
  , (modKey,               xK_m     )
  , (modKey,               xK_Return)
  , (modKey,               xK_p     )
  ] ++
  [(modKey, k) | k <- [xK_1 .. xK_9]]

startupHook :: X ()
startupHook = do
  trace "START"
  setDefaultCursor xC_arrow
  XS.put . DBusConnection =<< io C.connectSession
  spawn $ "xloadimage -onroot -fullscreen " ++ backgroundImage
  spawn "~/programme/bin/autorun.sh"
  setWMName "LG3D"
  void $ notifySend "XMonad" "startup finished" []

restartXMonad :: X ()
restartXMonad = do
  conn <- XS.get
  case conn of
    DBusConnection c -> io $ C.disconnect c
    _ -> return ()
  spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    Just idx <- W.findTag w <$> gets windowset
    void $ notifySendHints (show name) ("Workspace " ++ idx) [] [N.Urgency N.Critical]


switchToWS :: ManageHook
switchToWS = composeAll [
    className =? "google-chrome" --> doShift "Chrome"
  , className =? "discord"       --> doShift "Discord"
  , className =? "Steam"         --> doShift "Steam"
  ]

floatIt :: ManageHook
floatIt = composeAll [
    className =? "Gimp" <&&> stringProperty "WM_NAME" /=? "GNU Image Manipulation Program"  --> doFloat
  , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog"                               --> doFloat
  , stringProperty "WM_NAME" =? "Picross Touch Configuration"                               --> doFloat
  , className =? "flameshot"                                                                --> doFloat
  , isDialog --> doFloat
  ]

sinkIt :: ManageHook
sinkIt = composeAll [
    stringProperty "WM_NAME" =? "SHENZHEN I/O" --> doSink
  ]

closeSteamFriends :: ManageHook
closeSteamFriends = composeAll [
    stringProperty "WM_NAME" =? "Friends List" <&&> className =? "Steam" --> doIgnore
  ]

minimizeInHook :: ManageHook
minimizeInHook = ask >>= \w -> liftX (minimizeWindow w) >> idHook

toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    ( \s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s
    )

runAutorun :: X ()
runAutorun = spawn "~/programme/bin/autorun.sh"

toggleCompton :: X ()
toggleCompton = spawn "pgrep picom && killall picom || picom >> ~/.autorun.log"

myXPConfig :: XPConfig
myXPConfig = def {
    font = "xft:Liberation Sans:pixelsize=12"
  , autoComplete = Just 1000
  }

goToWindow :: X ()
goToWindow = do
  Just window <- getWindowPrompt myXPConfig wsWindows
  maximizeWindow window
  windows $ W.focusWindow window

findWindow :: X ()
findWindow = do
  Just window <- getWindowPrompt myXPConfig allWindows
  maximizeWindow window
  windows $ \ws -> fromMaybe ws (goToWorkspaceOf window ws)
  windows $ W.focusWindow window

getTimeString :: String -> X String
getTimeString fmt = io $ formatTime defaultTimeLocale fmt <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)

goToWorkspaceOf :: Window -> WindowSet -> Maybe WindowSet
goToWorkspaceOf window ws = flip W.view ws <$> W.findTag window ws


newtype TimeNotification = TimeNotification (Maybe N.Notification) deriving (Typeable)

instance ExtensionClass TimeNotification where
  initialValue = TimeNotification Nothing

notifyTime :: X ()
notifyTime = replaceStateNotification "Time" getN (getTimeString "%a %d.%m.%Y: %T") (TimeNotification . Just)
  where
    getN = do
      TimeNotification oldN <- XS.get
      return oldN

newtype WSNotification = WSNotification (Maybe N.Notification) deriving (Typeable)

instance ExtensionClass WSNotification where
  initialValue = WSNotification Nothing

notifyWS :: X ()
notifyWS = replaceStateNotification "Workspace" getN getTag (WSNotification . Just)
  where
    getN = do
      WSNotification oldN <- XS.get
      return oldN
    getTag = withWindowSet $ return . W.currentTag

nextWS :: X ()
nextWS = Cycle.nextWS >> notifyWS

prevWS :: X ()
prevWS = Cycle.prevWS >> notifyWS

notifyOutput :: String -> X ()
notifyOutput s = do
  out <- runProcessWithInput s [] ""
  h <- spawnPipe "xmessage -file -"
  io $ do
    hPutStr h out
    hClose h

toggleSystray :: X ()
toggleSystray = do
  spawn "pgrep stalonetray && killall stalonetray || stalonetray >> ~/.autorun.log"

unhideLogHook :: X ()
unhideLogHook = do
  refocusLastLogHook
  -- withFocused maximizeHiddenFocused

maximizeHiddenFocused :: Window -> X ()
maximizeHiddenFocused win = do
  withLastMinimized $ \last_ -> do
    when (last_ /= win) $
      maximizeWindow win
