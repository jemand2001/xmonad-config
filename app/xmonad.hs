{-# LANGUAGE TupleSections #-}
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
import Data.Functor

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

import XMonad.Prompt

import XMonad.Util.Ignore

import XMonad.Actions.MyNotify
import qualified XMonad.Util.ExtensibleState as XS
import qualified DBus.Notify as N

import XMonad.Actions.CycleRecentWS
import qualified XMonad.Actions.MyFlexibleResize as Flex
import XMonad.Actions.CopyWindow

import XMonad.Actions.StatefulNotify
import XMonad.Actions.DBus
import XMonad.Layout.NoBorders

import qualified Data.Map as M

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.RefocusLast (refocusLastLogHook)

import qualified Conf
import XMonad.Hooks.MyManageHelpers
import XMonad.Layout.LayoutHints

modKey :: KeyMask
modKey = Conf.modKey

myWorkspaces :: [String]
myWorkspaces = ["Browser", "Discord", "3", "Steam"] ++ map show [5 .. 9]

myLayout = layoutHints $ boringAuto $ minimize $ Full ||| tiled ||| Mirror tiled
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
        , terminal = Conf.terminal
        , borderWidth = 1
        , normalBorderColor = "#449944"
        , focusedBorderColor = "#994444"
        , workspaces = myWorkspaces
        , XMonad.startupHook = Main.startupHook
        , manageHook = composeAll [manageHook def, switchToWS, floatIt, closeSteamFriends, sinkIt, removeBorders]
        , logHook = unhideLogHook <+> logHook def
        , clickJustFocuses = False
        }
      `removeKeys` badKeys
      `additionalKeys` [
          ((0,                    xK_Print  ), spawn "flameshot gui")
        , ((shiftMask,            xK_Print  ), spawn "flameshot full -c")

        , ((modKey,               xK_Return ), spawn Conf.terminal)
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

        , ((modKey,               xK_j      ), withMinimized $ windows . focusDownIgnoring)
        , ((modKey,               xK_k      ), withMinimized $ windows . focusUpIgnoring)

        , ((modKey,               xK_i      ), windows copyToAll)   -- "mark sticky"
        , ((modKey .|. shiftMask, xK_i      ), killAllOtherCopies)  -- "unmark sticky"

        , ((modKey .|. shiftMask, xK_n      ), notifyWindowName)

        , ((modKey,               xK_u      ), withFocused maximizeWindowAndFocus)  -- "unminimize" the focused window; useful when you have a notification from a minimized window

        , ((modKey,               xK_q      ), restartXMonad)
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
      `additionalKeys` map (\(a, b, program) -> ((a, b), spawn program)) (catMaybes [
          (modKey, xK_z,) <$> Conf.boomerInstall
        , (modKey, xK_l,) <$> Conf.screenLock
        , (0, xF86XK_AudioPlay, ) <$> (Conf.mediaController <&> (++ " play"))
        , (0, xF86XK_AudioPause,) <$> (Conf.mediaController <&> (++ " play"))
        , (0, xF86XK_AudioStop, ) <$> (Conf.mediaController <&> (++ " stop"))
        , (0, xF86XK_AudioNext, ) <$> (Conf.mediaController <&> (++ " next"))
        , (0, xF86XK_AudioPrev, ) <$> (Conf.mediaController <&> (++ " prev"))
        ])

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
  void ensureConnected
  spawn $ "xloadimage -onroot -fullscreen " ++ Conf.backgroundImage
  runAutorun
  setWMName "LG3D"
  void $ notifySend "XMonad" "startup finished" []

restartXMonad :: X ()
restartXMonad = do
  ensureDisconnected
  spawn Conf.restartCommand

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    Just idx <- W.findTag w <$> gets windowset
    void $ notifySendHints (show name) ("Workspace " ++ idx) [] [N.Urgency N.Critical]


switchToWS :: ManageHook
switchToWS = composeAll [
    className =? "firefox"  --> doShift "Browser"
  , className =? "discord"  --> doShift "Discord"
  , className =? "steam"    --> doShift "Steam"
  ]

floatIt :: ManageHook
floatIt = composeAll [
    className =? "Gimp" <&&> wmName /=? "GNU Image Manipulation Program"  --> doFloat
  , stringProperty "WM_WINDOW_ROLE" =? "GtkFileChooserDialog"             --> doFloat
  , wmName =? "Picross Touch Configuration"                               --> doFloat
  , className =? "flameshot"                                              --> doFloat
  , isDialog                                                              --> doFloat
  , willFloat                                                             --> doFloat
  ]

removeBorders :: ManageHook
removeBorders = composeAll [
    className =? "Google-chrome"  --> hasBorder False
  , className =? "firefox"        --> hasBorder False
  ]

sinkIt :: ManageHook
sinkIt = composeAll [
    wmName =? "SHENZHEN I/O"  --> doSink
  ]

closeSteamFriends :: ManageHook
closeSteamFriends = composeAll [
    wmName =? "Friends List" <&&> className =? "Steam"  --> doIgnore
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
runAutorun = spawn Conf.autorun


myXPConfig :: XPConfig
myXPConfig = def {
    font = Conf.xPromptFont
  , autoComplete = Just 1000
  }

getTimeString :: String -> X String
getTimeString fmt = io $ formatTime defaultTimeLocale fmt <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)

goToWorkspaceOf :: Window -> WindowSet -> Maybe WindowSet
goToWorkspaceOf window ws = flip W.view ws <$> W.findTag window ws

notifyTime :: X ()
notifyTime = replaceStateNotification "time" "Time" (getTimeString "%a %d.%m.%Y: %T")

notifyWS :: X ()
notifyWS = replaceStateNotification "workspace" "Workspace" getTag
  where
    getTag = withWindowSet $ return . W.currentTag

notifyWindowName :: X ()
notifyWindowName = withFocused $ getName >=> (replaceStateNotification "windowTitle" "Window Title" . pure . show)

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
  whenJust Conf.systemTray $ \tray ->
    spawn $ concat ["pgrep ", tray, " && killall ", tray, " || ", tray, " >> ", fromMaybe "/dev/null" Conf.autorunLog]

unhideLogHook :: X ()
unhideLogHook = do
  refocusLastLogHook
  -- withFocused maximizeHiddenFocused

maximizeHiddenFocused :: Window -> X ()
maximizeHiddenFocused win = do
  withLastMinimized $ \last_ -> do
    when (last_ /= win) $
      maximizeWindow win
