{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-missing-signatures -Wno-name-shadowing #-}

module Main where

import Conf (modKey)
import qualified Conf
import Control.Exception (bracket)
import Control.Monad
import qualified DBus.Notify as N
import Data.Bits
import Data.Char
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Graphics.X11.ExtraTypes
import Graphics.X11.Types
import System.IO
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.Countdown
import qualified XMonad.Actions.CycleWS as Cycle
import XMonad.Actions.DBus
import XMonad.Actions.FloatSnap
import XMonad.Actions.Minimize
import qualified XMonad.Actions.MyFlexibleResize as Flex
import XMonad.Actions.MyNotify
import XMonad.Actions.PulseAudio
import XMonad.Actions.StatefulNotify
import XMonad.Actions.WithAll
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.MyManageHelpers
import XMonad.Hooks.RefocusLast (refocusLastLogHook)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutHints
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Prompt
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings, removeKeys)
import XMonad.Util.Ignore
import XMonad.Util.NamedWindows
import XMonad.Util.Run (runProcessWithInput, spawnPipe)
import XMonad.Util.Time
import XMonad.Actions.PushWindow
import qualified Constants as C

myLayout = layoutHints $ boringAuto $ minimize $ noBorders Full ||| tiled ||| Mirror tiled
  where
    tiled = Tall nmaster delta ratio
    -- see XMonad.Config.layout
    nmaster = 1
    delta = 3/100
    ratio = 1/2

main :: IO ()
main = xmonad $
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
      , workspaces = Conf.workspaces
      , XMonad.startupHook = Main.startupHook
      , manageHook = composeAll [manageHook def, switchToWS, floatIt, closeSteamFriends, sinkIt]
      , logHook = unhideLogHook <+> logHook def
      , clickJustFocuses = False
      }
    `removeKeys` badKeys
    `additionalKeys` [
        ((0,                    xK_Print  ), spawn "QT_ENABLE_HIGHDPI_SCALING=0 flameshot gui")
      , ((shiftMask,            xK_Print  ), spawn "QT_ENABLE_HIGHDPI_SCALING=0 flameshot full -c")

      , ((modKey,               xK_Return ), spawn Conf.terminal)
      , ((modKey,               xK_r      ), spawn "rofi -show drun")
      , ((modKey .|. shiftMask, xK_r      ), spawn "rofi -show run")

      , ((modKey,               xK_Right  ), nextWS)
      , ((modKey,               xK_Left   ), prevWS)

      , ((modKey,               xK_e      ), Cycle.nextScreen >> notifyWS)

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

      , ((modKey .|. shiftMask, xK_j      ), windows pushDown)
      , ((modKey .|. shiftMask, xK_k      ), windows pushUp)

      , ((modKey,               xK_i      ), windows copyToAll)   -- "mark sticky"
      , ((modKey .|. shiftMask, xK_i      ), killAllOtherCopies)  -- "unmark sticky"

      , ((modKey .|. shiftMask, xK_n      ), notifyWindowName)

      , ((modKey,               xK_u      ), withFocused maximizeWindowAndFocus)  -- "unminimize" the focused window; useful when you have a notification from a minimized window

      , ((modKey,               xK_q      ), restartXMonad)

      , ((modKey,               xK_F2     ), lowerVolume 3 >>= notifyVolume)
      , ((modKey,               xK_F3     ), raiseVolume 3 >>= notifyVolume)
      , ((0,       xF86XK_AudioLowerVolume), lowerVolume 3 >>= notifyVolume)
      , ((0,       xF86XK_AudioRaiseVolume), raiseVolume 3 >>= notifyVolume)
      , ((0,       xF86XK_AudioMute       ), muteVolume)
      ]
    `additionalKeys` [
        ((modKey .|. modifier,  k         ), windows (viewPolicy ws) >> notifyWS)
        | (k, ws) <- zip [xK_1..] Conf.workspaces
        , (modifier, viewPolicy) <- [(noModMask, W.view), (controlMask, W.greedyView)]
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
      , (modKey .|. shiftMask, xK_c,) <$> Conf.clipboardManager
      ])
    `additionalKeys` maybe [] (\p -> [((modKey, xK_c), notifyCountdowns p)]) Conf.countdownFile
    `additionalKeys` maybe [] (\controller -> [((0, k), spawn $ controller ++ " " ++ c)
      |
      (k, c) <- [
        (xF86XK_AudioPlay,  "play")
      , (xF86XK_AudioPause, "play")
      , (xF86XK_AudioStop,  "stop")
      , (xF86XK_AudioNext,  "next")
      , (xF86XK_AudioPrev,  "prev")
      ]
    ]) Conf.mediaController

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
  spawn $ "xloadimage -onroot " ++ Conf.backgroundImage
  runAutorun
  setWMName "LG3D"
  void $ notifySend C.appName "startup finished" []

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
    className =? "firefox"     --> doShift "Browser"
  , className =? "discord"     --> doShift "Chat"
  , wmName    ~=? "steam"      --> doShift "Steam"
  , className =? "Signal"      --> doShift "Chat"
  , className =? "thunderbird" --> doShift "Chat"
  , className <&> isPrefixOf "vscod" . map toLower --> doShift "Code"
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

goToWorkspaceOf :: Window -> WindowSet -> Maybe WindowSet
goToWorkspaceOf window ws = flip W.view ws <$> W.findTag window ws

notifyTime :: X ()
notifyTime = replaceStateNotification "time" "Time" (getTimeString "%a %d.%m.%Y: %T")

notifyWS :: X ()
notifyWS = replaceStateNotification "workspace" "Workspace" getTag
  where
    getTag = withWindowSet $ \windowset -> do
      let tag = W.currentTag windowset
      let screens = W.screens windowset
      let screen = find (\scr -> W.tag (W.workspace scr) == tag) screens
      let suffix = maybe "" (\scr -> " on screen " ++ show (fromEnum $ W.screen scr)) screen
      return (tag ++ suffix)

notifyWindowName :: X ()
notifyWindowName = withFocused $ getName >=> (replaceStateNotification "windowTitle" "Window Title" . pure . show)

nextWS :: X ()
nextWS = Cycle.nextWS >> notifyWS

prevWS :: X ()
prevWS = Cycle.prevWS >> notifyWS

notifyOutput :: String -> X ()
notifyOutput s = do
  out <- runProcessWithInput s [] ""
  io $ bracket (spawnPipe "xmessage -file -") hClose $ flip hPutStr out

toggleSystray :: X ()
toggleSystray = whenJust Conf.systemTray $ \tray ->
    spawn $ concat ["pgrep ", tray, " && killall ", tray, " || ", tray, " >> ", fromMaybe "/dev/null" Conf.autorunLog]

unhideLogHook :: X ()
unhideLogHook = do
  refocusLastLogHook
  withFocused maximizeHiddenFocused

maximizeHiddenFocused :: Window -> X ()
maximizeHiddenFocused win = withLastMinimized $ \last_ -> when (last_ /= win) $
      maximizeWindow win

notifyVolume :: Double -> X ()
notifyVolume = replaceStateNotification "volume" "New Volume" . pure . show
