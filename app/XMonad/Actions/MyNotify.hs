module XMonad.Actions.MyNotify
  (
  --   notifySendN
  -- , notifySend
  -- , notifySendIcon
  -- , replaceNotificationN
  -- , replaceNotification
  -- , replaceNotificationIcon
    notifySend
  , notifySendIcon
  , replaceNotification
  , replaceNotificationIcon
  , notifySendHints
  , DBusConnection (DBusConnection)
  -- , TimeNotification (..)
  ) where

import qualified XMonad.Util.ExtensibleState as XS
import qualified DBus.Notify as N
import qualified DBus.Client as C
import Control.Monad

import XMonad

type Actions = [(N.Action, String)]

data DBusConnection = DBusConnection N.Client | NotConnected

instance ExtensionClass DBusConnection where
  initialValue = NotConnected

disconnected :: DBusConnection -> Bool
disconnected NotConnected = True
disconnected _ = False

ensureConnected :: X DBusConnection
ensureConnected = do
  conn <- XS.get
  when (disconnected conn) $ void . XS.put . DBusConnection =<< io C.connectSession
  XS.get

notifySendN :: N.Note -> X N.Notification
notifySendN note = do
  DBusConnection client <- ensureConnected
  io $ do
    trace "sending a notification"
    N.notify client note

notifySend :: String -> String -> Actions -> X N.Notification
notifySend summary body actions = notifySendN N.blankNote {
    N.appName = "XMonad"
  , N.body = Just $ N.Text body
  , N.appImage = Just $ N.Icon "xmonad"
  , N.actions = actions
  , N.summary = summary
  }

notifySendIcon :: String -> String -> Actions -> Maybe N.Icon -> X N.Notification
notifySendIcon summary body actions ico = notifySendN N.blankNote {
    N.appName = "XMonad"
  , N.body = Just $ N.Text body
  , N.appImage = ico
  , N.actions = actions
  , N.summary = summary
  }

notifySendHints :: String -> String -> Actions -> [N.Hint] -> X N.Notification
notifySendHints summary body actions hints =
  notifySendN
    N.blankNote
      { N.appName = "XMonad",
        N.body = Just $ N.Text body,
        N.appImage = Just $ N.Icon "xmonad",
        N.actions = actions,
        N.summary = summary,
        N.hints = hints
      }

replaceNotificationN :: N.Notification -> N.Note -> X N.Notification
replaceNotificationN notif note = do
  DBusConnection client <- XS.get
  io $ N.replace client notif note

replaceNotification :: N.Notification -> String -> String -> Actions -> X N.Notification
replaceNotification notif summary body actions = replaceNotificationN notif N.blankNote {
    N.appName = "XMonad"
  , N.body = Just $ N.Text body
  , N.appImage = Just $ N.Icon "xmonad"
  , N.actions = actions
  , N.summary = summary
  }

replaceNotificationIcon :: N.Notification -> String -> String -> Actions -> Maybe N.Icon -> X N.Notification
replaceNotificationIcon notif summary body actions ico = replaceNotificationN notif N.blankNote {
    N.appName = "XMonad"
  , N.body = Just $ N.Text body
  , N.appImage = ico
  , N.actions = actions
  , N.summary = summary
  }
