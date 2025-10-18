module XMonad.Actions.MyNotify
  ( notifySend
  , notifySendIcon
  , replaceNotification
  , replaceNotificationIcon
  , notifySendHints
  , DBusConnection (DBusConnection)
  ) where

import qualified XMonad.Util.ExtensibleState as XS
import qualified DBus.Notify as N

import XMonad

import XMonad.Actions.DBus
import qualified Constants as C

type Actions = [(N.Action, String)]

notifySendN :: N.Note -> X N.Notification
notifySendN note = do
  DBusConnection client <- ensureConnected
  io $ N.notify client note

notifySend :: String -> String -> Actions -> X N.Notification
notifySend summary body actions = notifySendN N.blankNote {
    N.appName = C.appName
  , N.body = Just $ N.Text body
  , N.appImage = Just $ N.Icon "xmonad"
  , N.actions = actions
  , N.summary = summary
  }

notifySendIcon :: String -> String -> Actions -> Maybe N.Icon -> X N.Notification
notifySendIcon summary body actions ico = notifySendN N.blankNote {
    N.appName = C.appName
  , N.body = Just $ N.Text body
  , N.appImage = ico
  , N.actions = actions
  , N.summary = summary
  }

notifySendHints :: String -> String -> Actions -> [N.Hint] -> X N.Notification
notifySendHints summary body actions hints =
  notifySendN
    N.blankNote
      { N.appName = C.appName
      , N.body = Just $ N.Text body
      , N.appImage = Just $ N.Icon "xmonad"
      , N.actions = actions
      , N.summary = summary
      , N.hints = hints
      }

replaceNotificationN :: N.Notification -> N.Note -> X N.Notification
replaceNotificationN notif note = do
  DBusConnection client <- XS.get
  io $ N.replace client notif note

replaceNotification :: N.Notification -> String -> String -> Actions -> X N.Notification
replaceNotification notif summary body actions = replaceNotificationN notif N.blankNote {
    N.appName = C.appName
  , N.body = Just $ N.Text body
  , N.appImage = Just $ N.Icon "xmonad"
  , N.actions = actions
  , N.summary = summary
  }

replaceNotificationIcon :: N.Notification -> String -> String -> Actions -> Maybe N.Icon -> X N.Notification
replaceNotificationIcon notif summary body actions ico = replaceNotificationN notif N.blankNote {
    N.appName = C.appName
  , N.body = Just $ N.Text body
  , N.appImage = ico
  , N.actions = actions
  , N.summary = summary
  }
