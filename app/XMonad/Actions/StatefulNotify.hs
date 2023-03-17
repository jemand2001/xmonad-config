module XMonad.Actions.StatefulNotify where

import XMonad.Core
import qualified DBus.Notify as N
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.MyNotify

import qualified Data.Map.Strict as M

newtype StateNotifications = StateNotifications (M.Map String N.Notification) deriving (Typeable)

instance ExtensionClass StateNotifications where
  initialValue = StateNotifications M.empty

getNotification :: String -> X (Maybe N.Notification)
getNotification key = do
  StateNotifications notes <- XS.get
  return $ M.lookup key notes

setNotification :: String -> Maybe N.Notification -> X ()
setNotification key new = do
  StateNotifications notes <- XS.get
  let notes' = M.alter (const new) key notes
  XS.put $ StateNotifications notes'

replaceStateNotification :: String -> String -> X String -> X ()
replaceStateNotification key title text = do
  text' <- text
  old <- getNotification key

  let notify = maybe send replace old
  new <- notify text'
  setNotification key $ Just new
  where
    replace n str = replaceNotificationIcon n title str [] Nothing
    send str = notifySendIcon title str [] Nothing
