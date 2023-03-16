module XMonad.Actions.StatefulNotify where

import XMonad.Core
import qualified DBus.Notify as N
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.MyNotify

import qualified Data.Map as M

newtype StateNotifications = StateNotifications (M.Map String N.Notification) deriving (Typeable)

instance ExtensionClass StateNotifications where
  initialValue = StateNotifications M.empty

replaceStateNotification :: String -> String -> X String -> X ()
replaceStateNotification key title text = do
  text' <- text
  StateNotifications notes <- XS.get

  let old = M.lookup key notes

  let notify = maybe send replace old
  new <- notify text'
  XS.put $ StateNotifications $ M.insert key new notes
  where
    replace n str = replaceNotificationIcon n title str [] Nothing
    send str = notifySendIcon title str [] Nothing
