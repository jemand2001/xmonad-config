module XMonad.Actions.StatefulNotify where

import XMonad.Core
import qualified DBus.Notify as N
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.MyNotify

replaceStateNotification :: ExtensionClass a => String -> X (Maybe N.Notification) -> X String -> (N.Notification -> a) -> X ()
replaceStateNotification title old text f = do
  oldN <- old
  text' <- text

  let notify = maybe send replace oldN

  XS.put . f =<< notify text'
  where
      replace n str = replaceNotificationIcon n title str [] Nothing
      send str = notifySendIcon title str [] Nothing
