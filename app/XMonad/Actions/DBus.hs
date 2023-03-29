{-# LANGUAGE OverloadedStrings #-}
module XMonad.Actions.DBus (
    ensureConnected
  , ensureDisconnected
  , DBusConnection (..)
) where

import Control.Monad

import qualified XMonad.Util.ExtensibleState as XS
import qualified DBus as C
import qualified DBus.Client as C

import XMonad hiding (restart)
import qualified Conf

data DBusConnection = DBusConnection C.Client | NotConnected

instance ExtensionClass DBusConnection where
  initialValue = NotConnected

serviceName :: C.BusName
serviceName = "org.XMonad"

disconnected :: DBusConnection -> Bool
disconnected NotConnected = True
disconnected _ = False

ensureConnected :: X DBusConnection
ensureConnected = do
  conn <- XS.get
  when (disconnected conn) $ do
    sess <- io C.connectSession
    void $ XS.put $ DBusConnection sess
    io $ do
      void $ C.requestName sess serviceName [C.nameAllowReplacement, C.nameReplaceExisting]
      C.export sess "/xmonad" $ C.defaultInterface {
        C.interfaceName = "com.xmonad"
      , C.interfaceMethods = [C.autoMethod "Restart" restart]
      }
  XS.get
  where
    restart :: IO ()
    restart = trace "restart command received" >> spawn Conf.restartCommand

ensureDisconnected :: X ()
ensureDisconnected = do
  conn <- XS.get
  case conn of
    DBusConnection c -> io $ C.disconnect c
    NotConnected -> return ()
  XS.put NotConnected

-- export :: C.Client -> Interface -> X ()
-- export client interface = do
--   io $ C.export 
