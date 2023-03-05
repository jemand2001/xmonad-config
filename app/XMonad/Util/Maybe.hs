module XMonad.Util.Maybe where

maybeDefault :: a -> Maybe a -> (a -> b) -> b
maybeDefault _ (Just x) f = f x
maybeDefault d Nothing f = f d
