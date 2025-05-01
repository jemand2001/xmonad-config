module XMonad.Actions.PushWindow (pushUp, pushDown) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.StackSet

pushUp :: WindowSet -> WindowSet
pushUp = W.modify' pushUp'

pushDown :: WindowSet -> WindowSet
pushDown = W.modify' $ reverseStack . pushUp' . reverseStack

pushUp' :: W.Stack a -> W.Stack a
pushUp' s@(W.Stack _ (u:up) down) = s {W.up = up, W.down = u:down}
pushUp' s@(W.Stack _ [] down) = s {W.up = reverse down, W.down = []}
