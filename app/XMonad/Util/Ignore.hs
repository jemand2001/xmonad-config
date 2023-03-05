module XMonad.Util.Ignore (focusUpIgnoring, focusDownIgnoring) where

import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W

focusUpIgnoring, focusDownIgnoring :: [Window] -> WindowSet -> WindowSet
focusUpIgnoring minimized = W.modify' $ myFocusUp' minimized
focusDownIgnoring minimized = W.modify' $ myFocusDown' minimized

myFocusUp', myFocusDown' :: Eq a => [a] -> W.Stack a -> W.Stack a
myFocusUp' m s = if W.focus next `elem` m then myFocusUp' m next else next
  where
    next = W.focusUp' s
myFocusDown' m = reverseStack . myFocusUp' m . reverseStack

reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls
