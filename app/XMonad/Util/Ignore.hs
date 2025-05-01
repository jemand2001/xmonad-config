module XMonad.Util.Ignore (focusUpIgnoring, focusDownIgnoring) where


import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.StackSet

focusUpIgnoring, focusDownIgnoring :: [Window] -> WindowSet -> WindowSet
focusUpIgnoring minimized = W.modify' $ myFocusUp' minimized
focusDownIgnoring minimized = W.modify' $ myFocusDown' minimized

myFocusUp', myFocusDown' :: Eq a => [a] -> W.Stack a -> W.Stack a
myFocusUp' m s = if W.focus next `elem` m then myFocusUp' m next else next
  where
    next = W.focusUp' s
myFocusDown' m = reverseStack . myFocusUp' m . reverseStack
