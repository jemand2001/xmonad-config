module XMonad.Util.StackSet (reverseStack) where

import qualified XMonad.StackSet as W

reverseStack :: W.Stack a -> W.Stack a
reverseStack (W.Stack t ls rs) = W.Stack t rs ls
