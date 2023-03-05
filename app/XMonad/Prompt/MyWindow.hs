module XMonad.Prompt.MyWindow (getWindowPrompt) where

import qualified Data.Map as M
import Data.Maybe
import Data.Char (toLower)
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Prompt
import Data.Map (fromList, toList)
-- import Data.Foldable

data MyWindowPrompt = MyWindowPrompt WindowMap (String -> String -> Bool)
type WindowMap = M.Map String Window
type XWindowMap = X WindowMap

instance XPrompt MyWindowPrompt where
  showXPrompt (MyWindowPrompt _ _) = "Find Window: "

  completionFunction (MyWindowPrompt winmap predicate) =
    \s -> return . filter (predicate  (toLower <$> s) . map toLower) . map fst . M.toList $ winmap

getWindowPrompt :: XPConfig -> XWindowMap -> X (Maybe Window)
getWindowPrompt c winmap = do
  wm <- winmap
  root <- asks theRoot
  let wm' = massage 70 wm
  let mode = MyWindowPrompt wm' (searchPredicate c)
  let compList = completionFunction mode
  mkXPromptWithReturn mode c compList (getWindow root wm')

getWindow :: Window -> WindowMap -> String -> X Window
getWindow d m s = do
    let w = M.lookup s m
    return $ fromMaybe d w

massage :: Int -> WindowMap -> WindowMap
massage count wm = fromList [(f1 k, v) | (k, v) <- toList wm]
  where
    f1 s
      | length s > count = take (count - 3) s ++ "..."
      | otherwise = s
