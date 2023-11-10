module XMonad.Util.Paths (tildeExpand) where

import System.Directory
import System.FilePath
import XMonad
import Data.Functor

tildeExpand :: MonadIO m => FilePath -> m FilePath
tildeExpand ('~':s) = io getUserDocumentsDirectory <&> (</> dropWhile isPathSeparator s)
tildeExpand s = return s
