module XMonad.Hooks.MyManageHelpers (propertyContains, wmName, (~=?)) where

import Data.List
import XMonad
import Data.Char

-- | A Query that returns whether a string property contains a substring.
propertyContains ::
  String        -- ^ Property name
  -> String     -- ^ Substring to search for
  -> Query Bool -- ^ Query that returns whether the property contains the substring
propertyContains prop val = do
  v <- stringProperty prop
  return $ val `isInfixOf` v

wmName :: Query String
wmName = stringProperty "WM_NAME"

(~=?) :: Query String -> String -> Query Bool
q ~=? s = (map toLower <$> q) =? map toLower s
