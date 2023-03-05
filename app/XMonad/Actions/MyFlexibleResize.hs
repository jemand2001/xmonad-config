module XMonad.Actions.MyFlexibleResize (mouseResizeEdgeWindow) where

import Prelude hiding (last)
import Graphics.X11
import Graphics.X11.Xlib.Cursor
import Foreign.C.Types
import XMonad
import XMonad.Util.XUtils (fi)

data PortionX = L | EH | R deriving (Eq)
data PortionY = T | EV | B deriving (Eq)

class Eq a => Portion a where
  first :: a
  middle :: a
  last :: a

instance Portion PortionX where
  first = L
  middle = EH
  last = R
instance Portion PortionY where
  first = T
  middle = EV
  last = B

mouseResizeEdgeWindow
  :: Rational   -- ^ The size of the area where only one edge is resized.
  -> Window     -- ^ The window to resize.
  -> X ()
mouseResizeEdgeWindow edge w = whenX (isClient w) $ withDisplay $ \d -> do
  io $ raiseWindow d w
  wa <- io $ getWindowAttributes d w
  sh <- io $ getWMNormalHints d w
  (_, _, _, _, _, ix, iy, _) <- io $ queryPointer d w
  let
    [pos_x, pos_y, width, height] = map (fi . ($ wa)) [wa_x, wa_y, wa_width, wa_height]
    west  = findPos ix width
    north = findPos iy height
    (cx, fx, gx) = mkSel west  width  pos_x
    (cy, fy, gy) = mkSel north height pos_y
  setC w (west, north)
  io $ warpPointer d none w 0 0 0 0 cx cy
  mouseDrag (\ex ey -> do
    let (nw,nh) = applySizeHintsContents sh (gx ex, gy ey)
    io $ moveResizeWindow d w (fx nw) (fy nh) nw nh)
    (float w >> setCursor w xC_left_ptr)
  where
  findPos :: Portion a => CInt -> Position -> a
  findPos m s
    | p < 0.5 - edge / 2 = first
    | p < 0.5 + edge / 2 = middle
    | otherwise = last
    where p = fi m / fi s
  mkSel :: Portion a => a -> Position -> Position -> (Position, Dimension -> Position, Position -> Dimension)
  mkSel b k p
    | b == first =  (0, (fi k + fi p -).fi, (fi k + fi p -).fi)
    | b == middle =    (k `div` 2, const p, const $ fi k)
    | b == last = (k, const p, subtract (fi p) . fi)
    | otherwise = undefined
  setC w wn = setCursor w $ case wn of
    (L, T) -> xC_top_left_corner
    (L, EV) -> xC_left_side
    (L, B) -> xC_bottom_left_corner
    (EH, T) -> xC_top_side
    (EH, EV) -> xC_cross
    (EH, B) -> xC_bottom_side
    (R, T) -> xC_top_right_corner
    (R, EV) -> xC_right_side
    (R, B) -> xC_bottom_right_corner

setCursor :: Window -> Glyph -> X ()
setCursor window glyph = withDisplay $ \d -> do
  -- root <- asks theRoot
  curs <- io $ createFontCursor d glyph
  io $ do
    defineCursor d window curs
    flush d
    freeCursor d curs
