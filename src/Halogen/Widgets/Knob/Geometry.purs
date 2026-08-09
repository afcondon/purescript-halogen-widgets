-- | The trigonometry of a rotary knob, with no opinion about how it looks.
-- |
-- | A knob is a 300° sweep from 7 o'clock to 5 o'clock — the conventional feel,
-- | and the one every knob in this family uses. Turning that into pixels means
-- | two things: mapping a value onto an angle, and drawing a filled donut wedge
-- | between two angles. Both are fiddly, neither has anything to do with colour
-- | or stroke width, and **two copies of them must agree exactly or the same
-- | value reads as two different positions**.
-- |
-- | That is why this is its own module rather than a private section of
-- | `Halogen.Widgets.Knob`. The component here and Triggerfish's pure-view knob
-- | (whose parent owns the drag, so it cannot use the component) draw the same
-- | instrument with different chrome; they now share the part that has to match
-- | and keep their own of the part that does not.
-- |
-- | Angles are radians with **0 = up**, which is not SVG's convention — SVG puts
-- | 0 at 3 o'clock. `arcPath` does the quarter-turn internally, so a caller
-- | placing a pointer must subtract `pi / 2` itself (see `pointerAt`, which is
-- | there precisely so it does not have to remember).
module Halogen.Widgets.Knob.Geometry
  ( minAngle
  , maxAngle
  , sweep
  , valToAngle
  , tickAngle
  , pointerAt
  , arcPath
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Number (cos, sin, pi)

-- | 7 o'clock, where the sweep starts.
minAngle :: Number
minAngle = -5.0 * pi / 6.0

-- | 5 o'clock, where it ends.
maxAngle :: Number
maxAngle = 5.0 * pi / 6.0

-- | 300°, the total travel.
sweep :: Number
sweep = maxAngle - minAngle

-- | Where a value sits on the sweep. A degenerate range (`lo == hi`) parks the
-- | pointer at `minAngle` rather than dividing by zero — a knob that cannot move
-- | should read as fully counter-clockwise, not as NaN.
valToAngle :: Number -> Number -> Number -> Number
valToAngle lo hi v =
  let
    frac = if hi == lo then 0.0 else (v - lo) / (hi - lo)
  in
    minAngle + frac * sweep

-- | The angle of detent `i` of `n`, for the tick marks that turn a small-range
-- | knob into a rotary selector. Ticks span the full sweep inclusive, so tick 0
-- | is at `minAngle` and tick `n - 1` at `maxAngle`.
tickAngle :: Int -> Int -> Number
tickAngle n i =
  let
    frac = if n <= 1 then 0.0 else toNumber i / toNumber (n - 1)
  in
    minAngle + frac * sweep

-- | The screen point at radius `r` and angle `a` from centre `(cx, cy)` —
-- | the quarter-turn into SVG's frame done for you. Used for the pointer
-- | indicator and for tick endpoints.
pointerAt :: Number -> Number -> Number -> Number -> { x :: Number, y :: Number }
pointerAt cx cy r a =
  let
    a' = a - pi / 2.0
  in
    { x: cx + r * cos a', y: cy + r * sin a' }

-- | A filled donut wedge from `a0` to `a1` — out along the outer radius, in
-- | across the end cap, back along the inner radius, closed. This is the arc
-- | both the unfilled track and the filled value use; they differ only in which
-- | angles they are given.
arcPath :: Number -> Number -> Number -> Number -> Number -> Number -> String
arcPath cx cy outerR innerR a0 a1 =
  let
    o0 = pointerAt cx cy outerR a0
    o1 = pointerAt cx cy outerR a1
    i0 = pointerAt cx cy innerR a0
    i1 = pointerAt cx cy innerR a1
    largeArc = if (a1 - a0) > pi then "1" else "0"
    s = show
  in
    "M" <> s o0.x <> "," <> s o0.y
      <> " A" <> s outerR <> "," <> s outerR <> " 0 " <> largeArc <> ",1 " <> s o1.x <> "," <> s o1.y
      <> " L" <> s i1.x <> "," <> s i1.y
      <> " A" <> s innerR <> "," <> s innerR <> " 0 " <> largeArc <> ",0 " <> s i0.x <> "," <> s i0.y
      <> " Z"
