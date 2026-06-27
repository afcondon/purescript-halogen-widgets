-- | Optional, opt-in transition timing for the widgets that can animate a
-- | state change (the accordion chevron, the accordion body reveal).
-- |
-- | The kit's default is **`NoMotion`** — instant, no easing — on purpose: the
-- | house style is restraint, and a widget toolkit should not impose animation
-- | on consumers who did not ask for it. A consumer opts a single widget into
-- | an eased transition by passing `Motion` (or the ready-made `defaultMotion`)
-- | in that widget's `Input`. Nothing animates library-wide; you turn it on
-- | where you want it.
-- |
-- | `transition` renders the CSS declaration these widgets splice into their
-- | inline styles, and emits the empty string under `NoMotion` — so the
-- | un-opted-in path adds no `transition` at all. (For users who opt in but set
-- | the OS "reduce motion" preference, `css/hylograph-ui.css` carries a
-- | `prefers-reduced-motion` block that neutralises these transitions.)
module Hylograph.Halogen.UI.Motion
  ( Motion(..)
  , Easing(..)
  , defaultMotion
  , easingCss
  , transition
  ) where

import Prelude

import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))

-- | Whether — and how — a state change animates. `NoMotion` is the default
-- | everywhere; `Motion d e` eases over duration `d` with timing function `e`.
data Motion
  = NoMotion
  | Motion Milliseconds Easing

-- | The CSS timing functions, named rather than stringly-typed.
data Easing
  = Linear
  | EaseIn
  | EaseOut
  | EaseInOut

-- | A pleasant opt-in default for callers who just want "animate it, sensibly":
-- | 180 ms, decelerating. Pass this rather than inventing a duration.
defaultMotion :: Motion
defaultMotion = Motion (Milliseconds 180.0) EaseOut

easingCss :: Easing -> String
easingCss = case _ of
  Linear -> "linear"
  EaseIn -> "ease-in"
  EaseOut -> "ease-out"
  EaseInOut -> "ease-in-out"

-- | Build a `transition:` declaration (with trailing `;`) for the given CSS
-- | property list, or the empty string under `NoMotion`:
-- |
-- | ```purescript
-- | transition "transform" defaultMotion
-- |   == "transition:transform 180ms ease-out;"
-- | transition "transform" NoMotion == ""
-- | ```
transition :: String -> Motion -> String
transition props = case _ of
  NoMotion -> ""
  Motion (Milliseconds ms) e ->
    "transition:" <> props <> " " <> show (Int.round ms) <> "ms " <> easingCss e <> ";"
