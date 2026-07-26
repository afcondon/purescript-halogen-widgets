-- | Shared styling helpers and a restrained light-Swiss palette, so every
-- | widget in the kit reads as one family.
-- |
-- | Each colour token is a CSS custom property with a baked-in fallback —
-- | `var(--hw-ink, #2b2b2b)`. So widgets are **self-contained** (they render
-- | with the light defaults even when no stylesheet is loaded) *and*
-- | **themeable** (drop in `css/halogen-widgets.css`, or just define the
-- | `--hw-*` variables yourself, to recolour — e.g. dark mode — without
-- | touching any PureScript). Tokens are plain `String`s meant to be
-- | concatenated into inline `style` strings; class names are emitted too.
module Halogen.Widgets.Style
  ( sty
  , cls
  , clss
  , ink
  , inkSoft
  , line
  , surface
  , surfaceAlt
  , accent
  , danger
  , warn
  , ok
  , trackOff
  , controlBorder
  , knob
  , shadow
  , backdrop
  , uiFont
  , monoFont
  ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | An inline `style` attribute from a raw CSS string.
sty :: forall r i. String -> HP.IProp r i
sty = HP.attr (H.AttrName "style")

-- | A single class name.
cls :: forall r i. String -> HP.IProp ( class :: String | r ) i
cls s = HP.class_ (HH.ClassName s)

-- | Several class names.
clss :: forall r i. Array String -> HP.IProp ( class :: String | r ) i
clss names = HP.classes (map HH.ClassName names)

ink :: String
ink = "var(--hw-ink, #2b2b2b)"

inkSoft :: String
inkSoft = "var(--hw-ink-soft, #7a7a7a)"

line :: String
line = "var(--hw-line, rgba(0,0,0,0.09))"

surface :: String
surface = "var(--hw-surface, #ffffff)"

surfaceAlt :: String
surfaceAlt = "var(--hw-surface-alt, #f4f3f0)"

accent :: String
accent = "var(--hw-accent, #2f5fb0)"

danger :: String
danger = "var(--hw-danger, #b0492f)"

warn :: String
warn = "var(--hw-warn, #b07a2f)"

ok :: String
ok = "var(--hw-ok, #2f8a5c)"

-- | The "off" track of a switch / unfilled control rail.
trackOff :: String
trackOff = "var(--hw-track-off, #c9c6bd)"

-- | The border of a small control (buttons, steppers).
controlBorder :: String
controlBorder = "var(--hw-control-border, #cfcabb)"

-- | The moving knob / thumb of a switch.
knob :: String
knob = "var(--hw-knob, #ffffff)"

-- | A drop-shadow colour (use as the colour stop of a box-shadow).
shadow :: String
shadow = "var(--hw-shadow, rgba(0,0,0,0.13))"

-- | The dimming layer behind a modal.
backdrop :: String
backdrop = "var(--hw-backdrop, rgba(0,0,0,0.4))"

uiFont :: String
uiFont = "var(--hw-font, system-ui,-apple-system,'Segoe UI',sans-serif)"

monoFont :: String
monoFont = "'SF Mono',Menlo,monospace"
