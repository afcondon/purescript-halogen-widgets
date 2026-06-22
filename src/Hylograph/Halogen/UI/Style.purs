-- | Shared styling helpers and a restrained light-Swiss palette, so every
-- | widget in the kit reads as one family. Tokens are plain `String`s meant to
-- | be concatenated into inline `style` strings; class names are emitted too so
-- | consumers can override via their own CSS.
module Hylograph.Halogen.UI.Style
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
ink = "#2b2b2b"

inkSoft :: String
inkSoft = "#7a7a7a"

line :: String
line = "#00000018"

surface :: String
surface = "#ffffff"

surfaceAlt :: String
surfaceAlt = "#f4f3f0"

accent :: String
accent = "#2f5fb0"

danger :: String
danger = "#b0492f"

warn :: String
warn = "#b07a2f"

ok :: String
ok = "#2f8a5c"

uiFont :: String
uiFont = "system-ui,-apple-system,'Segoe UI',sans-serif"

monoFont :: String
monoFont = "'SF Mono',Menlo,monospace"
