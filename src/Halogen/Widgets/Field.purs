-- | A labelled form row: a label above a single control, with an optional hint
-- | beneath. Chrome-function tier — polymorphic in the caller's action `i`.
module Halogen.Widgets.Field
  ( FieldConfig
  , field
  ) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.Widgets.Style (sty, cls, ink, inkSoft, uiFont)

type FieldConfig =
  { label :: String
  , hint :: Maybe String
  }

field :: forall w i. FieldConfig -> HH.HTML w i -> HH.HTML w i
field config control =
  HH.div
    [ cls "hg-field"
    , sty $ "display:flex;flex-direction:column;gap:5px;font-family:" <> uiFont
    ]
    [ HH.span
        [ cls "hg-field__label"
        , sty $ "font-size:11px;font-weight:600;letter-spacing:0.04em;text-transform:uppercase;color:" <> inkSoft
        ]
        [ HH.text config.label ]
    , HH.div [ cls "hg-field__control", sty $ "color:" <> ink ] [ control ]
    , maybe (HH.text "")
        (\h -> HH.span [ cls "hg-field__hint", sty $ "font-size:11px;color:" <> inkSoft ] [ HH.text h ])
        config.hint
    ]
