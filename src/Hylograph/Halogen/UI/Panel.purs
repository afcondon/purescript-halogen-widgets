-- | A titled surface that wraps caller content — the generalised form of
-- | Triggerfish's `panelShell` body. Chrome-function tier: polymorphic in the
-- | caller's action `i`, so the body threads straight through.
module Hylograph.Halogen.UI.Panel
  ( PanelConfig
  , panel
  ) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Hylograph.Halogen.UI.Style (sty, cls, ink, inkSoft, surface, line, uiFont)

type PanelConfig =
  { title :: String
  , sub :: Maybe String
  }

panel :: forall w i. PanelConfig -> Array (HH.HTML w i) -> HH.HTML w i
panel config body =
  HH.div
    [ cls "hg-panel"
    , sty $ "background:" <> surface <> ";border:1px solid " <> line <> ";border-radius:8px;"
        <> "overflow:hidden;font-family:" <> uiFont
    ]
    [ HH.div
        [ cls "hg-panel__header"
        , sty $ "display:flex;align-items:baseline;justify-content:space-between;gap:10px;"
            <> "padding:10px 14px;border-bottom:1px solid " <> line
        ]
        [ HH.span [ sty $ "font-size:13px;font-weight:600;letter-spacing:0.02em;color:" <> ink ]
            [ HH.text config.title ]
        , maybe (HH.text "")
            (\s -> HH.span [ sty $ "font-size:11px;color:" <> inkSoft ] [ HH.text s ])
            config.sub
        ]
    , HH.div [ cls "hg-panel__body", sty "padding:14px" ] body
    ]
