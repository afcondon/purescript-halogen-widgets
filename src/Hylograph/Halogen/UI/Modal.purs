-- | A modal dialog — the chrome-function tier (see CONTRACT.md rule 5). Because
-- | a modal wraps caller-owned interactive content (forms, buttons), it cannot
-- | be a leaf component: it is a render function, polymorphic in the caller's
-- | action `i`, so the caller's body and the supplied `onClose` action thread
-- | straight through untouched.
-- |
-- | The backdrop and panel are siblings (not nested), so a click inside the
-- | panel never reaches the backdrop's `onClose` — no `stopPropagation` needed.
module Hylograph.Halogen.UI.Modal
  ( ModalConfig
  , modal
  ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Hylograph.Halogen.UI.Style (sty, cls, ink, inkSoft, surface, line, uiFont, backdrop)

type ModalConfig i =
  { open :: Boolean
  , title :: String
  , onClose :: i        -- ^ raised when the backdrop or the × is clicked
  }

-- | `modal config body` — renders nothing when closed; otherwise a centred
-- | panel over a dimming backdrop, with `body` as its content.
modal :: forall w i. ModalConfig i -> Array (HH.HTML w i) -> HH.HTML w i
modal config body =
  if not config.open then HH.text ""
  else
    HH.div
      [ cls "hg-modal"
      , sty $ "position:fixed;inset:0;z-index:1000;display:flex;align-items:center;"
          <> "justify-content:center;font-family:" <> uiFont
      ]
      [ HH.div
          [ cls "hg-modal__backdrop"
          , HE.onClick \_ -> config.onClose
          , sty $ "position:absolute;inset:0;cursor:pointer;background:" <> backdrop
          ]
          []
      , HH.div
          [ cls "hg-modal__panel"
          , sty $ "position:relative;min-width:320px;max-width:min(560px,92vw);max-height:88vh;"
              <> "overflow:auto;background:" <> surface <> ";border-radius:var(--hg-radius,10px);"
              <> "box-shadow:0 12px 40px #00000033;padding:0"
          ]
          [ HH.div
              [ cls "hg-modal__header"
              , sty $ "display:flex;align-items:center;justify-content:space-between;gap:12px;"
                  <> "padding:14px 18px;border-bottom:1px solid " <> line
              ]
              [ HH.span [ sty $ "font-size:15px;font-weight:600;color:" <> ink ] [ HH.text config.title ]
              , HH.span
                  [ cls "hg-modal__close"
                  , HE.onClick \_ -> config.onClose
                  , sty $ "cursor:pointer;font-size:18px;line-height:1;color:" <> inkSoft
                  ]
                  [ HH.text "×" ]
              ]
          , HH.div [ cls "hg-modal__body", sty "padding:18px" ] body
          ]
      ]
