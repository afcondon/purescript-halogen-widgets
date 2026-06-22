-- | A single notification banner, coloured by `Variant`, with an optional
-- | dismiss affordance. Chrome-function tier — polymorphic in the caller's
-- | action `i`. (A stateful toast *host* that queues several of these is a
-- | future leaf component; this is the atom it would render.)
module Hylograph.Halogen.UI.Toast
  ( Variant(..)
  , ToastConfig
  , toast
  ) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Hylograph.Halogen.UI.Style (sty, cls, accent, ok, warn, danger, uiFont)

-- | Closed set of severities — an ADT, not a String (contract: ADTs for closed
-- | alternatives).
data Variant = Info | Success | Warning | Error

type ToastConfig i =
  { variant :: Variant
  , message :: String
  , onDismiss :: Maybe i      -- ^ `Just act` renders a × that raises `act`
  }

variantColor :: Variant -> String
variantColor = case _ of
  Info -> accent
  Success -> ok
  Warning -> warn
  Error -> danger

toast :: forall w i. ToastConfig i -> HH.HTML w i
toast config =
  HH.div
    [ cls "hg-toast"
    , sty $ "display:flex;align-items:center;gap:12px;padding:10px 14px;border-radius:8px;"
        <> "font-family:" <> uiFont <> ";font-size:13px;color:#fff;"
        <> "box-shadow:0 4px 14px #0000002a;background:" <> variantColor config.variant
    ]
    [ HH.span [ cls "hg-toast__message", sty "flex:1" ] [ HH.text config.message ]
    , maybe (HH.text "")
        (\act ->
          HH.span
            [ cls "hg-toast__dismiss"
            , HE.onClick \_ -> act
            , sty "cursor:pointer;opacity:0.85;font-size:16px;line-height:1"
            ]
            [ HH.text "×" ])
        config.onDismiss
    ]
