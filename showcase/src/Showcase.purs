-- | Stub root component — proves the cross-package dependency, the slot wiring,
-- | and the controlled loop end-to-end before the full roster of stories lands.
module Showcase (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

import Hylograph.Halogen.UI.Toggle as Toggle

type Slots = ( toggle :: Toggle.Slot Unit )

_toggle :: Proxy "toggle"
_toggle = Proxy

type State = { on :: Boolean }

data Action = TogChanged Boolean

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { on: true }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  TogChanged v -> H.modify_ _ { on = v }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  HH.div_
    [ HH.h1_ [ HH.text "Hylograph Halogen UI" ]
    , HH.slot _toggle unit Toggle.component
        ((Toggle.defaultInput st.on) { label = Just "Live" })
        (\(Toggle.Changed v) -> TogChanged v)
    ]
