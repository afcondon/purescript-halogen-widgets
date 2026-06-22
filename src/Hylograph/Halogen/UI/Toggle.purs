-- | A controlled on/off switch. The parent owns `value`; clicking requests the
-- | opposite via `Changed`. The simplest instance of the contract — no
-- | ephemeral state at all (see CONTRACT.md).
module Hylograph.Halogen.UI.Toggle
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  ) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Hylograph.Halogen.UI.Style (sty, cls, accent, inkSoft, ink, uiFont)

type Input =
  { value :: Boolean
  , label :: Maybe String
  , disabled :: Boolean
  }

defaultInput :: Boolean -> Input
defaultInput value = { value, label: Nothing, disabled: false }

data Output = Changed Boolean

data Query a = Set Boolean a

type Slot = H.Slot Query Output

data Action = Receive Input | Click

type State = { input :: Input }

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  Click -> do
    { input } <- H.get
    when (not input.disabled) (H.raise (Changed (not input.value)))

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Set b a -> do
    { input } <- H.get
    when (not input.disabled) (H.raise (Changed b))
    pure (Just a)

render :: forall m. State -> H.ComponentHTML Action () m
render { input } =
  HH.div
    [ cls "hg-toggle"
    , sty $ "display:inline-flex;align-items:center;gap:8px;font-family:" <> uiFont
        <> ";" <> if input.disabled then "opacity:0.5" else ""
    ]
    ( [ HH.div
          [ cls "hg-toggle__track"
          , HE.onClick \_ -> Click
          , sty $ "position:relative;width:34px;height:20px;border-radius:10px;flex:0 0 auto;"
              <> "transition:background 120ms;"
              <> (if input.disabled then "cursor:default;" else "cursor:pointer;")
              <> "background:" <> (if input.value then accent else "#c9c6bd")
          ]
          [ HH.div
              [ cls "hg-toggle__knob"
              , sty $ "position:absolute;top:2px;width:16px;height:16px;border-radius:50%;background:#fff;"
                  <> "box-shadow:0 1px 2px #00000033;transition:left 120ms;"
                  <> "left:" <> (if input.value then "16px" else "2px")
              ]
              []
          ]
      ] <> maybe []
        (\l -> [ HH.span [ cls "hg-toggle__label", sty $ "font-size:13px;color:" <> (if input.value then ink else inkSoft) ] [ HH.text l ] ])
        input.label
    )
