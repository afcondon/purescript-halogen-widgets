-- | A controlled integer stepper: ‹ value › with clamped bounds. The parent
-- | owns `value`; the arrows request `value ± step` (clamped) via `Changed`.
module Hylograph.Halogen.UI.Stepper
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Hylograph.Halogen.UI.Style (sty, cls, ink, inkSoft, surfaceAlt, line, uiFont, monoFont)

type Input =
  { value :: Int
  , min :: Int
  , max :: Int
  , step :: Int
  , disabled :: Boolean
  }

defaultInput :: Int -> Input
defaultInput value = { value, min: 0, max: 100, step: 1, disabled: false }

data Output = Changed Int

data Query a = Set Int a

type Slot = H.Slot Query Output

data Action = Receive Input | Bump Int

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

clamp :: Input -> Int -> Int
clamp i v = if v < i.min then i.min else if v > i.max then i.max else v

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  Bump dir -> do
    { input } <- H.get
    let next = clamp input (input.value + dir * input.step)
    when (not input.disabled && next /= input.value) (H.raise (Changed next))

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Set v a -> do
    { input } <- H.get
    let next = clamp input v
    when (not input.disabled && next /= input.value) (H.raise (Changed next))
    pure (Just a)

render :: forall m. State -> H.ComponentHTML Action () m
render { input } =
  HH.div
    [ cls "hg-stepper"
    , sty $ "display:inline-flex;align-items:center;gap:6px;font-family:" <> uiFont
        <> ";" <> if input.disabled then "opacity:0.5" else ""
    ]
    [ stepBtn "‹" (input.value <= input.min) (Bump (-1))
    , HH.span
        [ cls "hg-stepper__value"
        , sty $ "min-width:48px;text-align:center;font-family:" <> monoFont
            <> ";font-size:13px;color:" <> ink
        ]
        [ HH.text (show input.value) ]
    , stepBtn "›" (input.value >= input.max) (Bump 1)
    ]
  where
  stepBtn :: String -> Boolean -> Action -> H.ComponentHTML Action () m
  stepBtn glyph atEnd act =
    HH.button
      [ cls "hg-stepper__btn"
      , HE.onClick \_ -> act
      , HP.disabled (input.disabled || atEnd)
      , sty $ "width:22px;height:22px;border-radius:5px;border:1px solid #cfcabb;"
          <> "background:" <> surfaceAlt <> ";color:" <> inkSoft <> ";font-size:13px;line-height:1;"
          <> "border-color:" <> line <> ";"
          <> (if input.disabled || atEnd then "opacity:0.4;cursor:default" else "cursor:pointer")
      ]
      [ HH.text glyph ]
