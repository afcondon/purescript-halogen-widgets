-- | A controlled range slider. The parent owns `value`; dragging requests new
-- | values via `Changed`, debounced inside the widget (a drag fires a flood of
-- | input events — exactly the behaviour the contract says belongs in the
-- | widget, not in app code). Set `debounce` to `Milliseconds 0.0` to emit every
-- | step.
module Halogen.Widgets.Slider
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Time.Duration (Milliseconds(..))
import DOM.HTML.Indexed.InputType (InputType(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Widgets.Style (sty, cls, accent)

type Input =
  { value :: Number
  , min :: Number
  , max :: Number
  , step :: Number
  , debounce :: Milliseconds
  , disabled :: Boolean
  }

defaultInput :: Number -> Input
defaultInput value =
  { value, min: 0.0, max: 100.0, step: 1.0, debounce: Milliseconds 80.0, disabled: false }

data Output = Changed Number

data Query a = Set Number a

type Slot = H.Slot Query Output

data Action = Receive Input | Drag String

type State = { input :: Input, version :: Int }

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, version: 0 }
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
  Drag raw -> do
    st <- H.get
    when (not st.input.disabled) do
      let requested = fromMaybe st.input.value (Number.fromString raw)
      case st.input.debounce of
        Milliseconds ms
          | ms <= 0.0 -> H.raise (Changed requested)
          | otherwise -> do
              next <- H.modify \s -> s { version = s.version + 1 }
              let mine = next.version
              void $ H.fork do
                liftAff (delay (Milliseconds ms))
                s' <- H.get
                when (s'.version == mine) (H.raise (Changed requested))

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Set v a -> do
    { input } <- H.get
    when (not input.disabled) (H.raise (Changed v))
    pure (Just a)

render :: forall m. State -> H.ComponentHTML Action () m
render { input } =
  HH.input
    [ cls "hg-slider"
    , HP.type_ InputRange
    , HP.value (show input.value)
    , HP.disabled input.disabled
    , sty $ "width:100%;accent-color:" <> accent <> ";"
        <> (if input.disabled then "opacity:0.5;cursor:default" else "cursor:pointer")
    , HP.attr (H.AttrName "min") (show input.min)
    , HP.attr (H.AttrName "max") (show input.max)
    , HP.attr (H.AttrName "step") (show input.step)
    , HE.onValueInput Drag
    ]
