-- | A controlled segmented control / tab bar. The parent owns `active` and
-- | renders the corresponding pane itself (the SegmentedControl is just the
-- | selector — see CONTRACT.md rule 5: content stays with the parent). Picking
-- | a segment requests it via `Selected`.
module Hylograph.Halogen.UI.SegmentedControl
  ( Segment
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  ) where

import Prelude

import Data.Array (null, head)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Hylograph.Halogen.UI.Style (sty, cls, ink, inkSoft, surface, surfaceAlt, line, uiFont)

type Segment = { key :: String, label :: String }

type Input =
  { segments :: Array Segment
  , active :: String
  , disabled :: Boolean
  }

defaultInput :: Array Segment -> Input
defaultInput segments =
  { segments
  , active: maybe "" _.key (head segments)
  , disabled: false
  }

data Output = Selected String

data Query a = Set String a

type Slot = H.Slot Query Output

data Action = Receive Input | Pick String

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
  Pick key -> do
    { input } <- H.get
    when (not input.disabled && key /= input.active) (H.raise (Selected key))

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Set key a -> do
    { input } <- H.get
    when (not input.disabled && key /= input.active) (H.raise (Selected key))
    pure (Just a)

render :: forall m. State -> H.ComponentHTML Action () m
render { input } =
  HH.div
    [ cls "hg-segmented"
    , sty $ "display:inline-flex;border:1px solid " <> line <> ";border-radius:7px;overflow:hidden;"
        <> "font-family:" <> uiFont <> ";"
        <> (if input.disabled then "opacity:0.5" else "")
    ]
    (if null input.segments then [] else map (segBtn input) input.segments)

segBtn :: forall m. Input -> Segment -> H.ComponentHTML Action () m
segBtn input seg =
  let active = seg.key == input.active
  in HH.button
       [ cls ("hg-segmented__seg" <> if active then " is-active" else "")
       , HE.onClick \_ -> Pick seg.key
       , sty $ "padding:5px 12px;border:0;border-left:1px solid " <> line <> ";font-size:12px;"
           <> (if input.disabled then "cursor:default;" else "cursor:pointer;")
           <> (if active
                 then "background:" <> surface <> ";color:" <> ink <> ";font-weight:600"
                 else "background:" <> surfaceAlt <> ";color:" <> inkSoft)
       ]
       [ HH.text seg.label ]
