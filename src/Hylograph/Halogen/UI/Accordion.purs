-- | A controlled disclosure header — the reference instance of the library
-- | contract (see CONTRACT.md).
-- |
-- | One `Accordion` is **one panel's interactive header**: an open header
-- | (label · optional sub · ▾) or, when collapsed, a thin rotated tab (▸).
-- | Clicking either emits `Toggled` carrying the requested new `open` value;
-- | the component never flips its own state. The PARENT owns `open` and renders
-- | the panel BODY itself (`if open then [body] else []`) — see rule 5 in
-- | CONTRACT.md for why the body cannot live inside the component.
-- |
-- | The toggle is debounced inside the component (configurable via
-- | `Input.debounce`). This is the machinery Triggerfish hand-rolls as
-- | `markTap` / `lastTapMicros` to stop a 30 fps re-render double-dispatch from
-- | cancelling a panel flip — here it is absorbed once, for everyone.
-- |
-- | A multi-panel accordion is N of these sharing a parent-owned open-set
-- | (exactly Triggerfish's `collapsed :: Array String`).
module Hylograph.Halogen.UI.Accordion
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  ) where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Hylograph.Halogen.UI.Style (sty, cls, clss)

-- | Controlled input. The parent owns `open`; everything else is config.
type Input =
  { open :: Boolean
  , label :: String
  , sub :: Maybe String          -- ^ small right-aligned subtitle (e.g. "3 sources")
  , debounce :: Milliseconds     -- ^ toggle debounce; `Milliseconds 0.0` disables it
  , disabled :: Boolean
  }

-- | A sensible starting `Input`; override the fields you care about.
-- | The 120 ms default coalesces a double-dispatched click into one `Toggled`.
defaultInput :: String -> Input
defaultInput label =
  { open: true
  , label
  , sub: Nothing
  , debounce: Milliseconds 120.0
  , disabled: false
  }

-- | A request, not a fact: the user asked for this new `open` value.
data Output = Toggled Boolean

-- | Imperative escape hatch. Rarely needed — `Input` is the main channel — but
-- | present so every widget's `Slot` has the same shape. `SetOpen` requests a
-- | value just as a click would.
data Query a = SetOpen Boolean a

type Slot = H.Slot Query Output

data Action
  = Receive Input
  | Toggle

-- | State mirrors `Input` (so `render` has something to read) plus the debounce
-- | generation counter — the only ephemeral state this widget keeps.
type State =
  { input :: Input
  , version :: Int
  }

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
  Receive input ->
    H.modify_ _ { input = input }
  Toggle -> do
    st <- H.get
    when (not st.input.disabled) do
      let requested = not st.input.open
      case st.input.debounce of
        Milliseconds ms
          | ms <= 0.0 -> H.raise (Toggled requested)
          | otherwise -> do
              next <- H.modify \s -> s { version = s.version + 1 }
              let mine = next.version
              void $ H.fork do
                liftAff (delay (Milliseconds ms))
                s' <- H.get
                when (s'.version == mine) (H.raise (Toggled requested))

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  SetOpen b a -> do
    st <- H.get
    when (not st.input.disabled) (H.raise (Toggled b))
    pure (Just a)

render :: forall m. State -> H.ComponentHTML Action () m
render { input } =
  if input.open then headerOpen input else headerCollapsed input

-- | The open-panel header: label on the left, optional sub + chevron on the
-- | right. Clicking requests a collapse.
headerOpen :: forall m. Input -> H.ComponentHTML Action () m
headerOpen input =
  HH.div
    [ clss [ "hg-accordion", "hg-accordion--open" ]
    , HE.onClick \_ -> Toggle
    , sty $ rowBase input <> ";justify-content:space-between;padding:6px 2px;border-bottom:1px solid #00000018"
    ]
    [ HH.span [ cls "hg-accordion__label", sty "font-weight:600;color:#2b2b2b" ]
        [ HH.text input.label ]
    , HH.span [ sty "display:flex;gap:8px;align-items:baseline;color:#7a7a7a;font-size:0.85em" ]
        [ maybe (HH.text "") (\s -> HH.span [ cls "hg-accordion__sub" ] [ HH.text s ]) input.sub
        , HH.span [ cls "hg-accordion__chevron" ] [ HH.text "▾" ]
        ]
    ]

-- | The collapsed tab: a thin, full-height strip with the rotated label.
-- | Clicking requests an expand.
headerCollapsed :: forall m. Input -> H.ComponentHTML Action () m
headerCollapsed input =
  HH.div
    [ clss [ "hg-accordion", "hg-accordion--collapsed" ]
    , HE.onClick \_ -> Toggle
    , sty $ rowBase input
        <> ";flex:0 0 30px;min-width:30px;justify-content:center;align-items:center"
    ]
    [ HH.span
        [ cls "hg-accordion__label"
        , sty "transform:rotate(-90deg);white-space:nowrap;color:#2b2b2b;letter-spacing:0.08em"
        ]
        [ HH.text input.label ]
    ]

-- | Shared row chrome; reflects `disabled` in cursor + opacity.
rowBase :: Input -> String
rowBase input =
  "display:flex;font-family:system-ui,sans-serif;user-select:none;"
    <> if input.disabled then "cursor:default;opacity:0.5" else "cursor:pointer"
