-- | Shared core for the two accordion orientations. Not a widget itself — it
-- | carries no public `component`; the orientation-fixed `VAccordion` and
-- | `HAccordion` modules wrap `mkComponent` and re-export this surface so each
-- | satisfies the uniform-exports rule (CONTRACT.md rule 4).
-- |
-- | One accordion is **one panel's interactive header**. Clicking it emits
-- | `Toggled` carrying the requested new `open` value; the component never flips
-- | its own state. The PARENT owns `open` and renders the panel BODY itself
-- | (`if open then [body] else []`) — see CONTRACT.md rule 5.
-- |
-- | The toggle is debounced inside the component (configurable via
-- | `Input.debounce`) — the machinery Triggerfish hand-rolls as `markTap` /
-- | `lastTapMicros` to stop a 30 fps re-render double-dispatch from cancelling a
-- | panel flip, absorbed once for everyone.
-- |
-- | **Orientation** is the only thing the two wrappers differ on, and it changes
-- | only the *collapsed* rendering:
-- |
-- |   * `Vertical` — panels stack top-to-bottom. The header is always a
-- |     full-width horizontal bar; collapsing just hides the parent's body and
-- |     flips the chevron (▾ → ▸). This is the common web accordion.
-- |   * `Horizontal` — panels sit side-by-side as columns (Triggerfish). A
-- |     collapsed panel shrinks to a thin vertical strip with a rotated label.
module Hylograph.Halogen.UI.Accordion.Internal
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , Orientation(..)
  , mkComponent
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
import Hylograph.Halogen.UI.Style (sty, cls, clss, ink, inkSoft, line)

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

-- | Which way the accordion lays out. Fixed per-component by the wrapper.
data Orientation = Vertical | Horizontal

data Action
  = Receive Input
  | Toggle

-- | State mirrors `Input` (so `render` has something to read) plus the debounce
-- | generation counter — the only ephemeral state this widget keeps.
type State =
  { input :: Input
  , version :: Int
  }

-- | Build an accordion component fixed to one orientation.
mkComponent :: forall m. MonadAff m => Orientation -> H.Component Query Input Output m
mkComponent orientation =
  H.mkComponent
    { initialState: \input -> { input, version: 0 }
    , render: render orientation
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

render :: forall m. Orientation -> State -> H.ComponentHTML Action () m
render orientation { input } = case orientation of
  -- Vertical never rotates: the header is always a full-width bar, open or not.
  Vertical -> headerBar input
  -- Horizontal collapses to a thin rotated strip; open is the same bar.
  Horizontal ->
    if input.open then headerBar input else headerCollapsedStrip input

-- | The full-width horizontal header bar: label on the left, optional sub +
-- | chevron on the right. The chevron glyph reflects `open` (▾ open, ▸ closed),
-- | and the bottom rule shows only while open. Clicking requests a toggle.
headerBar :: forall m. Input -> H.ComponentHTML Action () m
headerBar input =
  HH.div
    [ clss [ "hg-accordion", if input.open then "hg-accordion--open" else "hg-accordion--collapsed" ]
    , HE.onClick \_ -> Toggle
    , sty $ rowBase input
        <> ";justify-content:space-between;padding:6px 2px"
        <> (if input.open then ";border-bottom:1px solid " <> line else "")
    ]
    [ HH.span [ cls "hg-accordion__label", sty $ "font-weight:600;color:" <> ink ]
        [ HH.text input.label ]
    , HH.span [ sty $ "display:flex;gap:8px;align-items:baseline;font-size:0.85em;color:" <> inkSoft ]
        [ maybe (HH.text "") (\s -> HH.span [ cls "hg-accordion__sub" ] [ HH.text s ]) input.sub
        , HH.span [ cls "hg-accordion__chevron" ]
            [ HH.text (if input.open then "▾" else "▸") ]
        ]
    ]

-- | The collapsed strip for a HORIZONTAL accordion: a thin, full-height column
-- | with the rotated label. Clicking requests an expand.
headerCollapsedStrip :: forall m. Input -> H.ComponentHTML Action () m
headerCollapsedStrip input =
  HH.div
    [ clss [ "hg-accordion", "hg-accordion--collapsed", "hg-accordion--strip" ]
    , HE.onClick \_ -> Toggle
    , sty $ rowBase input
        <> ";width:30px;min-width:30px;height:100%;justify-content:center;align-items:center"
    ]
    [ HH.span
        [ cls "hg-accordion__label"
        , sty $ "transform:rotate(-90deg);white-space:nowrap;letter-spacing:0.08em;color:" <> ink
        ]
        [ HH.text input.label ]
    ]

-- | Shared row chrome; reflects `disabled` in cursor + opacity.
rowBase :: Input -> String
rowBase input =
  "display:flex;font-family:system-ui,sans-serif;user-select:none;"
    <> if input.disabled then "cursor:default;opacity:0.5" else "cursor:pointer"
