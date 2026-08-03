-- | A controlled **multi**-select dropdown: a compact control whose label
-- | summarises the current selection, opening a popover of options each with a
-- | checkbox that toggles it in or out. Several options are active at once.
-- |
-- | It is the sibling of `Halogen.Widgets.Select` — same popover look, same
-- | `Style` tokens, same controlled/ephemeral split — but a genuinely different
-- | contract, so it lives in its own module rather than as a `multi` flag on
-- | `Select`. That keeps the single-select path byte-for-byte unchanged and
-- | keeps each component's `Input`/`Output` honest about its own shape (an
-- | `Array String` selection, not a `Maybe String`).
-- |
-- | Two kinds of state, deliberately split (as in `Select`, see CONTRACT.md):
-- |   * `selected` is CONTROLLED — an `Array String`, app-meaningful, owned by
-- |     the parent; the widget only *requests* a change via `SelectedMany`.
-- |   * `open`, `query`, and the keyboard `focus` are EPHEMERAL — the widget
-- |     owns them and never surfaces them.
module Halogen.Widgets.MultiSelect
  ( Option
  , OptionGroup
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  , groupedInput
  ) where

import Prelude

import Data.Array (concatMap, elem, filter, index, length, mapWithIndex, null, snoc)
import Data.Array (find) as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), contains)
import Data.String.Common (joinWith, toLower)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.Element (getBoundingClientRect)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement (blur, toElement)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Halogen.Widgets.Style (sty, cls, ink, inkSoft, accent, surface, surfaceAlt, line, uiFont)

type Option = { value :: String, label :: String }

-- | A named group of options — renders as an inline, non-selectable header over
-- | its indented, checkable leaves. (No fly-out/cascade presentation here; a
-- | multi-select is about toggling, and a flat-ish list reads best for that.)
type OptionGroup = { label :: String, options :: Array Option }

-- | Two ways to supply options, and they coexist (mirroring `Select`):
-- |   * `options` — a flat, un-headed list.
-- |   * `groups`  — named sections with inline headers.
-- |
-- | `selected` is the controlled set of chosen `value`s (an `Array String`,
-- | order-insensitive; membership is what matters). `maxLabels` governs the
-- | control's summary: with `n` items selected, show the comma-joined labels
-- | when `n <= maxLabels`, else collapse to `"n selected"`. `minWidth` is an
-- | optional CSS length for the control box (`Nothing` = the `180px` default),
-- | exactly like `Select.minWidth`.
type Input =
  { options :: Array Option
  , groups :: Array OptionGroup
  , selected :: Array String
  , placeholder :: String
  , searchable :: Boolean
  , disabled :: Boolean
  , minWidth :: Maybe String
  , maxLabels :: Int
  }

-- | The flat on-ramp: a plain list of checkable options.
defaultInput :: Array Option -> Input
defaultInput options =
  { options, groups: [], selected: [], placeholder: "Select…", searchable: false, disabled: false, minWidth: Nothing, maxLabels: 2 }

-- | The grouped on-ramp: named sections with inline headers over their leaves.
groupedInput :: Array OptionGroup -> Input
groupedInput groups =
  { options: [], groups, selected: [], placeholder: "Select…", searchable: false, disabled: false, minWidth: Nothing, maxLabels: 2 }

-- | Raised whenever the user toggles an option, carrying the FULL new selection
-- | (not a per-item delta) — so the parent just stores the array it is handed.
data Output = SelectedMany (Array String)

-- | Imperative set of the controlled selection, symmetric with `Select.Set`.
data Query a = SetMany (Array String) a

type Slot = H.Slot Query Output

data Action
  = Receive Input
  | Toggle                     -- open/close the popover
  | Close
  | ToggleValue String         -- flip one option's membership
  | SetQuery String
  | HandleKey KeyboardEvent

type State =
  { input :: Input
  , open :: Boolean            -- ephemeral
  , query :: String            -- ephemeral
  , focus :: Maybe Int         -- ephemeral: keyboard focus over the visible rows
  -- ephemeral: the control's viewport rect, measured on open, so the panel can
  -- render `position:fixed` and escape any ancestor `overflow` clip (a host's
  -- scrolling pane). Nothing = not yet measured (fallback to absolute).
  , anchor :: Maybe { left :: Number, bottom :: Number, width :: Number }
  }

rootRef :: H.RefLabel
rootRef = H.RefLabel "hg-multiselect-root"

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input ->
        { input, open: false, query: "", focus: Nothing, anchor: Nothing }
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
    H.modify_ _ { input = input }              -- leaves ephemeral state untouched
  Toggle -> do
    { input } <- H.get
    when (not input.disabled) do
      opening <- H.gets (not <<< _.open)
      manchor <- if opening then measureRoot else pure Nothing
      H.modify_ _ { open = opening, query = "", focus = Nothing, anchor = manchor }
  Close ->
    H.modify_ _ { open = false, query = "", focus = Nothing }
  SetQuery q ->
    H.modify_ _ { query = q, focus = Nothing }
  ToggleValue value -> do
    st <- H.get
    -- Compute the new selection and RAISE it; the popover stays open so several
    -- toggles chain without reopening (the multi-select convention).
    let
      cur = st.input.selected
      next = if value `elem` cur then filter (_ /= value) cur else snoc cur value
    H.raise (SelectedMany next)
  HandleKey ev ->
    handleKey ev

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  SetMany values a -> do
    H.raise (SelectedMany values)
    pure (Just a)

-- Measure the control (root) rect for the fixed-panel anchor, before the panel
-- is inserted, so the box is just the control.
measureRoot :: forall m. MonadAff m => H.HalogenM State Action () Output m (Maybe { left :: Number, bottom :: Number, width :: Number })
measureRoot = do
  mEl <- H.getHTMLElementRef rootRef
  case mEl of
    Nothing -> pure Nothing
    Just el -> do
      r <- liftEffect (getBoundingClientRect (toElement el))
      pure (Just { left: r.left, bottom: r.bottom, width: r.width })

blurRoot :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
blurRoot = do
  mEl <- H.getHTMLElementRef rootRef
  maybe (pure unit) (liftEffect <<< blur) mEl

-- A deliberately simpler keyboard model than the cascade: closed → open on
-- Enter/Space/↓; open → ↑/↓ move focus over the visible rows, Enter/Space
-- toggle the focused row (popover stays open), Escape closes.
handleKey :: forall m. MonadAff m => KeyboardEvent -> H.HalogenM State Action () Output m Unit
handleKey ev = do
  st <- H.get
  let
    k = KE.key ev
    prevent = liftEffect (preventDefault (KE.toEvent ev))
  if not st.open then
    when (k == "Enter" || k == " " || k == "Spacebar" || k == "ArrowDown") (prevent *> handleAction Toggle)
  else do
    let
      rows = visibleFlat st
      n = length rows
      wrap i = if n <= 0 then 0 else ((i `mod` n) + n) `mod` n
      move d = when (n > 0) (H.modify_ _ { focus = Just (wrap (fromMaybe (-1) st.focus + d)) })
      activate = case st.focus >>= index rows of
        Just o -> handleAction (ToggleValue o.value)
        Nothing -> pure unit
    case k of
      "ArrowDown" -> prevent *> move 1
      "ArrowUp" -> prevent *> move (-1)
      "Enter" -> prevent *> activate
      " " -> prevent *> activate
      "Spacebar" -> prevent *> activate
      "Escape" -> prevent *> (blurRoot *> handleAction Close)
      _ -> pure unit

-- Every leaf option, flat ones plus those nested in groups.
allOptions :: Input -> Array Option
allOptions input = input.options <> concatMap _.options input.groups

matchesQuery :: State -> Option -> Boolean
matchesQuery { input, query } o =
  not input.searchable || query == "" || contains (Pattern (toLower query)) (toLower o.label)

-- The visible options as one flat array, in render order (flat list first, then
-- each group's surviving leaves). This is the array keyboard focus indexes into,
-- and the order in which the panel body draws its checkable rows.
visibleFlat :: State -> Array Option
visibleFlat st =
  filter (matchesQuery st) st.input.options
    <> concatMap (filter (matchesQuery st) <<< _.options) st.input.groups

visibleGroups :: State -> Array OptionGroup
visibleGroups st =
  filter (\g -> not (null g.options))
    (map (\g -> g { options = filter (matchesQuery st) g.options }) st.input.groups)

-- The control's summary label: placeholder when empty; the comma-joined labels
-- while the count is within `maxLabels`; otherwise the count ("n selected").
summaryLabel :: Input -> String
summaryLabel input =
  case input.selected of
    [] -> input.placeholder
    sel ->
      let
        labelFor v = maybe v _.label (Array.find (\o -> o.value == v) (allOptions input))
        n = length sel
      in
        if n <= input.maxLabels then joinWith ", " (map labelFor sel)
        else show n <> " selected"

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div
    [ cls "hg-multiselect"
    , HP.ref rootRef
    , HP.tabIndex 0
    , HE.onKeyDown HandleKey
    , sty $ "position:relative;display:inline-block;min-width:" <> fromMaybe "180px" st.input.minWidth
        <> ";outline:none;font-family:" <> uiFont
        <> ";" <> if st.input.disabled then "opacity:0.5" else ""
    ]
    ( [ control ] <> if st.open then [ backdrop, panel ] else [] )
  where
  isEmpty = null st.input.selected

  backdrop =
    HH.div
      [ cls "hg-multiselect__backdrop"
      , HE.onClick \_ -> Close
      , sty "position:fixed;inset:0;z-index:40;background:transparent"
      ]
      []

  control =
    HH.div
      [ cls "hg-multiselect__control"
      , HE.onClick \_ -> Toggle
      , sty $ "display:flex;align-items:center;justify-content:space-between;gap:8px;position:relative;z-index:45;"
          <> "padding:6px 10px;border:1px solid " <> line <> ";border-radius:var(--hw-radius,6px);"
          <> "background:" <> surface <> ";font-size:13px;"
          <> (if st.input.disabled then "cursor:default;" else "cursor:pointer;")
          <> "color:" <> (if isEmpty then inkSoft else ink)
      ]
      [ HH.span_ [ HH.text (summaryLabel st.input) ]
      , HH.span [ sty $ "color:" <> inkSoft <> ";font-size:11px" ] [ HH.text "▾" ]
      ]

  panelPos = case st.anchor of
    Just a ->
      "position:fixed;top:" <> show (a.bottom + 4.0) <> "px;left:" <> show a.left <> "px;"
        <> "width:" <> show a.width <> "px;"
    Nothing ->
      "position:absolute;top:calc(100% + 4px);left:0;right:0;"

  panel =
    HH.div
      [ cls "hg-multiselect__panel"
      , sty $ panelPos <> "z-index:50;"
          <> "background:" <> surface <> ";border:1px solid " <> line <> ";border-radius:var(--hw-radius,6px);"
          <> "box-shadow:0 6px 20px #00000022;max-height:260px;overflow:auto"
      ]
      ( (if st.input.searchable then [ searchBox ] else [])
          <> panelBody
      )

  -- Focus indices run over `visibleFlat`: the flat options first, then each
  -- group's leaves. We render in exactly that order, carrying a running leaf
  -- index so the keyboard highlight lines up with what is drawn.
  panelBody =
    let
      flatVisible = filter (matchesQuery st) st.input.options
      flatRows = mapWithIndex (\i o -> optionRow false i o) flatVisible
      seed = { idx: length flatVisible, html: [] }
      folded = foldl addGroup seed (visibleGroups st)
    in flatRows <> folded.html

  -- Fold one visible group onto the accumulator, indexing its leaves from the
  -- running `idx` so focus stays aligned across the flat list and every group.
  addGroup acc g =
    { idx: acc.idx + length g.options
    , html: acc.html <>
        [ HH.div [ cls "hg-multiselect__group" ]
            ([ groupHeader g.label ] <> mapWithIndex (\j o -> optionRow true (acc.idx + j) o) g.options)
        ]
    }

  searchBox =
    HH.input
      [ cls "hg-multiselect__search"
      , HP.value st.query
      , HP.placeholder "Filter…"
      , HE.onValueInput SetQuery
      , sty $ "width:100%;box-sizing:border-box;padding:7px 10px;border:0;border-bottom:1px solid "
          <> line <> ";font-size:12px;outline:none;font-family:" <> uiFont
      ]

  groupHeader label =
    HH.div
      [ cls "hg-multiselect__group-label"
      , sty $ "padding:8px 10px 4px;font-size:10px;font-weight:700;letter-spacing:0.08em;"
          <> "text-transform:uppercase;color:" <> inkSoft <> ";user-select:none;pointer-events:none"
      ]
      [ HH.text label ]

  -- A checkable leaf: a checkbox glyph + label. `nested` indents leaves inside a
  -- group; `focused` is the keyboard highlight; `chosen` is membership.
  optionRow nested i o =
    let
      chosen = o.value `elem` st.input.selected
      focused = st.focus == Just i
    in HH.div
         [ cls $ "hg-multiselect__option"
             <> (if nested then " hg-multiselect__option--nested" else "")
             <> (if chosen then " is-selected" else "")
             <> (if focused then " is-focused" else "")
         , HE.onClick \_ -> ToggleValue o.value
         , sty $ "display:flex;align-items:center;gap:8px;padding:7px 10px;cursor:pointer;font-size:13px;"
             <> (if nested then "padding-left:22px;" else "")
             <> (if focused then "background:" <> surfaceAlt <> ";" else "")
             <> "color:" <> (if chosen then accent else ink)
             <> (if chosen then ";font-weight:600" else "")
         ]
         [ HH.span
             [ cls "hg-multiselect__box"
             , sty $ "flex:0 0 auto;width:14px;height:14px;line-height:12px;text-align:center;"
                 <> "border:1px solid " <> (if chosen then accent else line) <> ";border-radius:3px;"
                 <> "font-size:11px;color:" <> (if chosen then accent else inkSoft)
                 <> (if chosen then ";background:" <> surfaceAlt else "")
             ]
             [ HH.text (if chosen then "✓" else "") ]
         , HH.span_ [ HH.text o.label ]
         ]
