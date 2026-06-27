-- | A controlled single-select dropdown, optionally typeahead-filtered, with
-- | three presentations of the same option data:
-- |
-- |   * **flat** (`defaultInput`) — one un-headed list.
-- |   * **grouped** (`groupedInput`) — named groups inline, non-selectable
-- |     headers over indented leaves.
-- |   * **cascade** (`cascadingInput`, `cascade = true`) — the same groups as a
-- |     macOS-style fly-out menu: top-level family rows, each hovering/keying open
-- |     a submenu to the side. Hover-intent, click/touch open, full keyboard
-- |     navigation, and viewport-edge flip are all handled inside the widget.
-- |
-- | Two kinds of state, deliberately split (see CONTRACT.md):
-- |   * `selected` is CONTROLLED — it is app-meaningful, so the parent owns it
-- |     and `Selected` requests a change.
-- |   * `open`, `query`, and the cascade interaction state (`hovered`,
-- |     `leafFocus`, `flipLeft`) are EPHEMERAL — the widget owns them and never
-- |     surfaces them.
module Hylograph.Halogen.UI.Select
  ( Option
  , OptionGroup
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  , groupedInput
  , cascadingInput
  ) where

import Prelude

import Data.Array (filter, find, findIndex, index, length, mapWithIndex, null)
import Data.Array (concatMap) as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), contains)
import Data.String.Common (toLower)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.DOM.Element (getBoundingClientRect)
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.HTMLElement (blur, toElement)
import Web.HTML.Window (innerWidth)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Hylograph.Halogen.UI.Style (sty, cls, ink, inkSoft, accent, surface, surfaceAlt, line, uiFont)

type Option = { value :: String, label :: String }

-- | A named group of options. The same data drives all three grouped
-- | presentations (see `cascade`): inline headers, or fly-out submenus.
type OptionGroup = { label :: String, options :: Array Option }

-- | Two ways to supply options, and they coexist:
-- |   * `options` — a flat, un-headed list (the original surface).
-- |   * `groups`  — named sections.
-- | When `groups` is non-empty the dropdown renders grouped; otherwise it falls
-- | back to the flat `options`. `selected` is still a single controlled value
-- | resolved across *both* sources.
-- |
-- | `cascade` chooses the grouped *presentation*: `false` (default) is the
-- | inline grouped list; `true` is the fly-out submenu menu. No effect without
-- | `groups`.
type Input =
  { options :: Array Option
  , groups :: Array OptionGroup
  , cascade :: Boolean
  , selected :: Maybe String
  , placeholder :: String
  , searchable :: Boolean
  , disabled :: Boolean
  }

-- | The flat on-ramp, unchanged: `groups` defaults to `[]`, so every existing
-- | caller compiles and behaves exactly as before.
defaultInput :: Array Option -> Input
defaultInput options =
  { options, groups: [], cascade: false, selected: Nothing, placeholder: "Select…", searchable: false, disabled: false }

-- | The grouped on-ramp (inline list): named groups, headers over their leaves.
groupedInput :: Array OptionGroup -> Input
groupedInput groups =
  { options: [], groups, cascade: false, selected: Nothing, placeholder: "Select…", searchable: false, disabled: false }

-- | The cascade on-ramp (fly-out submenus): named groups, each revealed as a
-- | side popup on hover/click/keyboard. Same controlled `selected` and `Selected`
-- | output as every other shape — only the panel interaction differs.
cascadingInput :: Array OptionGroup -> Input
cascadingInput groups =
  { options: [], groups, cascade: true, selected: Nothing, placeholder: "Select…", searchable: false, disabled: false }

data Output = Selected String

data Query a = Set (Maybe String) a

type Slot = H.Slot Query Output

data Action
  = Receive Input
  | Toggle
  | Close
  | Pick String
  | SetQuery String
  | EnterGroup String          -- cascade: hover/click a parent → open its submenu
  | LeaveSubmenus              -- cascade: pointer left → schedule a delayed close
  | HandleKey KeyboardEvent

type State =
  { input :: Input
  , open :: Boolean            -- ephemeral
  , query :: String            -- ephemeral
  -- Cascade keeps focus and openness apart (the spec's keyboard model): the
  -- *highlighted* parent and the parent whose submenu is *open* can differ —
  -- ↑/↓ move the highlight without opening; →/Enter open it.
  , focusParent :: Maybe String -- ephemeral: keyboard-highlighted parent
  , hovered :: Maybe String     -- ephemeral: the parent whose submenu is OPEN
  , hoverGen :: Int             -- ephemeral: hover-intent debounce generation
  , leafFocus :: Maybe Int      -- ephemeral: keyboard focus inside a submenu
  , flipLeft :: Boolean         -- ephemeral: submenu flipped left (edge overflow)
  }

-- The ref on the first panel, read to measure-and-flip the submenu on open.
panelRef :: H.RefLabel
panelRef = H.RefLabel "hg-select-panel"

-- The ref on the focusable root, blurred on commit so the widget stops
-- capturing keys (it is `tabIndex 0` + `onKeyDown`) the moment it is no longer
-- the user's deliberate focus. Without this, a host page's global key handler
-- (e.g. Space-to-audition) double-fires: the host acts AND the still-focused
-- Select re-opens on the same key.
rootRef :: H.RefLabel
rootRef = H.RefLabel "hg-select-root"

-- The hover-intent close delay (the diagonal-travel grace period).
hoverDelay :: Milliseconds
hoverDelay = Milliseconds 180.0

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input ->
        { input, open: false, query: "", focusParent: Nothing, hovered: Nothing, hoverGen: 0, leafFocus: Nothing, flipLeft: false }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }

-- A non-empty typeahead is "active": it temporarily flattens the cascade.
searchActive :: State -> Boolean
searchActive st = st.input.searchable && st.query /= ""

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ _ { input = input }              -- leaves all ephemeral state untouched
  Toggle -> do
    { input } <- H.get
    when (not input.disabled) do
      opening <- H.gets (not <<< _.open)
      let selGroup = if opening then input.selected >>= groupOf input else Nothing
      H.modify_ \s -> s
        { open = opening
        , query = ""
        -- On open, pre-expand the submenu holding the current selection (and
        -- highlight that parent).
        , hovered = selGroup
        , focusParent = selGroup
        , leafFocus = Nothing
        , hoverGen = s.hoverGen + 1
        , flipLeft = false
        }
      -- Measure-and-flip once the panel is in the DOM (next tick).
      when opening $ void $ H.fork (liftAff (delay (Milliseconds 0.0)) *> measureFlip)
  Close ->
    H.modify_ _ { open = false, query = "", hovered = Nothing, focusParent = Nothing, leafFocus = Nothing }
  SetQuery q ->
    H.modify_ _ { query = q }
  EnterGroup label ->
    -- Mouse hover: highlight AND open, immediately; bump the generation so any
    -- pending close is cancelled.
    H.modify_ \s -> s { hovered = Just label, focusParent = Just label, leafFocus = Nothing, hoverGen = s.hoverGen + 1 }
  LeaveSubmenus -> do
    -- Don't close immediately — wait out the hover-intent delay; only the latest
    -- leave wins (generation guard), so moving onto another parent cancels it.
    next <- H.modify \s -> s { hoverGen = s.hoverGen + 1 }
    let mine = next.hoverGen
    void $ H.fork do
      liftAff (delay hoverDelay)
      s' <- H.get
      when (s'.hoverGen == mine) (H.modify_ _ { hovered = Nothing, focusParent = Nothing, leafFocus = Nothing })
  Pick value -> do
    H.modify_ _ { open = false, query = "", hovered = Nothing, focusParent = Nothing, leafFocus = Nothing }
    -- Relinquish keyboard focus on commit: the control should not keep
    -- shadowing the host page's keys after the user has chosen.
    blurRoot
    H.raise (Selected value)
  HandleKey ev ->
    handleKey ev

-- Drop focus from the focusable root element (see `rootRef`).
blurRoot :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
blurRoot = do
  mEl <- H.getHTMLElementRef rootRef
  case mEl of
    Just el -> liftEffect (blur el)
    Nothing -> pure unit

-- Measure the first panel's right edge against the viewport; flip the submenu to
-- the left when it would overflow. A one-shot measure on open (per the spec) —
-- no live popper.
measureFlip :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
measureFlip = do
  mEl <- H.getHTMLElementRef panelRef
  case mEl of
    Nothing -> pure unit
    Just el -> do
      rect <- liftEffect (getBoundingClientRect (toElement el))
      w <- liftEffect (innerWidth =<< window)
      -- ~210px is the submenu's min-width plus a little breathing room.
      H.modify_ _ { flipLeft = rect.right + 210.0 > toNumber w }

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Set mValue a -> do
    case mValue of
      Just value -> H.raise (Selected value)
      Nothing -> pure unit
    pure (Just a)

-- The cascade keyboard model. First panel: ↑/↓ among parents (which opens that
-- submenu), →/Enter/Space enters the submenu, Escape closes. Submenu: ↑/↓ among
-- leaves, Enter/Space selects, ←/Escape returns to the parents.
handleKey :: forall m. MonadAff m => KeyboardEvent -> H.HalogenM State Action () Output m Unit
handleKey ev = do
  st <- H.get
  let
    k = KE.key ev
    prevent = liftEffect (preventDefault (KE.toEvent ev))
  if not st.open then
    when (k == "Enter" || k == " " || k == "Spacebar" || k == "ArrowDown") (prevent *> handleAction Toggle)
  else if st.input.cascade && not (null st.input.groups) && not (searchActive st) then do
    let
      groups = st.input.groups
      labels = map _.label groups
      n = length groups
      -- Parent navigation tracks the *highlight* (focusParent), not openness.
      curIdx = fromMaybe 0 (st.focusParent >>= \h -> findIndex (_ == h) labels)
      wrap len i = if len <= 0 then 0 else ((i `mod` len) + len) `mod` len
      -- Leaf navigation runs over the OPEN submenu's options.
      curOpts = fromMaybe [] (st.hovered >>= \h -> map _.options (find (\g -> g.label == h) groups))
      -- ↑/↓ at the parent level: move the highlight and CLOSE any open submenu
      -- (so arrowing previews nothing until you commit with → / Enter).
      gotoParent i = H.modify_ \s -> s { focusParent = index labels (wrap n i), hovered = Nothing, leafFocus = Nothing }
      moveLeaf d = let m = length curOpts in when (m > 0) (H.modify_ _ { leafFocus = Just (wrap m (fromMaybe 0 st.leafFocus + d)) })
      -- → / Enter at the parent level: open the highlighted parent's submenu and
      -- drop focus onto its selected leaf (or the first).
      enterSubmenu =
        let target = case st.focusParent of
              Just p -> Just p
              Nothing -> index labels 0
            opts = fromMaybe [] (target >>= \h -> map _.options (find (\g -> g.label == h) groups))
            selIdx = st.input.selected >>= \v -> findIndex (\o -> o.value == v) opts
        in H.modify_ \s -> s { hovered = target, focusParent = target, hoverGen = s.hoverGen + 1, leafFocus = Just (fromMaybe 0 selIdx) }
      activate = case st.leafFocus of
        Nothing -> enterSubmenu
        Just i -> case index curOpts i of
          Just o -> handleAction (Pick o.value)
          Nothing -> pure unit
    case k of
      "ArrowDown" -> prevent *> case st.leafFocus of
        Nothing -> if isJust st.focusParent then gotoParent (curIdx + 1) else gotoParent 0
        Just _ -> moveLeaf 1
      "ArrowUp" -> prevent *> case st.leafFocus of
        Nothing -> if isJust st.focusParent then gotoParent (curIdx - 1) else gotoParent 0
        Just _ -> moveLeaf (-1)
      "ArrowRight" -> prevent *> enterSubmenu
      -- ← from a submenu: close it back to the highlighted parent (which stays).
      "ArrowLeft" -> prevent *> H.modify_ _ { leafFocus = Nothing, hovered = Nothing }
      "Enter" -> prevent *> activate
      " " -> prevent *> activate
      "Spacebar" -> prevent *> activate
      "Escape" -> prevent *> case st.leafFocus of
        -- First Escape inside a submenu: close it back to the parent list.
        Just _ -> H.modify_ _ { leafFocus = Nothing, hovered = Nothing }
        -- At the parent list: close the whole control.
        Nothing -> handleAction Close
      _ -> pure unit
  else case k of
    "Escape" -> prevent *> handleAction Close
    _ -> pure unit

-- Every leaf option, flat ones plus those nested in groups — the set across
-- which `selected` is resolved and search is run.
allOptions :: Input -> Array Option
allOptions input = input.options <> Array.concatMap _.options input.groups

-- Does this leaf survive the current typeahead query? (Always, when not searching.)
matchesQuery :: State -> Option -> Boolean
matchesQuery { input, query } o =
  not input.searchable || query == "" || contains (Pattern (toLower query)) (toLower o.label)

-- Flat options matching the current query (all of them when not searchable).
visibleOptions :: State -> Array Option
visibleOptions st = filter (matchesQuery st) st.input.options

-- Groups with their leaves filtered to the query; a group with no surviving
-- leaf is dropped entirely (header and all), per the requirement.
visibleGroups :: State -> Array OptionGroup
visibleGroups st =
  filter (\g -> not (null g.options))
    (map (\g -> g { options = filter (matchesQuery st) g.options }) st.input.groups)

selectedLabel :: Input -> String
selectedLabel input =
  case input.selected of
    Nothing -> input.placeholder
    Just v -> maybe input.placeholder _.label (find (\o -> o.value == v) (allOptions input))

-- The label of the group that contains a given value, if any — used to
-- pre-expand the right cascade submenu when the menu opens.
groupOf :: Input -> String -> Maybe String
groupOf input v =
  map _.label (find (\g -> isJust (find (\o -> o.value == v) g.options)) input.groups)

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div
    [ cls "hg-select"
    , HP.ref rootRef
    , HP.tabIndex 0
    , HE.onKeyDown HandleKey
    , sty $ "position:relative;display:inline-block;min-width:180px;outline:none;font-family:" <> uiFont
        <> ";" <> if st.input.disabled then "opacity:0.5" else ""
    ]
    ( [ control ] <> if st.open then [ backdrop, panel ] else [] )
  where
  isPlaceholder = st.input.selected == Nothing

  -- A transparent full-screen catcher: a click outside the panel closes.
  backdrop =
    HH.div
      [ cls "hg-select__backdrop"
      , HE.onClick \_ -> Close
      , sty "position:fixed;inset:0;z-index:40;background:transparent"
      ]
      []

  control =
    HH.div
      [ cls "hg-select__control"
      , HE.onClick \_ -> Toggle
      , sty $ "display:flex;align-items:center;justify-content:space-between;gap:8px;position:relative;z-index:45;"
          <> "padding:6px 10px;border:1px solid " <> line <> ";border-radius:var(--hg-radius,6px);"
          <> "background:" <> surface <> ";font-size:13px;"
          <> (if st.input.disabled then "cursor:default;" else "cursor:pointer;")
          <> "color:" <> (if isPlaceholder then inkSoft else ink)
      ]
      [ HH.span_ [ HH.text (selectedLabel st.input) ]
      , HH.span [ sty $ "color:" <> inkSoft <> ";font-size:11px" ] [ HH.text "▾" ]
      ]

  -- Grouped when any group is supplied; otherwise the original flat list.
  grouped = not (null st.input.groups)
  -- Cascade is the fly-out presentation of a grouped menu.
  cascade = st.input.cascade && grouped

  panel =
    HH.div
      [ cls "hg-select__panel"
      , HP.ref panelRef
      , sty $ "position:absolute;top:calc(100% + 4px);z-index:50;"
          <> (if cascade then "left:0;min-width:100%;width:max-content;" else "left:0;right:0;")
          <> "background:" <> surface <> ";border:1px solid " <> line <> ";border-radius:var(--hg-radius,6px);"
          <> "box-shadow:0 6px 20px #00000022;"
          -- Cascade must NOT clip — its submenus escape the panel box.
          <> (if cascade then "overflow:visible" else "max-height:240px;overflow:auto")
      ]
      ( (if st.input.searchable then [ searchBox ] else [])
          <> panelBody
      )

  panelBody =
    if cascade && not (searchActive st) then map cascadeRow st.input.groups
    else if cascade then map (optionRow false) (filter (matchesQuery st) (allOptions st.input))
    else if grouped then map groupSection (visibleGroups st)
    else map (optionRow false) (visibleOptions st)

  searchBox =
    HH.input
      [ cls "hg-select__search"
      , HP.value st.query
      , HP.placeholder "Filter…"
      , HE.onValueInput SetQuery
      , sty $ "width:100%;box-sizing:border-box;padding:7px 10px;border:0;border-bottom:1px solid "
          <> line <> ";font-size:12px;outline:none;font-family:" <> uiFont
      ]

  -- A named group (inline presentation): a non-interactive header over its
  -- nested, indented leaves.
  groupSection g =
    HH.div [ cls "hg-select__group" ]
      ( [ groupHeader g.label ] <> map (optionRow true) g.options )

  groupHeader label =
    HH.div
      [ cls "hg-select__group-label"
      , sty $ "padding:8px 10px 4px;font-size:10px;font-weight:700;letter-spacing:0.08em;"
          <> "text-transform:uppercase;color:" <> inkSoft <> ";user-select:none;pointer-events:none"
      ]
      [ HH.text label ]

  -- A cascade top-level parent row: the group label + a ▸, hover/click/keyboard
  -- opening its submenu. The submenu is an absolutely-positioned child, so moving
  -- the cursor right into it stays inside this row (no flicker) — `onMouseLeave`
  -- only fires on exiting both, and is itself softened by the hover-intent delay.
  cascadeRow g =
    let
      isOpen = st.hovered == Just g.label          -- submenu showing → --active
      isFocused = st.focusParent == Just g.label    -- keyboard highlight
    in
    HH.div
      [ cls $ "hg-select__parent"
          <> (if isOpen then " hg-select__parent--active" else "")
          <> (if isFocused then " is-focused" else "")
      , sty $ "position:relative;display:flex;align-items:center;justify-content:space-between;gap:18px;"
          <> "padding:7px 10px;cursor:pointer;font-size:13px;white-space:nowrap;color:" <> ink
          <> (if isOpen || isFocused then ";background:" <> surfaceAlt else "")
      , HE.onMouseEnter \_ -> EnterGroup g.label
      , HE.onMouseLeave \_ -> LeaveSubmenus
      , HE.onClick \_ -> EnterGroup g.label
      ]
      ( [ HH.span_ [ HH.text g.label ]
        , HH.span [ cls "hg-select__caret", sty $ "color:" <> inkSoft <> ";font-size:11px" ] [ HH.text "▸" ]
        ] <> (if isOpen then [ submenu g ] else [])
      )

  -- The fly-out: its own bordered panel just past the parent's right edge, or to
  -- its left when `flipLeft` (measured on open).
  submenu g =
    HH.div
      [ cls ("hg-select__submenu" <> if st.flipLeft then " hg-select__submenu--left" else "")
      , sty $ "position:absolute;top:-1px;z-index:60;min-width:190px;"
          <> (if st.flipLeft then "right:100%;left:auto;" else "left:100%;")
          <> "background:" <> surface <> ";border:1px solid " <> line <> ";border-radius:var(--hg-radius,6px);"
          <> "box-shadow:0 6px 20px #00000022;max-height:280px;overflow:auto"
      ]
      (mapWithIndex (submenuLeaf) g.options)

  -- A submenu leaf, with keyboard-focus highlight (`is-focused`) layered on the
  -- selected highlight (`is-selected`).
  submenuLeaf i o =
    let
      chosen = st.input.selected == Just o.value
      focused = st.leafFocus == Just i
    in HH.div
         [ cls $ "hg-select__option"
             <> (if chosen then " is-selected" else "")
             <> (if focused then " is-focused" else "")
         , HE.onClick \_ -> Pick o.value
         , sty $ "padding:7px 10px;cursor:pointer;font-size:13px;"
             <> (if chosen then "background:" <> surfaceAlt <> ";color:" <> accent <> ";font-weight:600"
                 else if focused then "background:" <> surfaceAlt <> ";color:" <> ink
                 else "color:" <> ink)
         ]
         [ HH.text o.label ]

  -- `nested` indents leaves that live inside an inline group; flat leaves flush.
  optionRow nested o =
    let chosen = st.input.selected == Just o.value
    in HH.div
         [ cls $ "hg-select__option"
             <> (if nested then " hg-select__option--nested" else "")
             <> (if chosen then " is-selected" else "")
         , HE.onClick \_ -> Pick o.value
         , sty $ "padding:7px 10px;cursor:pointer;font-size:13px;"
             <> (if nested then "padding-left:22px;" else "")
             <> (if chosen
                   then "background:" <> surfaceAlt <> ";color:" <> accent <> ";font-weight:600"
                   else "color:" <> ink)
         ]
         [ HH.text o.label ]
