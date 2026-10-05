-- | An **edge-anchored slide-out drawer**: a panel attached to the left or
-- | right edge of a layout, opened and closed by a small arrow on its inner
-- | edge. Ableton Live's Browser is the model, and Lightroom's left panel; the
-- | first consumers are the preset-and-material browsers of Triggerfish's
-- | machine pages and Conspicillum.
-- |
-- | It comes in two parts, because it sits on the line CONTRACT.md rule 5
-- | draws between leaf widgets and containers:
-- |
-- |   * **`component` is the drawer's edge**: the thin rail with the arrow (and,
-- |     closed, an optional rotated label, as `HAccordion`'s spine has), and the
-- |     resize grip. It is a leaf on the full contract, because it owns the two
-- |     things a function cannot: the drag (document-level mousemove/mouseup, as
-- |     in `Compare`) and the debounce.
-- |   * **`frame` is the layout**: a chrome function that places the parent's
-- |     drawer body, that edge, and the parent's main content side by side
-- |     (`Push`) or the drawer over the content (`Overlay`).
-- |
-- | Why not one component with slots for body and main? A component's HTML is
-- | typed in its own action, so it cannot embed the parent's live content: a
-- | browser full of clickable presets, a machine page full of knobs. The parent
-- | owns `open` anyway (rule 1), so it renders the body and the main content
-- | itself, and hands `frame` its own slotted edge as `handle`. Everything stays
-- | typed in the parent's action, and the edge is still a real component.
-- |
-- | ```purescript
-- | let d = (Drawer.defaultInput "Browser")
-- |           { open = st.browserOpen, width = st.browserWidth, resizable = true }
-- | in Drawer.frame d
-- |      { handle: HH.slot _browser unit Drawer.component d HandleBrowser
-- |      , body: [ presetList st ]
-- |      , main: [ machinePage st ]
-- |      }
-- | ```
-- |
-- | Pass the **same `Input`** to `frame` and to the slot, so the panel and the
-- | rail agree on edge, width and rail width.
-- |
-- | **Controlled.** The parent owns `open` and `width`. The arrow emits
-- | `Toggled` with the requested value; a drag of the grip emits `Resizing`
-- | on every move (feed it back for a live resize) and `Resized` once on
-- | release (persist that one). The widget persists nothing. Widths are
-- | clamped to `minWidth`/`maxWidth` before they are emitted, and `frame`
-- | clamps what it draws too, so a stale stored width cannot break a layout.
-- |
-- | **Keyboard.** The arrow is a real `<button>` with `aria-expanded`,
-- | `aria-controls` (the panel's `panelId`) and an accessible name that says
-- | what a press will do (`showLabel` / `hideLabel`). A resizable grip is a
-- | focusable `role="separator"` that ←/→ step by 16 px and Home/End send to
-- | the bounds. There is **no hotkey** inside the widget: a shortcut such as
-- | Ableton's ⌥⌘B belongs to the page, which owns the state anyway. Bind it
-- | in the parent and flip `open` there.
-- |
-- | **Body lifetime.** The body stays mounted while closed (at width 0, and
-- | `inert` so it leaves the tab order), which is what lets the width ease and
-- | lets a browser keep its scroll position. To unmount it instead, pass
-- | `body: if open then [ … ] else []`.
-- |
-- | **Motion.** `NoMotion` by default, as everywhere in the kit: opt in with
-- | `motion = defaultMotion`. The panel's width and the arrow's turn then ease;
-- | `css/halogen-widgets.css` stops both for `prefers-reduced-motion`, and stops
-- | the width easing during a drag, so the panel follows the pointer.
module Halogen.Widgets.Drawer
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  , Edge(..)
  , Mode(..)
  , Parts
  , frame
  , clampWidth
  , dragWidth
  , toggled
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Subscription as HS
import Halogen.Widgets.Motion (Motion(..), transition)
import Halogen.Widgets.Style (clss, sty, cls, ink, inkSoft, line, surface, surfaceAlt, shadow)
import Web.Event.Event (EventType(..), preventDefault)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

-- | Which edge of the layout the drawer is attached to.
data Edge = Left | Right

derive instance eqEdge :: Eq Edge

-- | `Push`: the drawer and the content sit side by side, and the content
-- | reflows as the drawer opens. `Overlay`: the drawer lies over the content,
-- | which keeps its width (less the rail).
data Mode = Push | Overlay

derive instance eqMode :: Eq Mode

-- | Controlled input. The parent owns `open` and `width`; the rest is config.
-- | All widths are CSS pixels.
type Input =
  { open :: Boolean
  , width :: Number            -- ^ the open drawer's width (clamped to min/max)
  , edge :: Edge
  , mode :: Mode
  , resizable :: Boolean       -- ^ show a drag grip on the inner edge
  , minWidth :: Number
  , maxWidth :: Number
  , railWidth :: Number        -- ^ the edge strip that carries the arrow
  , label :: Maybe String      -- ^ rotated on the closed rail; `Nothing` for none
  , showLabel :: String        -- ^ the arrow's accessible name while closed
  , hideLabel :: String        -- ^ … and while open
  , panelId :: String          -- ^ the panel's DOM id (aria-controls); unique per page
  , debounce :: Milliseconds   -- ^ toggle debounce; `Milliseconds 0.0` (default) disables it
  , motion :: Motion           -- ^ width and arrow easing; `NoMotion` (default) snaps
  , disabled :: Boolean
  }

-- | A starting `Input` named for what the drawer holds: `defaultInput
-- | "Browser"` gives a closed-rail label of "Browser" and the accessible names
-- | "Show browser" / "Hide browser". Open, on the left, pushing, 260 px wide
-- | (160 to 480 if made resizable), on a 20 px rail.
defaultInput :: String -> Input
defaultInput name =
  { open: true
  , width: 260.0
  , edge: Left
  , mode: Push
  , resizable: false
  , minWidth: 160.0
  , maxWidth: 480.0
  , railWidth: 20.0
  , label: if name == "" then Nothing else Just name
  , showLabel: "Show " <> String.toLower name
  , hideLabel: "Hide " <> String.toLower name
  , panelId: "hw-drawer-" <> String.toLower (String.replaceAll (String.Pattern " ") (String.Replacement "-") name)
  , debounce: Milliseconds 0.0
  , motion: NoMotion
  , disabled: false
  }

-- | Requests, not facts. `Toggled` carries the requested `open`. `Resizing`
-- | is a drag in progress (feed it back as `width` for a live resize);
-- | `Resized` is the settled width, once per drag or key press: persist that.
-- | Both widths are already clamped.
data Output
  = Toggled Boolean
  | Resizing Number
  | Resized Number

derive instance eqOutput :: Eq Output

instance showOutput :: Show Output where
  show = case _ of
    Toggled b -> "(Toggled " <> show b <> ")"
    Resizing w -> "(Resizing " <> show w <> ")"
    Resized w -> "(Resized " <> show w <> ")"

-- | Imperative escape hatch. Each raises the same request a gesture would.
data Query a
  = SetOpen Boolean a
  | SetWidth Number a

type Slot = H.Slot Query Output

-- | The parent's three pieces for `frame`: its slotted edge component, the
-- | drawer's body, and the main content. All typed in the parent's action.
type Parts w i =
  { handle :: HH.HTML w i
  , body :: Array (HH.HTML w i)
  , main :: Array (HH.HTML w i)
  }

-- ─── Pure rules (the component's handlers use exactly these) ──────────────

-- | Clamp a width to the input's bounds. A `maxWidth` below `minWidth` is read
-- | as `minWidth`. Useful to the parent too, on restoring a stored width.
clampWidth :: Input -> Number -> Number
clampWidth input w =
  let hi = max input.minWidth input.maxWidth
  in if w < input.minWidth then input.minWidth else if w > hi then hi else w

-- | The width a drag asks for: the width at mousedown plus the pointer's
-- | horizontal travel, signed by edge (a left drawer grows as the pointer
-- | moves right, a right drawer as it moves left), clamped.
dragWidth :: Input -> Number -> Number -> Number
dragWidth input startWidth dx =
  clampWidth input (startWidth + (if input.edge == Left then dx else negate dx))

-- | What a press of the arrow asks for: the opposite of the current `open`,
-- | or nothing while disabled. It reads `open` from the input every time, so
-- | the request always follows the parent's value, never a private copy.
toggled :: Input -> Maybe Output
toggled input = if input.disabled then Nothing else Just (Toggled (not input.open))

-- ─── The component: the drawer's edge ─────────────────────────────────────

data Action
  = Receive Input
  | Toggle
  | StartDrag MouseEvent
  | DragMove Int
  | StopDrag
  | GripKey KeyboardEvent

type Drag =
  { sid :: H.SubscriptionId
  , startX :: Number
  , startWidth :: Number
  , last :: Number
  }

-- | The mirror, plus the debounce generation and any drag in flight.
type State =
  { input :: Input
  , version :: Int
  , drag :: Maybe Drag
  }

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, version: 0, drag: Nothing }
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

  Toggle -> do
    st <- H.get
    case toggled st.input of
      Nothing -> pure unit
      Just out -> case st.input.debounce of
        Milliseconds ms
          | ms <= 0.0 -> H.raise out
          | otherwise -> do
              next <- H.modify \s -> s { version = s.version + 1 }
              let mine = next.version
              void $ H.fork do
                liftAff (delay (Milliseconds ms))
                s' <- H.get
                when (s'.version == mine) (H.raise out)

  StartDrag ev -> do
    st <- H.get
    when (canResize st.input && not (isJust st.drag)) do
      liftEffect (preventDefault (ME.toEvent ev))
      sid <- H.subscribe dragEmitter
      let w = clampWidth st.input st.input.width
      H.modify_ _ { drag = Just { sid, startX: toNumber (ME.clientX ev), startWidth: w, last: w } }

  DragMove x -> do
    st <- H.get
    case st.drag of
      Nothing -> pure unit
      Just d -> do
        let w = dragWidth st.input d.startWidth (toNumber x - d.startX)
        when (w /= d.last) do
          H.modify_ _ { drag = Just d { last = w } }
          H.raise (Resizing w)

  StopDrag -> do
    st <- H.get
    case st.drag of
      Nothing -> pure unit
      Just d -> do
        H.unsubscribe d.sid
        H.modify_ _ { drag = Nothing }
        when (d.last /= d.startWidth) (H.raise (Resized d.last))

  GripKey ev -> do
    st <- H.get
    let
      input = st.input
      w = clampWidth input input.width
      -- ← / → move the inner edge that way; `dragWidth` signs it by edge.
      step dx = Just (dragWidth input w dx)
      target = case KE.key ev of
        "ArrowRight" -> step 16.0
        "ArrowLeft" -> step (-16.0)
        "Home" -> Just input.minWidth
        "End" -> Just (clampWidth input input.maxWidth)
        _ -> Nothing
    when (canResize input) case target of
      Nothing -> pure unit
      Just w' -> do
        liftEffect (preventDefault (KE.toEvent ev))
        when (w' /= w) (H.raise (Resized w'))

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  SetOpen b a -> do
    st <- H.get
    when (not st.input.disabled) (H.raise (Toggled b))
    pure (Just a)
  SetWidth w a -> do
    st <- H.get
    when (not st.input.disabled) (H.raise (Resized (clampWidth st.input w)))
    pure (Just a)

canResize :: Input -> Boolean
canResize input = input.open && input.resizable && not input.disabled

-- Document-level mousemove/mouseup while a drag is in flight (as `Compare`).
dragEmitter :: HS.Emitter Action
dragEmitter = HS.makeEmitter \emit -> do
  moveFn <- eventListener \e -> case ME.fromEvent e of
    Just me -> emit (DragMove (ME.clientX me))
    Nothing -> pure unit
  upFn <- eventListener \_ -> emit StopDrag
  target <- Window.toEventTarget <$> window
  addEventListener (EventType "mousemove") moveFn false target
  addEventListener (EventType "mouseup") upFn false target
  pure do
    removeEventListener (EventType "mousemove") moveFn false target
    removeEventListener (EventType "mouseup") upFn false target

-- | The rail. Closed, the whole rail is the button: arrow at the top, the
-- | label rotated beneath it. Open, the button is the arrow cell at the top
-- | and the rest of the rail is the resize grip (when `resizable`).
render :: forall m. State -> H.ComponentHTML Action () m
render { input, drag } =
  HH.div
    [ clss $
        [ "hw-drawer-edge"
        , edgeClass "hw-drawer-edge" input.edge
        , if input.open then "hw-drawer-edge--open" else "hw-drawer-edge--closed"
        ]
          <> (if isJust drag then [ "hw-drawer-edge--dragging" ] else [])
    , sty $ "display:flex;flex-direction:column;flex:none;box-sizing:border-box;height:100%;"
        <> "width:" <> px input.railWidth <> ";"
        <> "background:" <> surfaceAlt <> ";"
        <> "border-" <> innerSide input.edge <> ":1px solid " <> line <> ";"
        <> (if input.disabled then "opacity:0.5;" else "")
    ]
    ( [ HH.button
          ( [ cls "hw-drawer-edge__arrow"
            , HP.type_ HP.ButtonButton
            , HP.disabled input.disabled
            , HPA.expanded (show input.open)
            , HPA.controls input.panelId
            , HPA.label (if input.open then input.hideLabel else input.showLabel)
            , HP.title (if input.open then input.hideLabel else input.showLabel)
            , HE.onClick \_ -> Toggle
            , sty $ "appearance:none;-webkit-appearance:none;border:0;margin:0;background:none;"
                <> "display:flex;flex-direction:column;align-items:center;gap:10px;"
                <> "width:100%;padding:6px 0;font:inherit;font-family:system-ui,sans-serif;"
                <> "color:" <> inkSoft <> ";"
                <> (if input.disabled then "cursor:default;" else "cursor:pointer;")
                <> (if input.open then "flex:none;" else "flex:1 1 auto;min-height:0;")
            ]
          )
          ( [ HH.span
                [ cls "hw-drawer-edge__glyph"
                , HPA.hidden "true"
                , sty $ "display:inline-block;font-size:12px;line-height:1;"
                    <> transition "transform" input.motion
                    <> "transform:rotate(" <> (if pointsRight input then "0deg" else "180deg") <> ")"
                ]
                [ HH.text "▸" ]
            ]
              <> case input.label of
                Just l | not input.open ->
                  [ HH.span
                      [ cls "hw-drawer-edge__label"
                      , HPA.hidden "true"
                      , sty $ "writing-mode:vertical-rl;transform:rotate(180deg);white-space:nowrap;"
                          <> "font-size:10px;font-weight:600;letter-spacing:0.08em;text-transform:uppercase;"
                          <> "color:" <> ink <> ";overflow:hidden"
                      ]
                      [ HH.text l ]
                  ]
                _ -> []
          )
      ]
        <> (if input.open then [ grip input ] else [])
    )

-- | The rest of the open rail. When resizable it is a focusable vertical
-- | separator that reports the width as its value; otherwise a blank strip.
grip :: forall m. Input -> H.ComponentHTML Action () m
grip input
  | input.resizable && not input.disabled =
      HH.div
        [ cls "hw-drawer-edge__grip"
        , HPA.role "separator"
        , HP.attr (H.AttrName "aria-orientation") "vertical"
        , HPA.valueNow (show (clampWidth input input.width))
        , HPA.valueMin (show input.minWidth)
        , HPA.valueMax (show (clampWidth input input.maxWidth))
        , HPA.controls input.panelId
        , HPA.label ("Resize: " <> String.toLower (if input.open then input.hideLabel else input.showLabel))
        , HP.tabIndex 0
        , HE.onMouseDown StartDrag
        , HE.onKeyDown GripKey
        , sty "flex:1 1 auto;min-height:0;cursor:col-resize;user-select:none"
        ]
        []
  | otherwise = HH.div [ cls "hw-drawer-edge__spacer", sty "flex:1 1 auto" ] []

-- The arrow points the way the drawer will move: a closed left drawer
-- opens rightward, an open one closes leftward, and the mirror on the right.
pointsRight :: Input -> Boolean
pointsRight input = (input.edge == Left) /= input.open

-- ─── The frame: the layout chrome ─────────────────────────────────────────

-- | Lay out the drawer: the parent's `body` in the panel, the `handle` (the
-- | parent's slot of `component`) on its inner edge, and `main` beside it
-- | (`Push`) or beneath it (`Overlay`). Fills its container's height: give the
-- | container one. Action-polymorphic, so every part keeps its interactivity.
frame :: forall w i. Input -> Parts w i -> HH.HTML w i
frame input parts =
  HH.div
    [ clss
        [ "hw-drawer"
        , edgeClass "hw-drawer" input.edge
        , if input.mode == Push then "hw-drawer--push" else "hw-drawer--overlay"
        , if input.open then "hw-drawer--open" else "hw-drawer--closed"
        ]
    , sty "position:relative;display:flex;width:100%;height:100%;min-height:0;overflow:hidden"
    ]
    (inOrder [ dock, mainArea ])
  where
  w = clampWidth input input.width
  inOrder xs = if input.edge == Left then xs else reverseArr xs
  reverseArr = case _ of
    [ a, b ] -> [ b, a ]
    xs -> xs

  dock =
    HH.div
      [ cls "hw-drawer__dock"
      , sty $ "display:flex;flex:none;height:100%;"
          <> case input.mode of
            Push -> ""
            Overlay ->
              "position:absolute;top:0;bottom:0;" <> outerSide input.edge <> ":0;z-index:2;"
                <> (if input.open then "box-shadow:" <> shadowToward input.edge <> ";" else "")
      ]
      (inOrder [ panel, parts.handle ])

  panel =
    HH.div
      ( [ cls "hw-drawer__panel"
        , HP.id input.panelId
        , HPA.role "region"
        , sty $ "flex:none;height:100%;overflow:hidden;box-sizing:border-box;"
            <> "background:" <> surface <> ";"
            <> transition "width" input.motion
            <> "width:" <> (if input.open then px w else "0px")
        ]
          <> (case input.label of
                Just l -> [ HPA.label l ]
                Nothing -> [])
          <> (if input.open then [] else [ HP.attr (H.AttrName "inert") "", HPA.hidden "true" ])
      )
      [ HH.div
          [ cls "hw-drawer__body"
          , sty $ "width:" <> px w <> ";height:100%;overflow:auto;box-sizing:border-box"
          ]
          parts.body
      ]

  mainArea =
    HH.div
      [ cls "hw-drawer__main"
      , sty $ "flex:1 1 auto;min-width:0;height:100%;overflow:auto;box-sizing:border-box;"
          <> case input.mode of
            Push -> ""
            Overlay -> "margin-" <> outerSide input.edge <> ":" <> px input.railWidth <> ";"
      ]
      parts.main

-- ─── Small helpers ────────────────────────────────────────────────────────

px :: Number -> String
px n = show n <> "px"

edgeClass :: String -> Edge -> String
edgeClass base = case _ of
  Left -> base <> "--left"
  Right -> base <> "--right"

-- | The side that faces the page edge, and the side that faces the content.
outerSide :: Edge -> String
outerSide = case _ of
  Left -> "left"
  Right -> "right"

innerSide :: Edge -> String
innerSide = case _ of
  Left -> "right"
  Right -> "left"

shadowToward :: Edge -> String
shadowToward = case _ of
  Left -> "6px 0 18px " <> shadow
  Right -> "-6px 0 18px " <> shadow
