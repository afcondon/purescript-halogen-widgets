-- | A controlled rotary knob — `Number`-valued, vertical-drag gesture, 300°
-- | sweep (7 o'clock → 5 o'clock). The fully-controlled instance of the contract
-- | for a continuous control whose value lives in app state.
-- |
-- | The drag is debounced inside the widget (a drag fires a flood of mousemove
-- | events) — same machinery as `Slider`. Mousedown captures the start; document-
-- | level mousemove/mouseup subscriptions track the rest, so the gesture survives
-- | the pointer leaving the SVG. `140 px = full range` (the conventional knob
-- | feel, matching producing-with-your-feet's Donut and Triggerfish's port).
-- |
-- | Geometry ported from Triggerfish's `Triggerfish.Ui.Knob`, which itself was
-- | a port from producing-with-your-feet's `Component.Pedal.Donut`. The arc,
-- | pointer indicator, optional detent ticks, and face are SVG via Halogen's
-- | `elementNS`; no external SVG library involved.
module Halogen.Widgets.Knob
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  ) where

import Prelude

import Data.Array (range)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

import Halogen.Widgets.Style as Style
import Halogen.Widgets.Svg (svgAttr, svgEl, svgOnDown)
import Halogen.Widgets.Knob.Geometry (arcPath, maxAngle, minAngle, pointerAt, tickAngle, valToAngle)

type Input =
  { value :: Number
  , min :: Number
  , max :: Number
  , size :: Number          -- ^ rendered diameter in px; the SVG viewBox stays 0 0 48 48
  , color :: String         -- ^ filled-arc colour (defaults to theme accent)
  , label :: Maybe String
  , ticks :: Int            -- ^ 0 = continuous; N > 1 draws N detent marks
  , debounce :: Milliseconds
  , disabled :: Boolean
  }

-- A knob's value display is the parent's read-back — and *that's the value the
-- user is watching while they turn*. So unlike Slider/Toggle, the default
-- debounce is OFF; every drag tick gets emitted so the readout tracks the hand.
-- Raise it deliberately for cases where downstream work per-tick is expensive
-- (a MIDI write, an audio param ramp); the generation-counter machinery is
-- still here for you when you want it.
defaultInput :: Number -> Input
defaultInput v =
  { value: v
  , min: 0.0
  , max: 100.0
  , size: 56.0
  , color: Style.accent
  , label: Nothing
  , ticks: 0
  , debounce: Milliseconds 0.0
  , disabled: false
  }

data Output = Changed Number

data Query a = Set Number a

type Slot = H.Slot Query Output

data Action
  = Receive Input
  | StartDrag MouseEvent
  | DragMove Int
  | StopDrag

type DragInfo =
  { startY :: Int
  , startValue :: Number
  , subId :: H.SubscriptionId
  }

type State =
  { input :: Input
  , drag :: Maybe DragInfo
  , version :: Int
  }

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, drag: Nothing, version: 0 }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }

clampN :: Number -> Number -> Number -> Number
clampN lo hi v = if v < lo then lo else if v > hi then hi else v

handleAction
  :: forall m
   . MonadAff m
  => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }

  StartDrag me -> do
    st <- H.get
    when (not st.input.disabled) do
      sid <- H.subscribe dragEmitter
      H.modify_ _
        { drag = Just
            { startY: ME.clientY me
            , startValue: st.input.value
            , subId: sid
            }
        }

  DragMove clientY -> do
    st <- H.get
    case st.drag of
      Nothing -> pure unit
      Just d -> do
        let span_ = st.input.max - st.input.min
            -- 140 px of vertical travel = full range; up = increase.
            deltaPx = toNumber (d.startY - clientY)
            raw = d.startValue + deltaPx * span_ / 140.0
            next = clampN st.input.min st.input.max raw
        emitChange st next

  StopDrag -> do
    st <- H.get
    case st.drag of
      Just d -> do
        H.unsubscribe d.subId
        H.modify_ _ { drag = Nothing }
      Nothing -> pure unit

handleQuery
  :: forall m a
   . MonadAff m
  => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Set v a -> do
    { input } <- H.get
    when (not input.disabled) (H.raise (Changed (clampN input.min input.max v)))
    pure (Just a)

-- Emit a `Changed` honoring `debounce`, using the generation-counter idiom from
-- CONTRACT.md (only the latest fork raises).
emitChange
  :: forall m
   . MonadAff m
  => State -> Number -> H.HalogenM State Action () Output m Unit
emitChange st v =
  case st.input.debounce of
    Milliseconds ms
      | ms <= 0.0 -> H.raise (Changed v)
      | otherwise -> do
          next <- H.modify \s -> s { version = s.version + 1 }
          let mine = next.version
          void $ H.fork do
            liftAff (delay (Milliseconds ms))
            s' <- H.get
            when (s'.version == mine) (H.raise (Changed v))

-- Document-level mousemove/mouseup subscription, set up on mousedown and torn
-- down on mouseup. Pure event-target wiring; no FFI of our own.
dragEmitter :: HS.Emitter Action
dragEmitter = HS.makeEmitter \emit -> do
  moveFn <- eventListener \e -> case ME.fromEvent e of
    Just me -> emit (DragMove (ME.clientY me))
    Nothing -> pure unit
  upFn <- eventListener \_ -> emit StopDrag
  target <- Window.toEventTarget <$> window
  addEventListener (EventType "mousemove") moveFn false target
  addEventListener (EventType "mouseup") upFn false target
  pure do
    removeEventListener (EventType "mousemove") moveFn false target
    removeEventListener (EventType "mouseup") upFn false target

--------------------------------------------------------------------------------
-- SVG rendering
--------------------------------------------------------------------------------

-- `svgEl` / `svgAttr` / `svgOnDown` now live in `Halogen.Widgets.Svg` (imported
-- above), since every hand-rolled SVG widget needs the same three.

render :: forall m. State -> H.ComponentHTML Action () m
render { input } =
  let
    -- Internal SVG units stay 0–48 regardless of rendered `size`.
    cx = 24.0
    cy = 24.0
    rOuter = 20.0
    rInner = 14.0
    angle = valToAngle input.min input.max input.value
    ptr = pointerAt cx cy (rOuter - 4.0) angle
    s = show
  in
    HH.div
      [ Style.cls "hg-knob"
      , Style.sty $ "display:inline-flex;flex-direction:column;align-items:center;gap:6px;"
          <> "user-select:none;font-family:" <> Style.uiFont <> ";"
          <> (if input.disabled then "opacity:0.5;" else "")
      ]
      ( [ HH.div
            [ Style.sty $ "width:" <> s input.size <> "px;height:" <> s input.size <> "px" ]
            [ svgEl "svg"
                [ svgAttr "viewBox" "0 0 48 48"
                , svgAttr "width" "100%"
                , svgAttr "height" "100%"
                , svgAttr "style" "display:block;overflow:visible"
                ]
                ( [ -- Unfilled track (full sweep).
                    svgEl "path"
                      [ svgAttr "d" (arcPath cx cy rOuter rInner minAngle maxAngle)
                      , svgAttr "fill" Style.trackOff
                      ]
                      []
                  , -- Filled arc (min → value).
                    svgEl "path"
                      [ svgAttr "d" (arcPath cx cy rOuter rInner minAngle angle)
                      , svgAttr "fill" input.color
                      ]
                      []
                  ]
                  <> detents input.ticks cx cy rOuter
                  <>
                  [ -- Inner disc (the "face").
                    svgEl "circle"
                      [ svgAttr "cx" (s cx)
                      , svgAttr "cy" (s cy)
                      , svgAttr "r" (s (rInner - 1.0))
                      , svgAttr "fill" Style.surfaceAlt
                      ]
                      []
                  , -- Pointer indicator (from centre to the value angle).
                    svgEl "line"
                      [ svgAttr "x1" (s cx)
                      , svgAttr "y1" (s cy)
                      , svgAttr "x2" (s ptr.x)
                      , svgAttr "y2" (s ptr.y)
                      , svgAttr "stroke" Style.ink
                      , svgAttr "stroke-width" "1.6"
                      , svgAttr "stroke-linecap" "round"
                      ]
                      []
                  , -- Transparent capture rect — what mousedown actually hits.
                    svgEl "circle"
                      [ svgAttr "cx" (s cx)
                      , svgAttr "cy" (s cy)
                      , svgAttr "r" (s rOuter)
                      , svgAttr "fill" "transparent"
                      , svgAttr "style"
                          (if input.disabled then "cursor:default" else "cursor:ns-resize")
                      , svgOnDown StartDrag
                      ]
                      []
                  ]
                )
            ]
        ]
        <> maybe []
             (\l ->
               [ HH.span
                   [ Style.sty $ "font-size:11px;color:" <> Style.inkSoft <> ";letter-spacing:0.04em" ]
                   [ HH.text l ]
               ])
             input.label
      )

detents :: forall w i. Int -> Number -> Number -> Number -> Array (HH.HTML w i)
detents n cx cy rOuter
  | n <= 1 = []
  | otherwise =
      let
        s = show
        tick i =
          let
            ang = tickAngle n i
            a = pointerAt cx cy (rOuter + 1.2) ang
            b = pointerAt cx cy (rOuter + 3.4) ang
          in
            svgEl "line"
              [ svgAttr "x1" (s a.x)
              , svgAttr "y1" (s a.y)
              , svgAttr "x2" (s b.x)
              , svgAttr "y2" (s b.y)
              , svgAttr "stroke" Style.inkSoft
              , svgAttr "stroke-width" "0.8"
              ]
              []
      in
        map tick (range 0 (n - 1))
