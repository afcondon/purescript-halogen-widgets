-- | A concentric two-layer knob — the Strymon / Chase Bliss pattern where one
-- | physical control hosts two parameters: an outer ring (primary, more visible)
-- | and an inner ring (secondary, more subtle). Each layer has its own value,
-- | its own range, and its own drag — both report through a single `Output` ADT
-- | tagged by which layer changed.
-- |
-- | This is "DoubleKnob" in the producing-with-your-feet sense: concentric
-- | layers, not a 2D-drag (one axis per parameter). A 2D-drag variant could be a
-- | future `DoubleKnob2D`.
-- |
-- | Geometry, SVG primitives, and drag-tracking machinery share the same
-- | conventions as `Knob` (300° sweep, 140 px = full range, document-level
-- | mousemove/mouseup subscription on mousedown).
module Halogen.Widgets.DoubleKnob
  ( Layer
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Number (cos, sin, pi)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

import Halogen.Widgets.Style as Style

-- | Per-layer config + value. Each layer is independently controlled.
type Layer =
  { value :: Number
  , min :: Number
  , max :: Number
  , color :: String
  }

type Input =
  { outer :: Layer
  , inner :: Layer
  , size :: Number              -- ^ rendered diameter in px
  , label :: Maybe String
  , debounce :: Milliseconds
  , disabled :: Boolean
  }

defaultLayer :: Number -> Layer
defaultLayer v = { value: v, min: 0.0, max: 100.0, color: Style.accent }

-- Default debounce OFF — see Knob for rationale: the parent's read-back IS
-- what the user is watching while they turn.
defaultInput :: Number -> Number -> Input
defaultInput o i =
  { outer: defaultLayer o
  , inner: (defaultLayer i) { color = Style.inkSoft }
  , size: 72.0
  , label: Nothing
  , debounce: Milliseconds 0.0
  , disabled: false
  }

-- | Which physical layer was touched (decides where the value goes back).
data Which = Outer | Inner
derive instance eqWhich :: Eq Which

-- | A change request, tagged by layer. The parent fans it back to the
-- | appropriate `outer.value` / `inner.value` in `Input`.
data Output
  = OuterChanged Number
  | InnerChanged Number

data Query a
  = SetOuter Number a
  | SetInner Number a

type Slot = H.Slot Query Output

data Action
  = Receive Input
  | StartDrag Which MouseEvent
  | DragMove Int
  | StopDrag

type DragInfo =
  { which :: Which
  , startY :: Int
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

layerOf :: Input -> Which -> Layer
layerOf input = case _ of
  Outer -> input.outer
  Inner -> input.inner

mkOutput :: Which -> Number -> Output
mkOutput = case _ of
  Outer -> OuterChanged
  Inner -> InnerChanged

handleAction
  :: forall m
   . MonadAff m
  => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }

  StartDrag which me -> do
    st <- H.get
    when (not st.input.disabled) do
      sid <- H.subscribe dragEmitter
      H.modify_ _
        { drag = Just
            { which
            , startY: ME.clientY me
            , startValue: (layerOf st.input which).value
            , subId: sid
            }
        }

  DragMove clientY -> do
    st <- H.get
    case st.drag of
      Nothing -> pure unit
      Just d -> do
        let l = layerOf st.input d.which
            span_ = l.max - l.min
            deltaPx = toNumber (d.startY - clientY)
            raw = d.startValue + deltaPx * span_ / 140.0
            next = clampN l.min l.max raw
        emitChange st d.which next

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
  SetOuter v a -> do
    { input } <- H.get
    when (not input.disabled) $
      H.raise (OuterChanged (clampN input.outer.min input.outer.max v))
    pure (Just a)
  SetInner v a -> do
    { input } <- H.get
    when (not input.disabled) $
      H.raise (InnerChanged (clampN input.inner.min input.inner.max v))
    pure (Just a)

emitChange
  :: forall m
   . MonadAff m
  => State -> Which -> Number -> H.HalogenM State Action () Output m Unit
emitChange st which v =
  case st.input.debounce of
    Milliseconds ms
      | ms <= 0.0 -> H.raise (mkOutput which v)
      | otherwise -> do
          next <- H.modify \s -> s { version = s.version + 1 }
          let mine = next.version
          void $ H.fork do
            liftAff (delay (Milliseconds ms))
            s' <- H.get
            when (s'.version == mine) (H.raise (mkOutput which v))

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
-- SVG rendering — same conventions as Knob.
--------------------------------------------------------------------------------

svgEl :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr n v = HP.attr (HH.AttrName n) v

svgOnDown :: forall r i. (MouseEvent -> i) -> HH.IProp r i
svgOnDown f = HE.handler (EventType "mousedown") (unsafeCoerce f)

minAngle :: Number
minAngle = -5.0 * pi / 6.0

maxAngle :: Number
maxAngle = 5.0 * pi / 6.0

sweep :: Number
sweep = maxAngle - minAngle

valToAngle :: Number -> Number -> Number -> Number
valToAngle lo hi v =
  let frac = if hi == lo then 0.0 else (v - lo) / (hi - lo)
  in minAngle + frac * sweep

arcPath :: Number -> Number -> Number -> Number -> Number -> Number -> String
arcPath cx cy outerR innerR a0 a1 =
  let
    toSvg a = a - pi / 2.0
    sa = toSvg a0
    ea = toSvg a1
    ox0 = cx + outerR * cos sa
    oy0 = cy + outerR * sin sa
    ox1 = cx + outerR * cos ea
    oy1 = cy + outerR * sin ea
    ix0 = cx + innerR * cos sa
    iy0 = cy + innerR * sin sa
    ix1 = cx + innerR * cos ea
    iy1 = cy + innerR * sin ea
    largeArc = if (a1 - a0) > pi then "1" else "0"
    s = show
  in
    "M" <> s ox0 <> "," <> s oy0
      <> " A" <> s outerR <> "," <> s outerR <> " 0 " <> largeArc <> ",1 " <> s ox1 <> "," <> s oy1
      <> " L" <> s ix1 <> "," <> s iy1
      <> " A" <> s innerR <> "," <> s innerR <> " 0 " <> largeArc <> ",0 " <> s ix0 <> "," <> s iy0
      <> " Z"

-- | Centre is shared (24, 24); outer ring uses radii rOuterHi..rOuterLo, inner
-- | ring uses rInnerHi..rInnerLo. Inner-ring radii are smaller so it sits
-- | inside the outer.
render :: forall m. State -> H.ComponentHTML Action () m
render { input } =
  let
    cx = 24.0
    cy = 24.0
    rOuterHi = 22.0
    rOuterLo = 16.0
    rInnerHi = 13.5
    rInnerLo = 7.5
    aO = valToAngle input.outer.min input.outer.max input.outer.value
    aI = valToAngle input.inner.min input.inner.max input.inner.value
    -- Pointer tips for both layers (small radial ticks at the value angle).
    tipO = (rOuterHi + rOuterLo) / 2.0
    tipI = (rInnerHi + rInnerLo) / 2.0
    poX = cx + tipO * cos (aO - pi / 2.0)
    poY = cy + tipO * sin (aO - pi / 2.0)
    piX = cx + tipI * cos (aI - pi / 2.0)
    piY = cy + tipI * sin (aI - pi / 2.0)
    s = show
  in
    HH.div
      [ Style.cls "hg-double-knob"
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
                [ -- Outer ring track + filled arc.
                  svgEl "path"
                    [ svgAttr "d" (arcPath cx cy rOuterHi rOuterLo minAngle maxAngle)
                    , svgAttr "fill" Style.trackOff
                    ] []
                , svgEl "path"
                    [ svgAttr "d" (arcPath cx cy rOuterHi rOuterLo minAngle aO)
                    , svgAttr "fill" input.outer.color
                    , svgAttr "fill-opacity" "0.85"
                    ] []
                -- Inner ring track + filled arc.
                , svgEl "path"
                    [ svgAttr "d" (arcPath cx cy rInnerHi rInnerLo minAngle maxAngle)
                    , svgAttr "fill" Style.trackOff
                    ] []
                , svgEl "path"
                    [ svgAttr "d" (arcPath cx cy rInnerHi rInnerLo minAngle aI)
                    , svgAttr "fill" input.inner.color
                    , svgAttr "fill-opacity" "0.55"
                    ] []
                -- Centre disc (face) — under the pointers, above the inner arc.
                , svgEl "circle"
                    [ svgAttr "cx" (s cx), svgAttr "cy" (s cy)
                    , svgAttr "r" (s (rInnerLo - 1.0))
                    , svgAttr "fill" Style.surfaceAlt
                    ] []
                -- Pointers (short radial lines at each value angle).
                , svgEl "circle"
                    [ svgAttr "cx" (s poX), svgAttr "cy" (s poY)
                    , svgAttr "r" "1.4"
                    , svgAttr "fill" Style.ink
                    ] []
                , svgEl "circle"
                    [ svgAttr "cx" (s piX), svgAttr "cy" (s piY)
                    , svgAttr "r" "1.2"
                    , svgAttr "fill" Style.ink
                    ] []
                -- Capture rings (mousedown targets, layered: inner on top so
                -- clicks on the inner ring don't go to the outer).
                , svgEl "path"
                    [ svgAttr "d" (arcPath cx cy rOuterHi rOuterLo minAngle maxAngle)
                    , svgAttr "fill" "transparent"
                    , svgAttr "style"
                        (if input.disabled then "cursor:default" else "cursor:ns-resize")
                    , svgOnDown (StartDrag Outer)
                    ] []
                , svgEl "path"
                    [ svgAttr "d" (arcPath cx cy rInnerHi rInnerLo minAngle maxAngle)
                    , svgAttr "fill" "transparent"
                    , svgAttr "style"
                        (if input.disabled then "cursor:default" else "cursor:ns-resize")
                    , svgOnDown (StartDrag Inner)
                    ] []
                ]
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
