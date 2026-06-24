-- | A controlled **before/after comparison wipe** — the draggable-divider
-- | widget used for before/after image pairs (and any two registered, spatially
-- | overlaid renderings). A vertical handle splits the box: the *before* layer
-- | shows to its left, the *after* layer to its right. Dragging the handle
-- | requests a new divider position; the parent owns it.
-- |
-- | Both layers are `HH.PlainHTML` — static content with no actions. That is
-- | the whole point of a comparison wipe (you compare two *renderings*, you
-- | don't interact through them), and it is what lets this be a proper leaf
-- | component on the contract: the widget owns the drag (document-level
-- | mousemove/mouseup, like `Knob`), while the caller's content arrives as
-- | inert HTML rather than through a children channel the type system lacks.
-- |
-- | Geometry is plain CSS: two absolutely-positioned layers, the *before* one
-- | clipped with `clip-path: inset(...)` so its content keeps full width rather
-- | than squashing. The handle position is read back from the container's
-- | bounding rect on each move (library FFI via `getBoundingClientRect`).
module Hylograph.Halogen.UI.Compare
  ( Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust, maybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.DOM.Element as Element
import Web.HTML (window)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

import Hylograph.Halogen.UI.Style as Style

-- | Controlled input. The parent owns `position` (0–100, the divider as a
-- | percentage from the left); the two layers and the chrome are config.
type Input =
  { position :: Number
  , before :: HH.PlainHTML       -- ^ shown left of the divider (e.g. the "before")
  , after :: HH.PlainHTML        -- ^ shown right of the divider (e.g. the "after")
  , height :: String             -- ^ CSS height of the box (e.g. "240px")
  , beforeLabel :: Maybe String  -- ^ corner caption on the before side
  , afterLabel :: Maybe String   -- ^ corner caption on the after side
  , disabled :: Boolean
  }

-- | A sensible starting `Input` from the two layers; override the rest.
defaultInput :: HH.PlainHTML -> HH.PlainHTML -> Input
defaultInput before after =
  { position: 50.0
  , before
  , after
  , height: "240px"
  , beforeLabel: Nothing
  , afterLabel: Nothing
  , disabled: false
  }

-- | A request, not a fact: the user dragged the divider to this position.
data Output = Moved Number

-- | Imperative escape hatch, for parity with the rest of the roster.
data Query a = SetPosition Number a

type Slot = H.Slot Query Output

data Action
  = Receive Input
  | StartDrag MouseEvent
  | DragMove Int
  | StopDrag

type State =
  { input :: Input
  , drag :: Maybe H.SubscriptionId
  }

containerRef :: H.RefLabel
containerRef = H.RefLabel "hg-compare-container"

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, drag: Nothing }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }

clampN :: Number -> Number -> Number -> Number
clampN lo hi v = if v < lo then lo else if v > hi then hi else v

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }

  StartDrag _ -> do
    st <- H.get
    when (not st.input.disabled && not (isJust st.drag)) do
      sid <- H.subscribe dragEmitter
      H.modify_ _ { drag = Just sid }

  DragMove clientX -> do
    st <- H.get
    when (isJust st.drag) do
      mEl <- H.getHTMLElementRef containerRef
      case mEl of
        Nothing -> pure unit
        Just el -> do
          rect <- liftEffect (Element.getBoundingClientRect (HTMLElement.toElement el))
          let pos = clampN 0.0 100.0 ((toNumber clientX - rect.left) / rect.width * 100.0)
          H.raise (Moved pos)

  StopDrag -> do
    st <- H.get
    case st.drag of
      Just sid -> do
        H.unsubscribe sid
        H.modify_ _ { drag = Nothing }
      Nothing -> pure unit

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  SetPosition v a -> do
    st <- H.get
    when (not st.input.disabled) (H.raise (Moved (clampN 0.0 100.0 v)))
    pure (Just a)

-- Document-level mousemove/mouseup subscription (mirrors Knob). Pure
-- event-target wiring; no FFI of our own.
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

render :: forall m. State -> H.ComponentHTML Action () m
render { input } =
  let
    pos = clampN 0.0 100.0 input.position
    s = show
    -- Clip the before layer to its left `pos`%: inset clips (100-pos)% off the
    -- right, so the content underneath keeps full width (no squash).
    beforeClip = "clip-path:inset(0 " <> s (100.0 - pos) <> "% 0 0)"
    layerBase = "position:absolute;top:0;left:0;width:100%;height:100%;overflow:hidden;"
  in
    HH.div
      [ Style.cls "hg-compare"
      , HP.ref containerRef
      , Style.sty $ "position:relative;width:100%;overflow:hidden;user-select:none;"
          <> "height:" <> input.height <> ";"
          <> "border:1px solid " <> Style.line <> ";border-radius:" <> "8px;"
          <> "font-family:" <> Style.uiFont <> ";"
          <> (if input.disabled then "opacity:0.5;" else "")
      ]
      [ -- After layer (base, full box).
        HH.div [ Style.sty layerBase ] [ HH.fromPlainHTML input.after ]
      , corner false input.afterLabel
        -- Before layer (clipped to the left of the divider).
      , HH.div [ Style.sty $ layerBase <> beforeClip ] [ HH.fromPlainHTML input.before ]
      , corner true input.beforeLabel
        -- The divider handle.
      , HH.div
          [ Style.sty $ "position:absolute;top:0;bottom:0;left:" <> s pos <> "%;"
              <> "transform:translateX(-50%);width:32px;display:flex;align-items:center;"
              <> "justify-content:center;"
              <> (if input.disabled then "cursor:default" else "cursor:ew-resize")
          , HE.onMouseDown StartDrag
          ]
          [ -- The vertical rule.
            HH.div
              [ Style.sty $ "position:absolute;top:0;bottom:0;left:50%;width:2px;"
                  <> "transform:translateX(-50%);background:" <> Style.accent ]
              []
            -- The round grip with arrows.
          , HH.div
              [ Style.sty $ "position:relative;width:28px;height:28px;border-radius:50%;"
                  <> "background:" <> Style.surface <> ";border:2px solid " <> Style.accent <> ";"
                  <> "display:flex;align-items:center;justify-content:center;"
                  <> "font-size:11px;color:" <> Style.accent <> ";box-shadow:0 1px 3px " <> Style.shadow ]
              [ HH.text "◂▸" ]
          ]
      ]

-- | A corner caption. `left` places it bottom-left (before) or bottom-right
-- | (after). Renders nothing when there is no label.
corner :: forall m. Boolean -> Maybe String -> H.ComponentHTML Action () m
corner left = maybe (HH.text "") \label ->
  HH.div
    [ Style.sty $ "position:absolute;bottom:8px;"
        <> (if left then "left:8px;" else "right:8px;")
        <> "padding:2px 7px;border-radius:3px;font-size:10px;font-weight:600;"
        <> "letter-spacing:0.06em;text-transform:uppercase;"
        <> "background:" <> Style.backdrop <> ";color:#fff" ]
    [ HH.text label ]
