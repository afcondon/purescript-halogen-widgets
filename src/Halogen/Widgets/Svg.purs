-- | The three lines every hand-rolled SVG widget needs, and nothing else.
-- |
-- | Halogen has no SVG vocabulary of its own: an SVG element must be built with
-- | `elementNS` under the SVG namespace, and its attributes must be raw string
-- | attributes rather than typed HTML properties (`HP.width` and friends emit
-- | *properties*, which SVG elements ignore). So every module that draws
-- | something ends up redefining the same `svgEl` and `svgAttr`. This is that
-- | pair, defined once.
-- |
-- | It is deliberately **not** a typed SVG binding — `halogen-svg-elems` exists
-- | for that, and is the better answer when a module draws a lot. These three
-- | are for the common case of a widget that draws a handful of shapes and does
-- | not want a dependency for it.
-- |
-- | ### Why the coercion in `svgOn`
-- |
-- | `svgEl`'s property row is closed (`IProp ()`), so a typed event handler like
-- | `HE.onMouseDown` — whose row demands `onMouseDown :: MouseEvent` — will not
-- | fit it. `HE.handler` takes a raw `Event -> i` and an open row, so the only
-- | thing standing in the way is that our callback wants the narrower
-- | `MouseEvent`. That narrowing is *sound at runtime*: a listener registered
-- | for `"mousedown"` is only ever handed a `MouseEvent`. The coercion asserts
-- | exactly that and nothing more — which is why the event name and the callback
-- | type must be chosen together, and why `svgOnDown` is worth having as the
-- | pre-paired case.
module Halogen.Widgets.Svg
  ( svgNS
  , svgEl
  , svgAttr
  , svgOn
  , svgOnDown
  ) where

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.UIEvent.MouseEvent (MouseEvent)

-- | The SVG namespace URI. Exported because a module occasionally needs to build
-- | a namespaced element `svgEl` does not cover (a `foreignObject`, say).
svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

-- | An SVG element by tag name. The property row is closed, so its attributes
-- | must come from `svgAttr` / `svgOn` rather than from `Halogen.HTML.Properties`.
svgEl :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS (HH.Namespace svgNS) (HH.ElemName name)

-- | A raw string attribute — `svgAttr "cx" "12"`. SVG geometry is attributes,
-- | not properties, so this is the only way to set it.
svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr n v = HP.attr (HH.AttrName n) v

-- | A mouse handler that fits `svgEl`'s closed row. The event name and the
-- | handler's argument type must agree: name a non-mouse event here and the
-- | callback will be handed something that is not a `MouseEvent`.
svgOn :: forall r i. String -> (MouseEvent -> i) -> HH.IProp r i
svgOn name f = HE.handler (EventType name) (unsafeCoerce f)

-- | `svgOn "mousedown"`, pre-paired so the one case that is always right needs
-- | no thought.
svgOnDown :: forall r i. (MouseEvent -> i) -> HH.IProp r i
svgOnDown = svgOn "mousedown"
