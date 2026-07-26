-- | A **vertical** disclosure header — the reference instance of the library
-- | contract (see CONTRACT.md). Panels stack top-to-bottom; the header is
-- | always a full-width horizontal bar, and collapsing hides the parent's body
-- | and flips the chevron (▾ → ▸). This is the common web accordion, and the
-- | one to reach for unless you are laying panels out as side-by-side columns
-- | (then use `HAccordion`).
-- |
-- | The PARENT owns `open` and renders the panel BODY itself
-- | (`if open then [body] else []`). A multi-panel accordion is N of these
-- | sharing a parent-owned open-set (exactly Triggerfish's
-- | `collapsed :: Array String`).
module Halogen.Widgets.VAccordion
  ( module Export
  , component
  ) where

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Widgets.Accordion.Internal (Input, Output(..), Query(..), Slot, defaultInput, body) as Export
import Halogen.Widgets.Accordion.Internal (Orientation(Vertical), mkComponent)

component :: forall m. MonadAff m => H.Component Export.Query Export.Input Export.Output m
component = mkComponent Vertical
