-- | A **horizontal** disclosure header — panels sit side-by-side as columns.
-- | A collapsed panel shrinks to a thin vertical strip with a rotated label
-- | (the Triggerfish layout: several columns, all but one folded to a spine).
-- | Same controlled contract as `VAccordion`; only the *collapsed* rendering
-- | differs. Reach for `VAccordion` for the ordinary stacked case.
-- |
-- | The PARENT owns `open` and renders the panel BODY itself. In a horizontal
-- | layout the body is the column's content, shown only while that column is
-- | open.
module Hylograph.Halogen.UI.HAccordion
  ( module Export
  , component
  ) where

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Hylograph.Halogen.UI.Accordion.Internal (Input, Output(..), Query(..), Slot, defaultInput, body) as Export
import Hylograph.Halogen.UI.Accordion.Internal (Orientation(Horizontal), mkComponent)

component :: forall m. MonadAff m => H.Component Export.Query Export.Input Export.Output m
component = mkComponent Horizontal
