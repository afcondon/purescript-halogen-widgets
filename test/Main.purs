-- | A type-level smoke test: every widget's public surface is referenced with
-- | its full exported types, so a signature or export regression fails the
-- | build. Compilation *is* the test — `main` does no runtime work, and the
-- | target pulls in no extra dependencies.
module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

import Hylograph.Halogen.UI.Accordion as Accordion
import Hylograph.Halogen.UI.Toggle as Toggle
import Hylograph.Halogen.UI.Stepper as Stepper
import Hylograph.Halogen.UI.Slider as Slider
import Hylograph.Halogen.UI.SegmentedControl as Segmented
import Hylograph.Halogen.UI.Select as Select
import Hylograph.Halogen.UI.Modal as Modal
import Hylograph.Halogen.UI.Panel as Panel
import Hylograph.Halogen.UI.Field as Field
import Hylograph.Halogen.UI.Toast as Toast

seen :: forall a. a -> Boolean
seen _ = true

-- The whole conformance check, as one Boolean. Its body type-checks every
-- public export at its full exported type; that is the assertion.
checks :: Boolean
checks =
  -- Leaf components, pinned to their exported types at a concrete monad.
  seen (Accordion.component :: H.Component Accordion.Query Accordion.Input Accordion.Output Aff)
    && seen (Toggle.component :: H.Component Toggle.Query Toggle.Input Toggle.Output Aff)
    && seen (Stepper.component :: H.Component Stepper.Query Stepper.Input Stepper.Output Aff)
    && seen (Slider.component :: H.Component Slider.Query Slider.Input Slider.Output Aff)
    && seen (Segmented.component :: H.Component Segmented.Query Segmented.Input Segmented.Output Aff)
    && seen (Select.component :: H.Component Select.Query Select.Input Select.Output Aff)
    -- Chrome functions, applied to concrete args.
    && seen (Modal.modal { open: false, title: "t", onClose: unit } [] :: HH.HTML Unit Unit)
    && seen (Panel.panel { title: "t", sub: Nothing } [] :: HH.HTML Unit Unit)
    && seen (Field.field { label: "l", hint: Nothing } (HH.text "x") :: HH.HTML Unit Unit)
    && seen (Toast.toast { variant: Toast.Info, message: "m", onDismiss: Nothing } :: HH.HTML Unit Unit)
    -- defaultInput on-ramps.
    && (Toggle.defaultInput false).value == false
    && (Stepper.defaultInput 5).value == 5
    && (Slider.defaultInput 0.0).min == 0.0
    && (Select.defaultInput []).searchable == false
    && (Accordion.defaultInput "GENERATE").open == true

main :: Effect Unit
main = case checks of _ -> pure unit
