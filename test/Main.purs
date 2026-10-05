-- | A type-level smoke test: every widget's public surface is referenced with
-- | its full exported types, so a signature or export regression fails the
-- | build. Compilation *is* the test for the surface.
-- |
-- | Where a widget's decisions are pure functions (the Drawer's toggle request
-- | and width clamping), `behaviour` also checks them by value, and `main`
-- | throws if any fails, so `spago test` exits non-zero.
module Test.Main where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

import Halogen.Widgets.VAccordion as VAccordion
import Halogen.Widgets.HAccordion as HAccordion
import Halogen.Widgets.Toggle as Toggle
import Halogen.Widgets.Stepper as Stepper
import Halogen.Widgets.Slider as Slider
import Halogen.Widgets.Knob as Knob
import Halogen.Widgets.DoubleKnob as DoubleKnob
import Halogen.Widgets.SegmentedControl as Segmented
import Halogen.Widgets.Select as Select
import Halogen.Widgets.Compare as Compare
import Halogen.Widgets.Modal as Modal
import Halogen.Widgets.Panel as Panel
import Halogen.Widgets.Field as Field
import Halogen.Widgets.Toast as Toast
import Halogen.Widgets.Motion (Motion(..), defaultMotion)
import Halogen.Widgets.Quiet as Quiet
import Halogen.Widgets.Ledger as Ledger
import Halogen.Widgets.Drawer as Drawer

seen :: forall a. a -> Boolean
seen _ = true

-- The whole conformance check, as one Boolean. Its body type-checks every
-- public export at its full exported type; that is the assertion.
checks :: Boolean
checks =
  -- Leaf components, pinned to their exported types at a concrete monad.
  seen (VAccordion.component :: H.Component VAccordion.Query VAccordion.Input VAccordion.Output Aff)
    && seen (HAccordion.component :: H.Component HAccordion.Query HAccordion.Input HAccordion.Output Aff)
    && seen (Toggle.component :: H.Component Toggle.Query Toggle.Input Toggle.Output Aff)
    && seen (Stepper.component :: H.Component Stepper.Query Stepper.Input Stepper.Output Aff)
    && seen (Slider.component :: H.Component Slider.Query Slider.Input Slider.Output Aff)
    && seen (Knob.component :: H.Component Knob.Query Knob.Input Knob.Output Aff)
    && seen (DoubleKnob.component :: H.Component DoubleKnob.Query DoubleKnob.Input DoubleKnob.Output Aff)
    && seen (Segmented.component :: H.Component Segmented.Query Segmented.Input Segmented.Output Aff)
    && seen (Select.component :: H.Component Select.Query Select.Input Select.Output Aff)
    && seen (Compare.component :: H.Component Compare.Query Compare.Input Compare.Output Aff)
    && seen (Drawer.component :: H.Component Drawer.Query Drawer.Input Drawer.Output Aff)
    -- Chrome functions, applied to concrete args.
    && seen (Modal.modal { open: false, title: "t", onClose: unit } [] :: HH.HTML Unit Unit)
    && seen (Panel.panel { title: "t", sub: Nothing } [] :: HH.HTML Unit Unit)
    && seen (Field.field { label: "l", hint: Nothing } (HH.text "x") :: HH.HTML Unit Unit)
    && seen (Toast.toast { variant: Toast.Info, message: "m", onDismiss: Nothing } :: HH.HTML Unit Unit)
    -- Accordion body reveal — a chrome function, on each orientation's re-export.
    && seen (VAccordion.body { open: true, motion: NoMotion } [] :: HH.HTML Unit Unit)
    && seen (HAccordion.body { open: false, motion: defaultMotion } [] :: HH.HTML Unit Unit)
    -- Quiet controls and the ledger: view functions, polymorphic in the action.
    && seen (Quiet.number quietInput :: HH.HTML Unit Unit)
    && seen (Quiet.text quietInput { align = Quiet.Start } :: HH.HTML Unit Unit)
    && seen (Quiet.select { value: "a", options: [ "a", "b" ], label: "l", onChange: const unit } :: HH.HTML Unit Unit)
    && seen (Quiet.choose { prompt: "+ add", groups: [ { name: "g", options: [ { value: "v", label: "V" } ] } ], label: "l", onChoose: const unit } :: HH.HTML Unit Unit)
    && seen (Quiet.toggle { on: true, text: "on", label: "l", onToggle: unit } :: HH.HTML Unit Unit)
    && seen (Quiet.mark { name: "MIDI", detail: "", hue: "var(--x)", on: true, onToggle: unit } :: HH.HTML Unit Unit)
    && seen (Quiet.labelled "slots" [ Quiet.note "C2" ] :: HH.HTML Unit Unit)
    && seen (Quiet.fault "no port" :: HH.HTML Unit Unit)
    && seen (Quiet.tool { glyph: "✕", label: "remove", onClick: unit } :: HH.HTML Unit Unit)
    && seen (Quiet.key [ { name: "MIDI", hue: "red", count: 2 } ] :: HH.HTML Unit Unit)
    && seen (Ledger.ledger { columns: [ { head: "Name", track: "8em", align: Quiet.Start } ], minWidth: "400px" }
               [ Ledger.Heading (HH.text "g")
               , Ledger.Entry { first: true, off: false, cells: [ Ledger.name { name: "n", sub: "" } ] }
               , Ledger.Add { from: 0, content: Ledger.values [] }
               ] :: HH.HTML Unit Unit)
    -- The drawer's layout chrome, polymorphic in the caller's action.
    && seen (Drawer.frame drawer { handle: HH.text "edge", body: [], main: [] } :: HH.HTML Unit Unit)
    && seen (Drawer.clampWidth :: Drawer.Input -> Number -> Number)
    && seen (Drawer.dragWidth :: Drawer.Input -> Number -> Number -> Number)
    && seen (Drawer.toggled :: Drawer.Input -> Maybe Drawer.Output)
    -- defaultInput on-ramps.
    && (Toggle.defaultInput false).value == false
    && (Stepper.defaultInput 5).value == 5
    && (Slider.defaultInput 0.0).min == 0.0
    && (Knob.defaultInput 0.0).ticks == 0
    && (DoubleKnob.defaultInput 0.0 0.0).outer.value == 0.0
    && (Select.defaultInput []).searchable == false
    -- grouped Select on-ramp: the additive `groups` field + smart constructor.
    && seen ((Select.groupedInput [ { label: "g", options: [] } ]).groups :: Array Select.OptionGroup)
    -- cascade Select on-ramp: same data, fly-out presentation.
    && (Select.cascadingInput [ { label: "g", options: [] } ]).cascade == true
    && (Compare.defaultInput (HH.text "a") (HH.text "b")).position == 50.0
    && (VAccordion.defaultInput "GENERATE").open == true
    && (HAccordion.defaultInput "GENERATE").open == true
    && (Drawer.defaultInput "Browser").edge == Drawer.Left
    && (Drawer.defaultInput "Browser").mode == Drawer.Push

-- | The drawer used by the checks: resizable, 160..480, 260 wide.
drawer :: Drawer.Input
drawer = (Drawer.defaultInput "Browser") { resizable = true }

-- | Behaviour checks, by value. Each entry is a name and whether it held.
behaviour :: Array { name :: String, ok :: Boolean }
behaviour =
  -- The open/close output: a press asks for the opposite of `open`.
  [ { name: "toggle from open asks to close"
    , ok: Drawer.toggled drawer { open = true } == Just (Drawer.Toggled false) }
  , { name: "toggle from closed asks to open"
    , ok: Drawer.toggled drawer { open = false } == Just (Drawer.Toggled true) }
  , { name: "disabled drawer asks for nothing"
    , ok: Drawer.toggled drawer { disabled = true } == Nothing }
  -- The controlled contract: the request is computed from the parent's value
  -- every time. A parent that refuses the request (keeps open = true) gets the
  -- same request again; one that honours it gets the reverse next time.
  , { name: "a refused request is asked again"
    , ok: let refused = drawer { open = true }
          in Drawer.toggled refused == Drawer.toggled refused }
  , { name: "an honoured request reverses"
    , ok: case Drawer.toggled drawer { open = true } of
        Just (Drawer.Toggled o) -> Drawer.toggled drawer { open = o } == Just (Drawer.Toggled true)
        _ -> false }
  , { name: "the request does not touch the input"
    , ok: (drawer { open = true }).open == true }
  -- Width clamping.
  , { name: "width inside the bounds is kept"
    , ok: Drawer.clampWidth drawer 300.0 == 300.0 }
  , { name: "width below min clamps to min"
    , ok: Drawer.clampWidth drawer 20.0 == 160.0 }
  , { name: "width above max clamps to max"
    , ok: Drawer.clampWidth drawer 9000.0 == 480.0 }
  , { name: "max below min reads as min"
    , ok: Drawer.clampWidth drawer { minWidth = 200.0, maxWidth = 100.0 } 150.0 == 200.0 }
  -- Drags: signed by edge, then clamped.
  , { name: "left drawer grows as the pointer moves right"
    , ok: Drawer.dragWidth drawer 260.0 100.0 == 360.0 }
  , { name: "right drawer grows as the pointer moves left"
    , ok: Drawer.dragWidth drawer { edge = Drawer.Right } 260.0 (-100.0) == 360.0 }
  , { name: "right drawer shrinks as the pointer moves right"
    , ok: Drawer.dragWidth drawer { edge = Drawer.Right } 260.0 50.0 == 210.0 }
  , { name: "a drag past max stops at max"
    , ok: Drawer.dragWidth drawer 260.0 1000.0 == 480.0 }
  , { name: "a drag past min stops at min"
    , ok: Drawer.dragWidth drawer 260.0 (-1000.0) == 160.0 }
  ]

quietInput :: Quiet.InputConfig Unit
quietInput = { value: "1", width: 2, align: Quiet.End, label: "l", onChange: const unit, disabled: false }

main :: Effect Unit
main = do
  unless checks (throw "smoke checks failed")
  for_ behaviour \b -> unless b.ok (throw ("behaviour check failed: " <> b.name))
