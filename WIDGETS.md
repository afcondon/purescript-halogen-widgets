# The widget roster

Every entry obeys the contract in [CONTRACT.md](./CONTRACT.md). The roster is
split by tier: **leaf components** are full `H.Component`s (controlled,
`receive`-resyncing, `MonadAff`); **chrome functions** are action-polymorphic
render functions for containers that wrap caller-owned content.

All widgets share `Hylograph.Halogen.UI.Style` (the `sty`/`cls`/`clss` helpers
and a restrained light-Swiss palette), so the set reads as one family. Colours
are inline + class-named, so consumers can re-theme via plain CSS.

## Leaf components (`H.Component Query Input Output m`)

Each exports `Input`, `Output(..)`, `Query(..)`, `Slot`, `component`,
`defaultInput`.

| Module | Controlled value | `Output` | Notes |
|---|---|---|---|
| `VAccordion` | `open :: Boolean` | `Toggled Boolean` | The reference instance. **Vertical** (stacked rows): full-width header, chevron ▾→▸ on collapse; **self-debounced toggle** (`debounce :: Milliseconds`, default 120 ms) absorbs Triggerfish's `markTap`. Parent renders the body. |
| `HAccordion` | `open :: Boolean` | `Toggled Boolean` | **Horizontal** sibling: panels as side-by-side columns; a collapsed panel folds to a thin rotated spine. Same contract and internal core as `VAccordion`; only collapsed rendering differs. |
| `Toggle` | `value :: Boolean` | `Changed Boolean` | The minimal instance — no ephemeral state. Switch track + knob, optional label. |
| `Stepper` | `value :: Int` | `Changed Int` | `‹ value ›` with clamped `min`/`max`/`step`. Arrows disable at the bounds. |
| `Slider` | `value :: Number` | `Changed Number` | Range input, **self-debounced** (`debounce`, default 80 ms) because a drag floods input events. `0.0` emits every step. |
| `Knob` | `value :: Number` | `Changed Number` | Vertical-drag rotary, 140 px = full range, 300° sweep, pure SVG. **Default debounce 0** — the read-back is the gesture feedback. |
| `DoubleKnob` | `outer`, `inner :: Layer` | `OuterChanged` / `InnerChanged Number` | Concentric two-layer knob (Strymon / Chase Bliss). Each layer dragged independently; the tag says which. |
| `SegmentedControl` | `active :: String` | `Selected String` | Tab/segment selector. Parent owns `active` **and renders the pane** — the control is only the selector. |
| `Select` | `selected :: Maybe String` | `Selected String` | Single-select dropdown, optional `searchable` typeahead. `selected` is controlled; **`open`/`query` are ephemeral** interaction state the widget owns. |

## Chrome functions (action-polymorphic)

For containers that must wrap caller-owned *interactive* content (see CONTRACT
rule 5). Each is `forall w i. Config -> … -> HH.HTML w i`.

| Module | Signature shape | Notes |
|---|---|---|
| `Panel` | `PanelConfig -> Array HTML -> HTML` | Titled surface with optional sub. The general form of Triggerfish's `panelShell` body. |
| `Field` | `FieldConfig -> HTML -> HTML` | Labelled form row (label · control · optional hint). |
| `Modal` | `ModalConfig i -> Array HTML -> HTML` | Overlay + centred panel; `onClose :: i` raised by backdrop or ×. Renders nothing when `open = false`. Backdrop and panel are siblings, so no `stopPropagation` needed. |
| `Toast` | `ToastConfig i -> HTML` | Banner coloured by `Variant (Info\|Success\|Warning\|Error)`; optional `onDismiss :: Maybe i`. The atom a future stateful toast-host would render. |

## Adding the next widget

Follow the checklist at the end of CONTRACT.md. Decide the tier first: if the
widget owns no caller content, it's a **leaf component**; if it wraps
caller-owned interactive content, it's a **chrome function** (or a
controlled-header leaf where the parent renders the body, as `VAccordion` and
`SegmentedControl` do). Pin its full surface into `test/Main.purs` so a
signature regression fails the build.

### Candidate next widgets (not yet built)

- **Knob** — controlled `Number`, radial drag. Triggerfish and
  producing-with-your-feet both have bespoke knob geometry to harvest.
- **TextInput / TextArea** — controlled `String`, self-debounced (the obvious
  partner to `Slider`'s debounce).
- **Tooltip** — chrome + a hover behaviour (delay-in / delay-out).
- **Toast host** — stateful leaf that queues `Toast` atoms with auto-dismiss timers.
- **Menu / context menu** — ephemeral-open like `Select`, but action items
  rather than a controlled selection.
