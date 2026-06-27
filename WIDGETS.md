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
| `VAccordion` | `open :: Boolean` | `Toggled Boolean` | The reference instance. **Vertical** (stacked rows): full-width header, chevron rotates 0°→90° on open; **self-debounced toggle** (`debounce :: Milliseconds`, default 120 ms) absorbs Triggerfish's `markTap`. Parent renders the body — wrap it in `VAccordion.body` for an eased reveal. Opt into easing with `motion :: Motion` (`NoMotion` by default). |
| `HAccordion` | `open :: Boolean` | `Toggled Boolean` | **Horizontal** sibling: panels as side-by-side columns; a collapsed panel folds to a thin rotated spine. Same contract and internal core as `VAccordion`; only collapsed rendering differs. |
| `Toggle` | `value :: Boolean` | `Changed Boolean` | The minimal instance — no ephemeral state. Switch track + knob, optional label. |
| `Stepper` | `value :: Int` | `Changed Int` | `‹ value ›` with clamped `min`/`max`/`step`. Arrows disable at the bounds. |
| `Slider` | `value :: Number` | `Changed Number` | Range input, **self-debounced** (`debounce`, default 80 ms) because a drag floods input events. `0.0` emits every step. |
| `Knob` | `value :: Number` | `Changed Number` | Vertical-drag rotary, 140 px = full range, 300° sweep, pure SVG. **Default debounce 0** — the read-back is the gesture feedback. |
| `DoubleKnob` | `outer`, `inner :: Layer` | `OuterChanged` / `InnerChanged Number` | Concentric two-layer knob (Strymon / Chase Bliss). Each layer dragged independently; the tag says which. |
| `SegmentedControl` | `active :: String` | `Selected String` | Tab/segment selector. Parent owns `active` **and renders the pane** — the control is only the selector. |
| `Select` | `selected :: Maybe String` | `Selected String` | Single-select dropdown, optional `searchable` typeahead. `selected` is controlled; **`open`/`query`/`hovered` are ephemeral** interaction state the widget owns. Options may be **flat** (`defaultInput`), in **named groups** as an inline list (`groupedInput`), or the same groups as a **macOS-style fly-out menu** (`cascadingInput`, `cascade = true`) — hover a group to pop its leaves out to the side. All additive, one level deep; `selected` resolves across every shape and `Output`/`Slot` never change. |
| `Compare` | `position :: Number` | `Moved Number` | Before/after comparison wipe (draggable divider). The two layers are **static `HH.PlainHTML`** in `Input` — comparing renderings, not interacting through them — which is what lets a divider-drag widget sit on the leaf contract. The widget owns the drag (document mousemove + `getBoundingClientRect`). |

## Chrome functions (action-polymorphic)

For containers that must wrap caller-owned *interactive* content (see CONTRACT
rule 5). Each is `forall w i. Config -> … -> HH.HTML w i`.

| Module | Signature shape | Notes |
|---|---|---|
| `Panel` | `PanelConfig -> Array HTML -> HTML` | Titled surface with optional sub. The general form of Triggerfish's `panelShell` body. |
| `Field` | `FieldConfig -> HTML -> HTML` | Labelled form row (label · control · optional hint). |
| `Modal` | `ModalConfig i -> Array HTML -> HTML` | Overlay + centred panel; `onClose :: i` raised by backdrop or ×. Renders nothing when `open = false`. Backdrop and panel are siblings, so no `stopPropagation` needed. |
| `Toast` | `ToastConfig i -> HTML` | Banner coloured by `Variant (Info\|Success\|Warning\|Error)`; optional `onDismiss :: Maybe i`. The atom a future stateful toast-host would render. |
| `VAccordion.body` / `HAccordion.body` | `{ open, motion } -> Array HTML -> HTML` | Optional eased wrapper for the parent-rendered accordion body. Keeps the content **mounted** and slides its height (CSS grid-rows `0fr`↔`1fr`) instead of unmounting on collapse. Use when you want the reveal animated; the plain `if open then [body] else []` (unmount) is still right for a body of expensive live children. |

### Motion — opt-in easing

`Hylograph.Halogen.UI.Motion` carries the `Motion` / `Easing` types, `defaultMotion`
(180 ms ease-out), and `transition`. The kit's default is **`NoMotion`** — instant,
no easing — and you opt a single widget in (the accordion chevron via the `motion`
`Input` field; the body reveal via `Accordion.body`). `css/hylograph-ui.css` carries
a `prefers-reduced-motion` block that neutralises these transitions for users who
asked the OS to reduce motion, even after they've been opted in.

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
- **Menu / context menu** — ephemeral-open like `Select`, but **action items**
  rather than a controlled selection, summonable at an arbitrary point (right-click
  context menu, menu bar). `Select`'s `cascade` already covers the one-level fly-out
  *picker* (a controlled value) — including hover-intent, keyboard nav, and edge-flip;
  the standalone widget adds summon-at-a-point and arbitrary nesting depth.
