# Hylograph Halogen UI

Load the **`halogen-widgets`** widget toolkit into context for *consuming*
the library in a Halogen app — wiring widgets, owning their state, raising
their `Output` back into your `Action`. Fourteen widgets on one uniform controlled
contract; the value of the library is the rail, not any single widget. Pair
with `/halogen-hooks` for behaviour-level state inside hand-rolled code.

## Arguments

$ARGUMENTS

## Instructions

When invoked without arguments, confirm the contract is loaded and use these
widgets and patterns when writing Halogen UI going forward. When invoked with a
file path, review that file: flag hand-rolled controls that the library already
provides (modals, accordions, dropdowns, debounced sliders/knobs), and check
existing usage against the contract and gotchas below. Match the file's existing
state style; don't propose component refactors unless asked.

## Install

```bash
# Path dep until the library is registry-published:
# in your spago.yaml workspace:
#   extraPackages:
#     halogen-widgets:
#       path: /Users/afc/work/afc-work/purescript-halogen-widgets
#
# in your package dependencies:
#   - halogen-widgets

# To use the theme stylesheet:
<link rel="stylesheet" href="halogen-widgets.css">
```

Widgets carry baked-in light-mode fallbacks via `var(--hw-*, fallback)`, so they
render correctly with NO stylesheet. Linking `halogen-widgets.css` enables theming:
dark via `@media (prefers-color-scheme: dark)`, or set `data-theme="light" |
"dark" | "hylograph"` on the host element (usually `<html>`).

## The contract — five rules every widget obeys

These come straight from the library's CONTRACT.md. Internalise them; you'll
see them in every widget's surface.

1. **Controlled.** The parent owns the value. `Input` carries it; `Output`
   carries a *request to change it*. The widget never mutates its own copy.
2. **Resync on `receive`.** Every widget re-syncs its internal state from
   `Input` on every parent render. You never lose intent to drift.
3. **`MonadAff` everywhere.** Behaviours like debounce live *inside* the widget
   — never re-implement them in app code (use `/halogen-hooks` for *behaviour
   in your own components*).
4. **Uniform exports.** Every widget exports `Input`, `Output(..)`, `Query(..)`,
   `Slot`, `component`, `defaultInput`, in that order.
5. **Two tiers.** Leaf widgets are full `H.Component`s; containers wrapping
   caller-owned *interactive* content (Modal, Panel, Field, Toast) are
   action-polymorphic *render functions*, because Halogen has no children-
   channel for foreign-typed actions.

## The widgets

### Leaf components (full `H.Component`s — slot, query, output)

| Module | Controlled value | `Output` | Notes |
|---|---|---|---|
| `VAccordion` | `open :: Boolean` | `Toggled Boolean` | **Vertical** disclosure header (stacked rows); self-debounced toggle (default 120 ms — coalesces double-dispatched clicks). Parent renders the body. The common case. |
| `HAccordion` | `open :: Boolean` | `Toggled Boolean` | **Horizontal** sibling: panels as side-by-side columns, collapsing to a rotated spine (Triggerfish layout). Identical contract; only collapsed rendering differs. |
| `Toggle` | `value :: Boolean` | `Changed Boolean` | The minimal instance. Switch track + knob, optional label. |
| `Stepper` | `value :: Int` | `Changed Int` | `‹ value ›` with clamped `min`/`max`/`step`. Arrows disable at bounds. |
| `Slider` | `value :: Number` | `Changed Number` | Range input, **self-debounced** (default 80 ms) — a drag floods input events. Pass `Milliseconds 0.0` to emit every step. |
| `Knob` | `value :: Number` | `Changed Number` | Vertical-drag rotary, 140 px = full range, 300° sweep, pure SVG. **Default debounce 0** — the parent's read-back is what the user is watching mid-drag. Raise debounce only for expensive per-tick sinks (MIDI write, audio ramp). |
| `DoubleKnob` | `outer :: Layer`, `inner :: Layer` | `OuterChanged Number` / `InnerChanged Number` | Concentric two-layer knob (Strymon / Chase Bliss pattern). Each layer independently dragged; tag tells you which. |
| `SegmentedControl` | `active :: String` | `Selected String` | Tab-bar selector. **Parent owns `active` AND renders the pane** — control is only the selector. |
| `Select` | `selected :: Maybe String` | `Selected String` | Dropdown, optional `searchable` typeahead. `selected` controlled; `open`/`query`/`hovered` are *ephemeral* (widget owns them). Options are **flat** (`defaultInput opts`), **named groups** as an inline list (`groupedInput groups`), or the same groups as a **macOS-style fly-out menu** (`cascadingInput groups`, `cascade = true` — hover a group to pop its leaves out). All additive, one level deep; `selected` resolves across every shape; `Output`/`Slot` unchanged. |
| `Compare` | `position :: Number` | `Moved Number` | Before/after comparison wipe. The two layers are static `HH.PlainHTML` in `Input` (`before`/`after`) — comparing renderings, not interacting through them — which is exactly what lets a draggable-divider widget be a leaf component. The widget owns the drag. |

### Chrome functions (render functions polymorphic in the caller's action)

| Module | Signature | Notes |
|---|---|---|
| `Panel` | `PanelConfig -> Array HTML -> HTML` | Titled surface. |
| `VAccordion.body` / `HAccordion.body` | `{ open, motion } -> Array HTML -> HTML` | Optional **eased** wrapper for the accordion body the parent renders. Keeps content mounted and slides its height (grid-rows `0fr`↔`1fr`) instead of unmounting. Use for an animated reveal; plain `if open then [body] else []` (unmount) stays right for a body of expensive live children. |
| `Field` | `FieldConfig -> HTML -> HTML` | Labelled form row (label · control · optional hint). |
| `Modal` | `ModalConfig i -> Array HTML -> HTML` | Overlay + centred panel; `onClose :: i` raised by backdrop or ×. Renders nothing when `open: false`. |
| `Toast` | `ToastConfig i -> HTML` | Banner coloured by `Variant (Info\|Success\|Warning\|Error)`; optional `onDismiss :: Maybe i`. |

## Wiring a leaf widget — the canonical shape

The library's `defaultInput` is the on-ramp: instantiate with one argument and
override fields you care about with record update.

```purescript
import Halogen.Widgets.Slider as Slider
import Type.Proxy (Proxy(..))

type Slots =
  ( slider :: Slider.Slot Unit
  -- … one entry per widget instance, the value type is its Slot
  )

_slider :: Proxy "slider"
_slider = Proxy

type State = { gain :: Number, … }

data Action
  = GainChanged Number
  | …

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  HH.slot _slider unit Slider.component
    ((Slider.defaultInput st.gain) { min = 0.0, max = 100.0 })
    (\(Slider.Changed v) -> GainChanged v)

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  GainChanged v -> H.modify_ _ { gain = v }
  …
```

That's the whole pattern. The four moving parts are exactly:

1. **A slot row entry** (`slider :: Slider.Slot Unit`)
2. **A `Proxy` for the row label** (`_slider = Proxy`)
3. **An `Action` constructor** that takes the new value
4. **A `handleAction` arm** that does `H.modify_` to honour the request

Want multiple instances of the same widget? Vary the slot index: `HH.slot
_slider "channel-1" …`, `HH.slot _slider "channel-2" …`.

## Wiring a chrome function — even simpler

No slot, no query, no `Proxy` — just pass your action through:

```purescript
import Halogen.Widgets.Modal as Modal

data Action = OpenModal | CloseModal | …

render st =
  HH.div_
    [ HH.button [ HE.onClick \_ -> OpenModal ] [ HH.text "Open" ]
    , Modal.modal
        { open: st.modalOpen, title: "Confirm", onClose: CloseModal }
        [ HH.p_ [ HH.text "body content of your choice — typed in YOUR action" ]
        , HH.button [ HE.onClick \_ -> CloseModal ] [ HH.text "Close" ]
        ]
    ]
```

## Controlled vs ephemeral state — which lives where

Not every bit of widget state is in `Input`. Only the **app-meaningful** part is.
Pure interaction transient (a dropdown's open-flag, a search input's filter
text) is owned by the widget. Test: would the parent ever want to read, persist,
or drive it from elsewhere? If yes → controlled (in `Input`). If no → ephemeral
(widget keeps it internally, never surfaces).

`VAccordion.open` is **controlled** — Triggerfish persists `collapsed`. `Select.open`
is **ephemeral** — nothing outside the moment cares. Same word, opposite tier;
decided entirely by whether the app cares.

## Debounce — when to crank it up, when to leave off

The library has the same generation-counter debounce idiom in the accordions,
`Slider`, `Knob`, `DoubleKnob`. Defaults differ on purpose:

- **`VAccordion` / `HAccordion` 120 ms** — coalesces a double-dispatched click into one toggle.
- **`Slider` 80 ms** — a drag floods input events.
- **`Knob` / `DoubleKnob` 0 ms** — the parent's read-back IS the gesture
  feedback; debounce would lag the user's hand.

Raise `debounce` when each `Output` triggers expensive downstream work — a
MIDI write, a network round-trip, an audio param ramp. Lower it to `0` when
you want every tick.

```purescript
(Knob.defaultInput st.cutoff) { debounce = Milliseconds 30.0 }
```

## Motion — opt-in easing, off by default

`Halogen.Widgets.Motion` carries `Motion` / `Easing`, `defaultMotion`
(180 ms ease-out), and `transition`. The kit default is **`NoMotion`** — instant,
no easing — on purpose; you opt a single widget in. Today's animatable surface is
the accordion:

- **Chevron** — pass `motion` in the accordion's `Input`; it rotates 0°→90° eased.
- **Body reveal** — wrap the parent-rendered body in `VAccordion.body { open, motion } [body]`
  (a chrome function). It keeps the body **mounted** and slides its height, where
  the plain `if open then [body] else []` unmounts and so can't animate. Trade-off:
  the body stays in the DOM while collapsed — fine for presentational content,
  weigh it for a body of expensive live children.

```purescript
[ HH.slot _acc p.key VAccordion.component
    ((VAccordion.defaultInput p.label) { open = open, motion = defaultMotion })
    (\(VAccordion.Toggled o) -> Toggle p.key o)
, VAccordion.body { open, motion: defaultMotion } [ bodyFor p ]
]
```

`css/halogen-widgets.css` carries a `prefers-reduced-motion` block that neutralises
these transitions for users who asked the OS to reduce motion, even once opted in.

## Theming

Three themes ship in `halogen-widgets.css`:

- **`light`** (default) — Swiss-restrained whites and slate-grey ink.
- **`dark`** — applied automatically by OS preference if no `data-theme` is set,
  or forced via `data-theme="dark"`.
- **`hylograph`** — opinionated Swiss-poster: warm paper, near-black ink, single
  vermilion accent, 2 px corners, Helvetica. Opt in via `data-theme="hylograph"`.

To recolour the widgets, override the `--hw-*` variables on any host element —
no PureScript change needed. The full token list lives in `halogen-widgets.css`
(ink, ink-soft, line, surface, surface-alt, accent, danger, warn, ok,
track-off, control-border, knob, shadow, backdrop, page-bg; plus `--hw-radius`
and `--hw-font` which only `hylograph` overrides).

In **`hylograph` mode**, the showcase additionally typesets each widget's
contract surface (Input record, Output ADT, component signature) via Sigil —
that's a showcase trick, not a feature of the widgets themselves; consumers
don't get it automatically.

## Gotchas

- **`receive` is wired for you** — every widget honours `Input` changes from
  the parent. If a widget seems "stuck on its first value," it's something
  else (a memoised render, parent state not actually changing, etc.).
- **`defaultInput` always over a record literal.** It's the on-ramp; the
  record's fields will gain non-breaking additions over time. Constructing
  `Input` by hand is allowed but commits you to maintain every field.
- **`Slot` types are namespaced per widget.** A slot row is
  `( slider :: Slider.Slot Unit, knob :: Knob.Slot Unit, …)`; don't unify
  them. The `Unit` is the slot *index*; use `String`/`Int`/your own type to
  multi-instance.
- **Knob output is uncontrolled by default** (debounce = 0). If you mirror it
  to a sink that doesn't handle floods, set `debounce = Milliseconds 30.0` or
  higher.
- **`SegmentedControl` only renders the selector** — you render the pane.
  Same for the accordions (parent renders the body).
- **`Compare`'s layers are `PlainHTML`, not slots.** Pass static content
  (`HH.img`, a styled `HH.div`, highlighted code) as `before`/`after` — they
  carry no actions. That is deliberate: a comparison wipe compares renderings.
  If you need interaction on one side, it doesn't belong in a `Compare`.
- **Two accordions, one contract.** `VAccordion` stacks rows (default);
  `HAccordion` lays out columns and folds to a rotated spine. Pick by layout,
  not behaviour — they share an internal core. A `rotate(-90deg)` collapsed
  label where you wanted a flat header means you reached for `HAccordion` where
  `VAccordion` was wanted.
- **Radio vs bitfield is a parent-state choice, not a widget mode.** An
  accordion never owns `open` — it emits `Toggled Boolean`, a request. How many
  panels can be open at once is decided entirely by your `handleAction`:
  - *Radio* (one open at a time): keep a single key, `open :: String`.
    `Selected k -> modify_ _ { openPanel = k }`; render `open = openPanel == key`.
  - *Bitfield* (any subset, **Triggerfish's case**): keep a set,
    `collapsed :: Array String` (or `Set String`).
    `Toggle k wantOpen -> modify_ \s -> s { collapsed = if wantOpen then filter (_ /= k) s.collapsed else snoc s.collapsed k }`; render `open = not (elem key collapsed)`.

  Same widget, same `Toggled` output for both. Don't reach for an
  `AccordionGroup` that owns the open-set: the moment a panel body is
  interactive it hits the same children-channel limit that makes Modal/Panel
  chrome functions. Parent-orchestrated is the right level.

## When NOT to use this library — reach for `/halogen-hooks` instead

A *behaviour you need inside a component you're already writing* — debounce a
search input that you render yourself, run an effect on mount, hold a ref to a
DOM node — is a Hook, not a widget. The library widgets are for *reusable*
controls. Don't dress up a one-off behaviour as a widget; the boilerplate isn't
free even when it's mechanical.

Rule of thumb: **a Hook is how you write a behaviour once in app code; a widget
is how you ship a behaviour for reuse.** Triggerfish's accordion-toggle
debounce was the canonical case — could have been either; we shipped it as
`VAccordion`/`HAccordion` to retire the pattern across the ecosystem.
