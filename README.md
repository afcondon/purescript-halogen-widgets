# purescript-hylograph-halogen-ui

Reusable Halogen UI widgets for the Hylograph ecosystem, built on **one uniform
controlled component contract**. An old-fashioned widget toolkit, on purpose:
the value is not any single widget but that every widget obeys the same small
rail, so the next one is predictable to author and safe to delegate.

> Status: **v0.1, contract-settled.** Ten widgets ship — six leaf components
> (`Accordion`, `Toggle`, `Stepper`, `Slider`, `SegmentedControl`, `Select`) and
> four chrome functions (`Panel`, `Field`, `Modal`, `Toast`) — all compiling, all
> on the one contract, with a type-level smoke test. The roster is the
> beginning, not the end; see [WIDGETS.md](./WIDGETS.md).

Read **[CONTRACT.md](./CONTRACT.md)** first — it is the actual product — then
[WIDGETS.md](./WIDGETS.md) for the roster. The five rules in brief:

1. **Controlled** — the parent owns the state; `Input` carries the value,
   `Output` carries a request, the widget never mutates its own copy.
2. **Resync on `receive`** — `receive = Just <<< Receive`, always; the rule that
   prevents controlled-component drift.
3. **Every widget is `MonadAff`** — so behaviours (debounce, throttle) live
   *inside* the widget, not re-rolled in app code.
4. **Uniform exports** — `Input`, `Output(..)`, `Query(..)`, `Slot`,
   `component`, `defaultInput`, in that order.
5. **Two tiers** — leaf widgets are full components; containers that wrap
   caller-owned interactive content use the controlled-header pattern (the
   parent owns the open state and renders the body).

## Build

```bash
spago build
```

## Theming (light / dark)

Every colour is a CSS custom property with a baked-in light fallback —
`var(--hg-ink, #2b2b2b)`. So the widgets are **self-contained** (they render
with the light palette even with no stylesheet) *and* **themeable**: include
`css/hylograph-ui.css` (or just define the `--hg-*` variables yourself) and you
get dark mode for free.

```html
<link rel="stylesheet" href="hylograph-ui.css">
```

Dark mode follows the OS (`prefers-color-scheme`) automatically, or you can force
it by setting `data-theme="light" | "dark"` on a host element (usually `<html>`).
No PureScript changes, no per-widget props — recolour by overriding variables.
The showcase's header toggle does exactly this.

## Worked example: `Accordion`

`Accordion` is the reference instance — a controlled, self-debouncing disclosure
header. One `Accordion` is one panel's *header* (the parent owns `open` and
renders the body). Sketch of use from a parent component:

```purescript
import Hylograph.Halogen.UI.Accordion as Accordion

type Slots = ( generate :: Accordion.Slot Unit )

renderPanel :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderPanel s =
  HH.div_
    ( [ HH.slot (Proxy :: _ "generate") unit Accordion.component
          (Accordion.defaultInput "GENERATE")
            { open = not (elem "GENERATE" s.collapsed)
            , sub = Just (show (length s.gen) <> " sources")
            }
          \(Accordion.Toggled wantOpen) -> SetPanelOpen "GENERATE" wantOpen
      ]
      -- the parent owns `open`, so the parent renders the body:
      <> if elem "GENERATE" s.collapsed then [] else [ generatePanelBody s ]
    )
```

The `120 ms` default debounce coalesces a double-dispatched click into a single
`Toggled` — the same problem Triggerfish solves by hand with
`markTap`/`lastTapMicros`, here absorbed into the widget.

## License

MIT
