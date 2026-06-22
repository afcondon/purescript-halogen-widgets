# purescript-hylograph-halogen-ui

Reusable Halogen UI widgets for the Hylograph ecosystem, built on **one uniform
controlled component contract**. An old-fashioned widget toolkit, on purpose:
the value is not any single widget but that every widget obeys the same small
rail, so the next one is predictable to author and safe to delegate.

> Status: **greenfield, contract-first.** The contract is settled and has one
> compiling reference instance (`Accordion`). The remaining widgets are
> instances of the same skeleton.

Read **[CONTRACT.md](./CONTRACT.md)** first — it is the actual product. The five
rules in brief:

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

## The first widget: `Accordion`

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
