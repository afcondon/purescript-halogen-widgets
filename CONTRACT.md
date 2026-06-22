# The component contract

This library is an old-fashioned widget toolkit on purpose. Its value is
**not** the cleverness of any one widget — it is that *every* widget obeys the
same small contract, so the twenty-first widget is as predictable as the first.
A uniform rail is what makes a widget delegable: "add a `Slider` here" becomes a
fill-in-the-blanks task against a fixed skeleton, safe to hand to a cheaper
model.

The boilerplate this contract asks for (a slot type, a query algebra, a receive
handler, output wiring) is exactly the cost that used to make heavyweight
Halogen feel like a slog. That cost is mechanical and predictable — which is to
say it is cheap for an LLM author and expensive only for human fingers. We pay
it gladly in exchange for uniformity.

## The five rules

### 1. Controlled — the parent owns the state

A widget's authoritative value lives in the **parent**, passed down through
`Input`. The widget renders that value and emits an **intent to change it**
through `Output`. It never mutates its own authoritative copy.

- `Input` carries the value (`open :: Boolean`, `value :: Number`, …) plus config.
- `Output` carries a *request* (`Toggled Boolean`, `Changed Number`), never a fact.
- The parent decides whether to honour the request and feeds the new value back
  down as `Input`.

This is the React "controlled component" pattern. It is the right default here
because the apps in this ecosystem already want to own their state — Triggerfish
keeps `collapsed :: Array String` in *grid* state and drives its panels from
there. Uncontrolled "it just remembers for me" convenience is a thin stateful
wrapper you can add later; it is never the primitive.

### 2. Resync on `receive` — the rule that prevents drift

A controlled widget keeps a **mirror** of `Input` in its `State` (so `render`
has something to read) plus any purely-ephemeral UI state (hover, an in-flight
debounce generation). The mirror is re-synced from `Input` on **every parent
render**:

```purescript
eval = H.mkEval H.defaultEval
  { handleAction = handleAction
  , receive = Just <<< Receive          -- <- never omit this
  }

handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  ...
```

Omitting `receive` is the classic Halogen controlled-component bug: the child
honours `Input` once at construction and then silently ignores the parent
forever. Every widget in this library wires `receive`. No exceptions.

### 3. Every widget is `MonadAff` — behaviours live inside

Every `component` is constrained `MonadAff m`, whether or not it currently needs
it, so that ephemeral *behaviours* — debounce, throttle, delayed reveal — can
always be absorbed **into the widget** instead of re-implemented in app code.

This is where the toolkit earns its keep over a plain render helper. The
canonical debounce recipe is a generation counter in `State` plus `H.fork`:

```purescript
Toggle -> do
  st <- H.get
  let requested = not st.input.open
  next <- H.modify \s -> s { version = s.version + 1 }
  let mine = next.version
  void $ H.fork do
    liftAff (delay st.input.debounce)
    s' <- H.get
    when (s'.version == mine) (H.raise (Toggled requested))   -- only the latest fork wins
```

Triggerfish hand-rolls exactly this (its `markTap` / `lastTapMicros` machinery)
to stop a 30 fps re-render from double-dispatching a panel toggle. In this
library that machinery is *inside* `Accordion`; consumers never write it again.

### 4. Uniform exports — the skeleton

Every widget module exports the same surface, in the same order:

```purescript
module Hylograph.Halogen.UI.Thing
  ( Input          -- controlled value + config (a record)
  , Output(..)     -- requests, not facts
  , Query(..)      -- imperative parent->child escape hatch (often unused)
  , Slot           -- type Slot = H.Slot Query Output
  , component
  , defaultInput   -- a sensible starting Input; override what you care about
  ) where
```

`defaultInput` matters: it is the on-ramp that lets a caller (or a model)
instantiate the widget by overriding two fields instead of inventing a whole
record. `Query` is included even when empty-ish, so the `Slot` type — and
therefore the shape a parent must wire — is identical across widgets.

### 5. Two tiers — leaf widgets vs. container chrome

There is one place where "make it a full component" hits a wall that is *not*
boilerplate but the type system itself, and it is worth naming because the
accordion sits right on it.

A Halogen component's `render` produces `ComponentHTML childAction childSlots m`
— HTML typed in the **child's** action. It therefore **cannot embed arbitrary
parent-owned interactive content**: there is no "children" channel the way React
has one, because the parent's HTML is typed in the parent's action. (You can
pass static `HH.PlainHTML`, i.e. `HTML Void Void`, as data — fine for a purely
presentational body, useless for an accordion panel full of live knobs.)

So the contract has two tiers:

- **Leaf widgets** — toggles, sliders, selects, the *interactive chrome* of a
  container — are full `H.Component`s following rules 1–4. Self-contained; they
  own no caller content.
- **Containers** that must wrap caller-owned *interactive* content (accordion
  bodies, tab panes, sidebars) cannot swallow that content. They are resolved
  the **controlled-header** way: because the parent already owns the open/closed
  state (rule 1), it also renders the body itself — `if open then [body] else []`
  — and the *component* is just the interactive header that emits `Toggled`.
  A thin chrome helper may compose "header slot + conditional body" for
  ergonomics, but the interactive part is always a proper leaf component.

`Accordion` is the worked example: it is the controlled, debounced **header**
(open header, or a collapsed tab); the panel body stays with the parent, which
owns `open` anyway. This honours "full component for the interactive part"
without pretending the type system has a children channel it doesn't.

## Checklist for a new widget

- [ ] `Input` is a record: controlled value(s) + config. `defaultInput` provided.
- [ ] `Output` constructors are *requests*, named in the past tense (`Changed`, `Toggled`).
- [ ] `State` mirrors `Input`, plus only ephemeral UI state.
- [ ] `receive = Just <<< Receive`, and `Receive` overwrites the mirror.
- [ ] `component :: forall m. MonadAff m => H.Component Query Input Output m`.
- [ ] Any debounce/throttle uses the generation-counter + `H.fork` recipe, inside the widget.
- [ ] Exports in skeleton order; `Slot = H.Slot Query Output`.
- [ ] If it wraps caller-owned interactive content, it's a controlled-header leaf + the parent renders the body.
