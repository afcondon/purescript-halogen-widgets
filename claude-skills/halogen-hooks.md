# Halogen Hooks

Load idiomatic **Halogen Hooks** (`purescript-halogen-hooks`, 0.6.x) into
context for writing *behaviour* in hand-rolled Halogen app code — local state,
effects, refs, and especially **debounce / throttle** — without the ceremony of
a full child component. Apply these patterns when the thing you need is a
stateful behaviour inside a component you're already writing, not a reusable
widget.

## Arguments

$ARGUMENTS

## Instructions

When invoked without arguments, confirm the patterns are loaded and use Hooks
idioms for behaviour-level state/effects/debounce in PureScript Halogen code
going forward. When invoked with a file path, review that file: flag hand-rolled
debounce/throttle/state-threading that a Hook would simplify, and check existing
Hooks usage against the gotchas below.

## When to reach for Hooks (and when not)

- **Hooks** — a *behaviour* local to one component: debounced search/toggle,
  a mount/unmount effect, a tick effect on changed deps, an interval, a ref to a
  DOM node. You keep full control of rendering; the hook supplies the wiring.
- **A classic `H.Component`** — when you need queries, multiple slots, or a
  long-lived component identity. (Both interop: a Hooks component *is* an
  `H.Component`.)
- **The `hylograph-halogen-ui` widget library** — when the thing is a *reusable
  widget* (accordion header, slider, modal). Those internalize these very
  behaviours behind the controlled contract (see that repo's `CONTRACT.md`).
  Rule of thumb: **a Hook is how you write a behaviour once in app code; a widget
  is how you ship a behaviour for reuse.** The Triggerfish accordion debounce is
  the canonical case — a Hook would have prevented the yak-shave in app code; the
  library's `Accordion` prevents it for everyone.

## Setup

```bash
spago install halogen-hooks      # adds the `halogen-hooks` dependency
```

```purescript
import Halogen.Hooks as Hooks
import Halogen.Hooks.HookM (HookM)
import Halogen.Hooks.HookM as HookM
```

## The component shape

`Hooks.component` takes a function of `tokens` and `input`, written in the
qualified `Hooks.do` block, returning `ComponentHTML` wrapped in `Hooks.pure`:

```purescript
component :: forall q i m. MonadAff m => H.Component q i Output m
component = Hooks.component \{ outputToken } _input -> Hooks.do
  count /\ countId <- Hooks.useState 0
  Hooks.pure do                                    -- the render
    HH.button [ HE.onClick \_ -> Hooks.modify_ countId (_ + 1) ]
      [ HH.text ("count: " <> show count) ]
```

`tokens` is `{ queryToken, slotToken, outputToken }`. Note the render's event
handlers run in **`HookM`**, the Hooks action monad (the analogue of `HalogenM`).

## Core hooks (real 0.6.x signatures)

```purescript
-- Local state. Returns the current value and a StateId to mutate it.
useState :: forall state m. state -> Hook m (UseState state) (state /\ StateId state)

-- Run once on mount; optionally return a cleanup run on unmount.
useLifecycleEffect :: forall m. HookM m (Maybe (HookM m Unit)) -> Hook m UseEffect Unit

-- Run after render whenever the captured deps change (see `captures`).
useTickEffect :: forall m. MemoValues -> HookM m (Maybe (HookM m Unit)) -> Hook m UseEffect Unit

-- A persistent Effect Ref (survives re-renders). Great for "previous fork id".
useRef :: forall m a. a -> Hook m (UseRef a) (a /\ Ref a)

-- Memoize an expensive pure value against changed deps.
useMemo :: forall m a. MemoValues -> (Unit -> a) -> Hook m (UseMemo a) a

-- Wrap a deps record for useTickEffect / useMemo.
captures :: forall memos a. Eq (Record memos) => Record memos -> (MemoValues -> a) -> a
```

`useTickEffect` with deps:

```purescript
Hooks.captures { query } Hooks.useTickEffect do
  -- runs after every render where `query` changed value
  results <- runSearch query
  Hooks.modify_ resultsId (const results)
  pure Nothing            -- no cleanup
```

## HookM — the action monad

```purescript
get     :: StateId state -> HookM m state
modify_ :: StateId state -> (state -> state) -> HookM m Unit
put     :: StateId state -> state -> HookM m Unit
raise   :: OutputToken o -> o -> HookM m Unit        -- emit component output
fork    :: HookM m Unit -> HookM m H.ForkId          -- background work
kill    :: H.ForkId -> HookM m Unit                  -- cancel a fork
subscribe :: HS.Emitter (HookM m Unit) -> HookM m H.SubscriptionId
```

`MonadAff`/`MonadEffect` lift into `HookM` as usual (`liftAff`, `liftEffect`).

## Recipe: `useDebouncedCallback` (the debounce yak-shave, solved)

A custom Hook that returns a trigger you can call freely; the wrapped action
runs only after a quiet period, and each call cancels the previously-scheduled
run. This is *verified to compile* against halogen-hooks 0.6.3.

```purescript
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Hooks (Hook, HookType, Pure, type (<>), UseRef, class HookNewtype)
import Halogen.Hooks as Hooks
import Halogen.Hooks.HookM (HookM)
import Halogen.Hooks.HookM as HookM

-- A custom Hook is a new HookType plus a HookNewtype instance naming the
-- hooks it is built from. `Hooks.wrap` hides those internals behind it.
foreign import data UseDebouncedCallback :: Type -> HookType

instance HookNewtype (UseDebouncedCallback a) (UseRef (Maybe H.ForkId) <> Pure)

useDebouncedCallback
  :: forall m a
   . MonadAff m
  => Milliseconds
  -> (a -> HookM m Unit)                 -- the action to debounce
  -> Hook m (UseDebouncedCallback a) (a -> HookM m Unit)   -- returns: the trigger
useDebouncedCallback ms fn = Hooks.wrap Hooks.do
  _ /\ forkRef <- Hooks.useRef Nothing
  let
    trigger a = do
      mPrev <- liftEffect (Ref.read forkRef)
      traverse_ HookM.kill mPrev            -- cancel the pending run, if any
      forkId <- HookM.fork do
        liftAff (delay ms)
        liftEffect (Ref.write Nothing forkRef)
        fn a
      liftEffect (Ref.write (Just forkId) forkRef)
  Hooks.pure trigger
```

Use it inside a component — debounced commit on every keystroke:

```purescript
component :: forall q i m. MonadAff m => H.Component q i Output m
component = Hooks.component \{ outputToken } _ -> Hooks.do
  value /\ valueId <- Hooks.useState ""
  commit <- useDebouncedCallback (Milliseconds 300.0) \v ->
    HookM.raise outputToken (Committed v)
  Hooks.pure do
    HH.input
      [ HP.value value
      , HE.onValueInput \v -> do
          Hooks.put valueId v       -- update the field immediately
          commit v                  -- ...but only emit after 300 ms of quiet
      ]
```

For the **double-dispatch** case (a toggle fired twice in quick succession by a
re-render, as in Triggerfish), the same hook coalesces the duplicates: both
calls request the same value, the first fork is killed, only the last survives.

## Custom-hook pattern in general

1. `foreign import data UseThing :: HookType` (add a `Type ->` prefix per type
   parameter, e.g. `UseDebouncedCallback :: Type -> HookType`).
2. `instance HookNewtype UseThing (UseState X <> UseEffect <> Pure)` — list the
   primitive hooks it composes, ending in `Pure`.
3. Implement with `Hooks.wrap Hooks.do { ...primitive hooks... ; Hooks.pure result }`.

## Gotchas

- **`Hooks.do` is qualified do** — `bind`/`discard`/`pure` come from Hooks, not
  Prelude, inside the block. Use `Hooks.pure` for the render and `Hooks.modify_`
  etc. in handlers.
- **`useTickEffect` needs `captures`** — without wrapped deps it has no change
  signal. Capture exactly the values whose change should re-run the effect.
- **`useRef` persists across renders; `useState` triggers re-render.** Use a ref
  (not state) for bookkeeping that shouldn't itself cause a render — like the
  pending `ForkId` above.
- **Kill pending forks on unmount** for long-lived timers: pair the fork with a
  `useLifecycleEffect` whose cleanup `HookM.kill`s it, or store the id in a ref
  and kill in the cleanup.
- **Hooks order is fixed** — call the same hooks in the same order every render;
  never conditionally. (Branch *inside* the render or the handler, not over the
  hook calls.)
- **Tokens, not imports, carry output/query/slot** — raise with the
  `outputToken` from the component's first argument.
