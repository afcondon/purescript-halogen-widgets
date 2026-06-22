# Roadmap

Where v0.1 stands and what's next. v0.1 ships the contract (CONTRACT.md), ten
widgets on it (WIDGETS.md), a type-level smoke test, the `/halogen-hooks`
companion skill, and a live show-and-tell showcase. Tags below: **[value]** to
the ship-it-and-dogfood goal, **[effort]** rough size, **→** dependencies.

## Now — "ship it" (target v0.2)

The smallest set that makes the library presentable and consumable.

- **Theming: light + dark.** [high] [med] — Move `Style` colour tokens from
  hardcoded hex to CSS custom properties (`var(--hg-ink)` etc.); ship a
  `hylograph-ui.css` with a light default and a `prefers-color-scheme` /
  `[data-theme="dark"]` dark set. Widgets keep their structural inline styles but
  reference the variables, so a theme can override without touching PureScript.
  Unblocks the showcase toggle and gives the skill real theming to document.
- **`/halogen-ui` skill.** [high] [low-med] — Companion to `/halogen-hooks`:
  teaches the contract and how to wire each widget (Slots row, `Proxy`,
  `defaultInput` override, the controlled `Output → modify_` loop). This is what
  makes widget use delegable to a cheaper model. Independent of everything else.
- **`Knob` widget.** [high] [med] — Controlled `Number`, radial drag. Needed by
  the app-validation step below (Triggerfish + producing-with-your-feet both have
  bespoke knob geometry to harvest). Self-debounced like `Slider`.
- **Deploy `widgets.hylograph.net`.** [med] [low] — Built site is staged in
  `cloudflare-sites/widgets/`. Stand up a `hylograph-widgets` CF Pages project
  (git-connected, to match the `*.hylograph.net` family) + custom domain; register
  the child project in Marginalia under `cloudflare-sites #52`. → owner go-ahead.

## Next — validate and harden

- **Dogfood on Triggerfish + Vetula.** [high] [med] → Knob, theming — Migrate the
  hand-rolled controls to the library: Triggerfish's `panelShell` → `Accordion`
  (deletes its `markTap`/`lastTapMicros` debounce), its `stepperRow` → `Stepper`,
  its knob geometry → `Knob`; same for Vetula. These then double as real demos.
  The payoff that motivated the whole library.
- **Accessibility pass.** [high] [med] — Keyboard interaction (Enter/Space on
  Accordion/Toggle, arrows on Stepper/Slider/SegmentedControl/Select, Escape
  closes Modal/Select) and ARIA (`role=switch`, `aria-expanded`, `aria-selected`,
  `role=dialog`/`aria-modal` + focus trap). A credible widget kit needs this; best
  done as one cross-cutting pass once the set is stable.
- **Showcase polish.** [med] [low-med] → theming — Light/dark toggle, per-widget
  Input/Output type tables in the "tell" column, copy-on-click code blocks,
  scrollspy nav highlight, a short "the contract" intro section.

## Later — grow and publish

- **More widgets.** [med] [med] — `TextInput`/`TextArea` (debounced; partner to
  Slider), `Checkbox`, `RadioGroup`, `Tooltip` (hover behaviour + chrome),
  `Menu`/`ContextMenu` (ephemeral-open like Select), `Toast` host (stateful queue
  with auto-dismiss timers). Each an instance of the existing skeleton.
- **Publish to the PureScript registry.** [med] [low] → API stability — `spago
  publish` so apps depend on `hylograph-halogen-ui` by bare name instead of a path.
  Do it once the contract surface has settled against real app use (don't publish
  then immediately break consumers).
- **Behavioural tests.** [low] [med] — Component-level interaction tests. Lower
  priority while the real-app migration is itself the integration test.

## Open questions / decisions

- **CSS strategy.** Inline styles are self-contained but not easily themed or
  overridden. The theming item moves *colour* to CSS variables; do we also move
  *structure* to a shipped class-based stylesheet, or keep structure inline and
  variables for theme only? (Leaning: variables for theme, structure stays inline
  — minimal disruption, still fully themeable on colour.)
- **Uncontrolled convenience wrappers.** Worth a thin `*.Uncontrolled` layer that
  stores Output→Input for callers who don't want to own state? (Leaning: not yet —
  every real consumer so far wants control.)
- **Publish timing.** Hold registry publish until after the Triggerfish/Vetula
  migration shakes out the API, or publish 0.1 now to unblock path-free consumption?
