-- | The showcase root. It is the first real *consumer* of the library, and so
-- | it dogfoods the contract: this one component owns the state of every demo
-- | below and handles each widget's `Output` to update it. Show (the live
-- | widget) and tell (its code) sit side by side.
module Showcase (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

import Hylograph.Halogen.UI.Style (sty, cls)
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

foreign import setThemeAttr :: String -> Effect Unit
foreign import prefersDark :: Effect Boolean

data Theme = Light | Dark | Hylograph

derive instance eqTheme :: Eq Theme

themeName :: Theme -> String
themeName = case _ of
  Light -> "light"
  Dark -> "dark"
  Hylograph -> "hylograph"

parseTheme :: String -> Theme
parseTheme = case _ of
  "dark" -> Dark
  "hylograph" -> Hylograph
  _ -> Light

type Slots =
  ( accordion :: Accordion.Slot Unit
  , toggle :: Toggle.Slot Unit
  , stepper :: Stepper.Slot Unit
  , slider :: Slider.Slot Unit
  , segmented :: Segmented.Slot Unit
  , select :: Select.Slot Unit
  , themeSwitch :: Segmented.Slot Unit
  )

_accordion :: Proxy "accordion"
_accordion = Proxy

_toggle :: Proxy "toggle"
_toggle = Proxy

_stepper :: Proxy "stepper"
_stepper = Proxy

_slider :: Proxy "slider"
_slider = Proxy

_segmented :: Proxy "segmented"
_segmented = Proxy

_select :: Proxy "select"
_select = Proxy

_themeSwitch :: Proxy "themeSwitch"
_themeSwitch = Proxy

type State =
  { theme :: Theme
  , accordionOpen :: Boolean
  , toggleOn :: Boolean
  , stepper :: Int
  , slider :: Number
  , segment :: String
  , selected :: Maybe String
  , modalOpen :: Boolean
  , toastShown :: Boolean
  }

initialState :: State
initialState =
  { theme: Light
  , accordionOpen: true
  , toggleOn: true
  , stepper: 3
  , slider: 40.0
  , segment: "list"
  , selected: Nothing
  , modalOpen: false
  , toastShown: false
  }

data Action
  = Initialize
  | SetTheme Theme
  | AccToggled Boolean
  | TogChanged Boolean
  | StepChanged Int
  | SldChanged Number
  | SegSelected String
  | SelSelected String
  | OpenModal
  | CloseModal
  | ShowToast
  | HideToast

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    dark <- liftEffect prefersDark
    let t = if dark then Dark else Light
    H.modify_ _ { theme = t }
    liftEffect (setThemeAttr (themeName t))
  SetTheme t -> do
    H.modify_ _ { theme = t }
    liftEffect (setThemeAttr (themeName t))
  AccToggled o -> H.modify_ _ { accordionOpen = o }
  TogChanged v -> H.modify_ _ { toggleOn = v }
  StepChanged v -> H.modify_ _ { stepper = v }
  SldChanged v -> H.modify_ _ { slider = v }
  SegSelected k -> H.modify_ _ { segment = k }
  SelSelected v -> H.modify_ _ { selected = Just v }
  OpenModal -> H.modify_ _ { modalOpen = true }
  CloseModal -> H.modify_ _ { modalOpen = false }
  ShowToast -> H.modify_ _ { toastShown = true }
  HideToast -> H.modify_ _ { toastShown = false }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st =
  HH.div_
    [ styleTag
    , siteHeader st.theme
    , HH.div [ cls "page" ]
        [ navColumn
        , HH.main [ cls "main" ] (stories st)
        ]
    , modalLayer st
    ]

siteHeader :: forall m. MonadAff m => Theme -> H.ComponentHTML Action Slots m
siteHeader theme =
  HH.header [ cls "site-header" ]
    [ HH.div [ cls "site-header__row" ]
        [ HH.h1_ [ HH.text "Hylograph Halogen UI" ]
        -- the theme switcher is itself the library's SegmentedControl
        , HH.slot _themeSwitch unit Segmented.component
            ((Segmented.defaultInput
                [ { key: "light", label: "Light" }
                , { key: "dark", label: "Dark" }
                , { key: "hylograph", label: "Hylograph" }
                ]) { active = themeName theme })
            (\(Segmented.Selected k) -> SetTheme (parseTheme k))
        ]
    , HH.p_
        [ HH.text "Reusable Halogen widgets on one controlled component contract. \
                  \Every demo below is live — and this page (the parent) owns all of \
                  \their state, just as the contract says. Show, and tell." ]
    ]

navColumn :: forall m. H.ComponentHTML Action Slots m
navColumn =
  HH.nav [ cls "nav" ]
    [ HH.div [ cls "nav-group" ] [ HH.text "Leaf components" ]
    , navLink "accordion" "Accordion"
    , navLink "toggle" "Toggle"
    , navLink "stepper" "Stepper"
    , navLink "slider" "Slider"
    , navLink "segmented" "Segmented"
    , navLink "select" "Select"
    , HH.div [ cls "nav-group" ] [ HH.text "Chrome functions" ]
    , navLink "panel" "Panel"
    , navLink "field" "Field"
    , navLink "modal" "Modal"
    , navLink "toast" "Toast"
    ]

navLink :: forall m. String -> String -> H.ComponentHTML Action Slots m
navLink anchor label =
  HH.a [ HP.href ("#" <> anchor), cls "nav-link" ] [ HH.text label ]

-- | The show-and-tell frame: a titled section with the live demo and its code
-- | side by side.
story
  :: forall m
   . { anchor :: String, title :: String, tier :: String, blurb :: String, code :: String }
  -> H.ComponentHTML Action Slots m
  -> H.ComponentHTML Action Slots m
story meta demo =
  HH.section [ HP.id meta.anchor, cls "story" ]
    [ HH.div [ cls "story-head" ]
        [ HH.h2_ [ HH.text meta.title ]
        , HH.span [ cls "tier" ] [ HH.text meta.tier ]
        ]
    , HH.p [ cls "blurb" ] [ HH.text meta.blurb ]
    , HH.div [ cls "story-grid" ]
        [ HH.div [ cls "stage" ] [ demo ]
        , HH.pre [ cls "code" ] [ HH.code_ [ HH.text meta.code ] ]
        ]
    ]

btn :: forall w i. i -> String -> HH.HTML w i
btn act label =
  HH.button
    [ HE.onClick \_ -> act
    , sty "padding:7px 14px;border:1px solid #cfcabb;border-radius:7px;background:#fff;\
          \cursor:pointer;font:13px system-ui;color:#2b2b2b"
    ]
    [ HH.text label ]

stories :: forall m. MonadAff m => State -> Array (H.ComponentHTML Action Slots m)
stories st =
  [ story
      { anchor: "accordion", title: "Accordion", tier: "leaf · controlled-header"
      , blurb: "A controlled disclosure header with a self-debounced toggle. The parent owns `open` and renders the body itself."
      , code: accordionCode }
      ( HH.div [ sty "width:100%" ]
          [ HH.slot _accordion unit Accordion.component
              ((Accordion.defaultInput "DETAILS") { open = st.accordionOpen, sub = Just "click to fold" })
              (\(Accordion.Toggled o) -> AccToggled o)
          , if st.accordionOpen
              then HH.div [ sty "padding:12px 2px 0;color:#5a564b;font:13px/1.5 system-ui" ]
                     [ HH.text "The parent renders this body, gated on the open state it owns." ]
              else HH.text ""
          ]
      )
  , story
      { anchor: "toggle", title: "Toggle", tier: "leaf · controlled"
      , blurb: "The minimal instance of the contract — no ephemeral state at all. The parent owns `value`."
      , code: toggleCode }
      ( HH.slot _toggle unit Toggle.component
          ((Toggle.defaultInput st.toggleOn) { label = Just (if st.toggleOn then "Enabled" else "Disabled") })
          (\(Toggle.Changed v) -> TogChanged v)
      )
  , story
      { anchor: "stepper", title: "Stepper", tier: "leaf · controlled"
      , blurb: "A clamped integer stepper; the arrows disable at the bounds."
      , code: stepperCode }
      ( HH.slot _stepper unit Stepper.component
          ((Stepper.defaultInput st.stepper) { min = 0, max = 12 })
          (\(Stepper.Changed v) -> StepChanged v)
      )
  , story
      { anchor: "slider", title: "Slider", tier: "leaf · controlled · debounced"
      , blurb: "A range slider, debounced inside the widget because a drag floods input events. The value below updates as the parent honours each request."
      , code: sliderCode }
      ( HH.div [ sty "width:100%;display:flex;flex-direction:column;gap:10px" ]
          [ HH.slot _slider unit Slider.component
              ((Slider.defaultInput st.slider) { min = 0.0, max = 100.0 })
              (\(Slider.Changed v) -> SldChanged v)
          , HH.span [ sty "font:12px 'SF Mono',Menlo,monospace;color:#5a564b" ]
              [ HH.text ("value: " <> show st.slider) ]
          ]
      )
  , story
      { anchor: "segmented", title: "SegmentedControl", tier: "leaf · controlled-header"
      , blurb: "A tab/segment selector. The parent owns `active` and renders the corresponding pane — the control is only the selector."
      , code: segmentedCode }
      ( HH.div [ sty "display:flex;flex-direction:column;gap:14px;align-items:flex-start" ]
          [ HH.slot _segmented unit Segmented.component
              ((Segmented.defaultInput
                  [ { key: "list", label: "List" }
                  , { key: "grid", label: "Grid" }
                  , { key: "map", label: "Map" }
                  ]) { active = st.segment })
              (\(Segmented.Selected k) -> SegSelected k)
          , HH.div [ sty "font:13px system-ui;color:#5a564b" ]
              [ HH.text ("Parent renders pane: " <> st.segment) ]
          ]
      )
  , story
      { anchor: "select", title: "Select", tier: "leaf · controlled + ephemeral"
      , blurb: "A typeahead dropdown. `selected` is controlled (app-meaningful); `open` and the filter `query` are ephemeral interaction state the widget owns."
      , code: selectCode }
      ( HH.slot _select unit Select.component
          ((Select.defaultInput
              [ { value: "arbhar", label: "Arbhar" }
              , { value: "lubadh", label: "Lubadh" }
              , { value: "morphagene", label: "Morphagene" }
              , { value: "rample", label: "Rample" }
              ]) { selected = st.selected, searchable = true, placeholder = "Choose a module…" })
          (\(Select.Selected v) -> SelSelected v)
      )
  , story
      { anchor: "panel", title: "Panel", tier: "chrome function"
      , blurb: "A titled surface wrapping caller content. A render function, polymorphic in your action — so the body threads straight through."
      , code: panelCode }
      ( Panel.panel { title: "SOURCES", sub: Just "3 active" }
          [ HH.p [ sty "margin:0;font:13px/1.5 system-ui;color:#5a564b" ]
              [ HH.text "Any caller content sits inside the titled surface." ]
          ]
      )
  , story
      { anchor: "field", title: "Field", tier: "chrome function"
      , blurb: "A labelled form row: label, control, optional hint."
      , code: fieldCode }
      ( Field.field { label: "Threshold", hint: Just "0–100, applied live" }
          ( HH.input
              [ HP.value "42"
              , sty "padding:6px 10px;border:1px solid #00000022;border-radius:6px;font:13px system-ui;width:120px"
              ]
          )
      )
  , story
      { anchor: "modal", title: "Modal", tier: "chrome function"
      , blurb: "An overlay dialog. The body and the `onClose` action thread through the chrome function; it renders nothing when closed."
      , code: modalCode }
      ( btn OpenModal "Open dialog" )
  , story
      { anchor: "toast", title: "Toast", tier: "chrome function"
      , blurb: "A notification banner, coloured by `Variant`, with an optional dismiss that raises your action."
      , code: toastCode }
      ( HH.div [ sty "display:flex;flex-direction:column;gap:14px;align-items:flex-start" ]
          [ btn ShowToast "Show toast"
          , if st.toastShown
              then Toast.toast { variant: Toast.Success, message: "Saved.", onDismiss: Just HideToast }
              else HH.text ""
          ]
      )
  ]

-- | The modal lives at the page root (it is fixed-position chrome), driven by
-- | the same controlled state as everything else.
modalLayer :: forall m. State -> H.ComponentHTML Action Slots m
modalLayer st =
  Modal.modal { open: st.modalOpen, title: "Confirm", onClose: CloseModal }
    [ HH.p [ sty "margin:0 0 16px;font:14px/1.5 system-ui;color:#2b2b2b" ]
        [ HH.text "Caller-owned body content threads through the chrome function — including this button, which raises your action." ]
    , btn CloseModal "Close"
    ]

styleTag :: forall m. H.ComponentHTML Action Slots m
styleTag = HH.element (HH.ElemName "style") [] [ HH.text globalCss ]

--------------------------------------------------------------------------------
-- Code snippets (the "tell")
--------------------------------------------------------------------------------

accordionCode :: String
accordionCode =
  """-- parent owns `open`; widget emits a request, parent renders the body
HH.slot _accordion unit Accordion.component
  (Accordion.defaultInput "DETAILS")
    { open = state.accordionOpen, sub = Just "click to fold" }
  (\(Accordion.Toggled o) -> AccToggled o)
, if state.accordionOpen then bodyHtml else HH.text ""
"""

toggleCode :: String
toggleCode =
  """HH.slot _toggle unit Toggle.component
  (Toggle.defaultInput state.toggleOn)
    { label = Just "Enabled" }
  (\(Toggle.Changed v) -> TogChanged v)

-- handler:
TogChanged v -> H.modify_ _ { toggleOn = v }"""

stepperCode :: String
stepperCode =
  """HH.slot _stepper unit Stepper.component
  (Stepper.defaultInput state.stepper)
    { min = 0, max = 12 }
  (\(Stepper.Changed v) -> StepChanged v)"""

sliderCode :: String
sliderCode =
  """-- debounce lives inside the widget (default 80 ms)
HH.slot _slider unit Slider.component
  (Slider.defaultInput state.slider)
    { min = 0.0, max = 100.0 }
  (\(Slider.Changed v) -> SldChanged v)"""

segmentedCode :: String
segmentedCode =
  """-- selector only; the parent owns `active` AND renders the pane
HH.slot _segmented unit Segmented.component
  (Segmented.defaultInput
     [ { key: "list", label: "List" }
     , { key: "grid", label: "Grid" }
     , { key: "map",  label: "Map"  } ])
    { active = state.segment }
  (\(Segmented.Selected k) -> SegSelected k)"""

selectCode :: String
selectCode =
  """-- `selected` controlled; `open`/`query` are ephemeral, owned by the widget
HH.slot _select unit Select.component
  (Select.defaultInput modules)
    { selected = state.selected
    , searchable = true
    , placeholder = "Choose a module…" }
  (\(Select.Selected v) -> SelSelected v)"""

panelCode :: String
panelCode =
  """-- chrome function: polymorphic in your action `i`
Panel.panel { title: "SOURCES", sub: Just "3 active" }
  [ HH.p_ [ HH.text "any caller content" ] ]"""

fieldCode :: String
fieldCode =
  """Field.field { label: "Threshold", hint: Just "0–100" }
  ( HH.input [ HP.value "42" ] )"""

modalCode :: String
modalCode =
  """-- renders nothing when closed; body + onClose thread through
Modal.modal
  { open: state.modalOpen, title: "Confirm", onClose: CloseModal }
  [ HH.p_ [ HH.text "caller-owned body" ]
  , btn CloseModal "Close"
  ]"""

toastCode :: String
toastCode =
  """Toast.toast
  { variant: Toast.Success
  , message: "Saved."
  , onDismiss: Just HideToast
  }"""

--------------------------------------------------------------------------------
-- Page chrome styling (light Swiss). The widgets style themselves; this is just
-- the document around them.
--------------------------------------------------------------------------------

globalCss :: String
globalCss =
  """
* { box-sizing: border-box; }
body { margin: 0; background: var(--hg-page-bg); color: var(--hg-ink);
  font-family: var(--hg-font, system-ui,-apple-system,'Segoe UI',sans-serif); -webkit-font-smoothing: antialiased;
  transition: background 200ms ease, color 200ms ease; }
a { color: inherit; }
.site-header { max-width: 1100px; margin: 0 auto; padding: 52px 32px 36px; border-bottom: 1px solid var(--hg-line); }
.site-header__row { display: flex; align-items: center; justify-content: space-between; gap: 16px; }
.site-header h1 { margin: 0; font-size: 30px; font-weight: 700; letter-spacing: -0.01em; }
.site-header p { margin: 10px 0 0; max-width: 64ch; color: var(--hg-ink-soft); font-size: 15px; line-height: 1.55; }
.page { display: grid; grid-template-columns: 200px 1fr; gap: 32px; max-width: 1100px; margin: 0 auto; }
.nav { position: sticky; top: 0; align-self: start; padding: 40px 0 40px 32px; }
.nav-group { margin: 18px 0 8px; font-size: 11px; font-weight: 700; letter-spacing: 0.07em;
  text-transform: uppercase; color: var(--hg-ink-soft); }
.nav-group:first-child { margin-top: 0; }
.nav-link { display: block; padding: 4px 0; text-decoration: none; font-size: 14px; }
.nav-link:hover { color: var(--hg-accent); }
.main { padding: 40px 32px 140px; min-width: 0; }
.story { margin-bottom: 64px; scroll-margin-top: 24px; }
.story-head { display: flex; align-items: baseline; gap: 12px; }
.story-head h2 { margin: 0; font-size: 22px; font-weight: 600; }
.tier { font-size: 11px; font-weight: 600; letter-spacing: 0.04em; text-transform: uppercase; color: var(--hg-ink-soft); }
.blurb { margin: 8px 0 18px; max-width: 64ch; color: var(--hg-ink-soft); font-size: 15px; line-height: 1.55; }
.story-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 20px; align-items: start; }
.stage { display: flex; align-items: center; min-height: 96px; padding: 28px;
  background: var(--hg-surface); border: 1px solid var(--hg-line); border-radius: var(--hg-radius, 10px);
  transition: background 200ms ease, border-color 200ms ease; }
pre.code { margin: 0; padding: 16px 18px; overflow: auto; background: var(--hg-surface-alt);
  border: 1px solid var(--hg-line); border-radius: var(--hg-radius, 10px); color: var(--hg-ink);
  font-family: 'SF Mono',Menlo,monospace; font-size: 12px; line-height: 1.6; }
@media (max-width: 820px) {
  .page { grid-template-columns: 1fr; }
  .nav { position: static; padding: 0 32px; }
  .story-grid { grid-template-columns: 1fr; }
}
"""
