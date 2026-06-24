-- | The showcase root. It is the first real *consumer* of the library, and so
-- | it dogfoods the contract: this one component owns the state of every demo
-- | below and handles each widget's `Output` to update it. Show (the live
-- | widget) and tell (its code) sit side by side.
module Showcase (component) where

import Prelude

import Data.Array (find, mapMaybe, mapWithIndex)
import Data.Array as Array
import Data.Int as Int
import Data.String.CodeUnits as SCU
import Data.Foldable (traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.DOM.Element as Element
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (documentElement) as HTMLDocument
import Web.HTML.HTMLHtmlElement (toElement) as HTMLHtmlElement
import Web.HTML.Window (document, localStorage) as Window
import Web.Storage.Storage as Storage

import Hylograph.Halogen.UI.Style (sty, cls)
import Hylograph.Halogen.UI.VAccordion as VAccordion
import Hylograph.Halogen.UI.HAccordion as HAccordion
import Hylograph.Halogen.UI.Toggle as Toggle
import Hylograph.Halogen.UI.Stepper as Stepper
import Hylograph.Halogen.UI.Slider as Slider
import Hylograph.Halogen.UI.Knob as Knob
import Hylograph.Halogen.UI.DoubleKnob as DoubleKnob
import Hylograph.Halogen.UI.SegmentedControl as Segmented
import Hylograph.Halogen.UI.Select as Select
import Hylograph.Halogen.UI.Compare as Compare
import Hylograph.Halogen.UI.Modal as Modal
import Hylograph.Halogen.UI.Panel as Panel
import Hylograph.Halogen.UI.Field as Field
import Hylograph.Halogen.UI.Toast as Toast

import Sigil
  ( parseToRenderType
  , renderSignatureInto
  , renderDataDeclInto
  , renderTypeSynonymInto
  )

-- Set `data-theme` on <html> via typed web-dom (no hand-rolled FFI).
-- The CSS rule `:root:not([data-theme="light"])` under @media dark still
-- applies until this attribute is first set, so OS-dark users see dark by
-- default; clicking a theme locks in the explicit choice.
setThemeAttr :: String -> Effect Unit
setThemeAttr value = do
  win <- HTML.window
  doc <- Window.document win
  mRoot <- HTMLDocument.documentElement doc
  traverse_ (Element.setAttribute "data-theme" value <<< HTMLHtmlElement.toElement) mRoot

-- Theme persistence via typed web-storage (library FFI, no hand-rolled JS).
themeKey :: String
themeKey = "hg-showcase-theme"

saveTheme :: Theme -> Effect Unit
saveTheme t = do
  store <- Window.localStorage =<< HTML.window
  Storage.setItem themeKey (themeName t) store

-- `Nothing` means the user never picked one (leave OS-dark `@media` in charge).
loadTheme :: Effect (Maybe Theme)
loadTheme = do
  store <- Window.localStorage =<< HTML.window
  map (map parseTheme) (Storage.getItem themeKey store)

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
  ( accordion :: VAccordion.Slot String
  , accordionH :: HAccordion.Slot String
  , toggle :: Toggle.Slot Unit
  , stepper :: Stepper.Slot Unit
  , slider :: Slider.Slot Unit
  , knob :: Knob.Slot Unit
  , doubleKnob :: DoubleKnob.Slot Unit
  , segmented :: Segmented.Slot Unit
  , select :: Select.Slot Unit
  , compare :: Compare.Slot Unit
  , themeSwitch :: Segmented.Slot Unit
  )

_accordion :: Proxy "accordion"
_accordion = Proxy

_accordionH :: Proxy "accordionH"
_accordionH = Proxy

_toggle :: Proxy "toggle"
_toggle = Proxy

_stepper :: Proxy "stepper"
_stepper = Proxy

_slider :: Proxy "slider"
_slider = Proxy

_knob :: Proxy "knob"
_knob = Proxy

_doubleKnob :: Proxy "doubleKnob"
_doubleKnob = Proxy

_segmented :: Proxy "segmented"
_segmented = Proxy

_select :: Proxy "select"
_select = Proxy

_compare :: Proxy "compare"
_compare = Proxy

_themeSwitch :: Proxy "themeSwitch"
_themeSwitch = Proxy

type State =
  { theme :: Theme
  -- Vertical accordion: BITFIELD semantics — the set of collapsed panels
  -- (exactly Triggerfish's `collapsed`). Any subset may be folded.
  , vCollapsed :: Array String
  -- Horizontal accordion: RADIO semantics — the single open column.
  , accordionHOpen :: String
  , toggleOn :: Boolean
  , stepper :: Int
  , slider :: Number
  , knob :: Number
  , doubleOuter :: Number
  , doubleInner :: Number
  , segment :: String
  , comparePos :: Number
  , selected :: Maybe String
  , modalOpen :: Boolean
  , toastShown :: Boolean
  }

initialState :: State
initialState =
  { theme: Light
  , vCollapsed: [ "shape" ]
  , accordionHOpen: "sources"
  , toggleOn: true
  , stepper: 3
  , slider: 40.0
  , knob: 65.0
  , doubleOuter: 70.0
  , doubleInner: 30.0
  , segment: "list"
  , comparePos: 50.0
  , selected: Nothing
  , modalOpen: false
  , toastShown: false
  }

data Action
  = Initialize
  | SetTheme Theme
  | VAccToggle String Boolean
  | AccHSelect String
  | TogChanged Boolean
  | StepChanged Int
  | SldChanged Number
  | KnobChanged Number
  | DoubleOuterChanged Number
  | DoubleInnerChanged Number
  | SegSelected String
  | CompareMoved Number
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

-- Inject the Sigil contracts after the DOM settles. Fires twice — immediately
-- (next tick) and again at 80 ms — so a heavier-than-usual render can't lose
-- the race and leave the placeholders empty. `renderAllContracts` is
-- idempotent, so the second pass is free insurance, not double work that shows.
scheduleInject :: forall o m. MonadAff m => H.HalogenM State Action Slots o m Unit
scheduleInject = void $ H.fork do
  liftAff (delay (Milliseconds 0.0))
  liftEffect renderAllContracts
  liftAff (delay (Milliseconds 80.0))
  liftEffect renderAllContracts

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    -- Restore the last-picked theme so a reload keeps Hylograph (and its
    -- Sigil contracts). If nothing was ever picked, leave the `data-theme`
    -- attribute unset so OS-dark users still get dark via `@media`.
    saved <- liftEffect loadTheme
    case saved of
      Nothing -> pure unit
      Just t -> do
        H.modify_ _ { theme = t }
        liftEffect (setThemeAttr (themeName t))
        when (t == Hylograph) scheduleInject
  SetTheme t -> do
    H.modify_ _ { theme = t }
    liftEffect (setThemeAttr (themeName t))
    liftEffect (saveTheme t)
    when (t == Hylograph) scheduleInject
  -- BITFIELD: toggle just this panel's membership in the collapsed set.
  -- Every panel folds independently; any subset may be open.
  VAccToggle k wantOpen -> H.modify_ \s -> s
    { vCollapsed =
        if wantOpen then Array.filter (_ /= k) s.vCollapsed
        else if Array.elem k s.vCollapsed then s.vCollapsed else Array.snoc s.vCollapsed k
    }
  -- RADIO: one column open at a time. Clicking a folded spine opens it (folding
  -- the rest); clicking the open column's header is a no-op (one stays open).
  AccHSelect k -> H.modify_ _ { accordionHOpen = k }
  TogChanged v -> H.modify_ _ { toggleOn = v }
  StepChanged v -> H.modify_ _ { stepper = v }
  SldChanged v -> H.modify_ _ { slider = v }
  KnobChanged v -> H.modify_ _ { knob = v }
  DoubleOuterChanged v -> H.modify_ _ { doubleOuter = v }
  DoubleInnerChanged v -> H.modify_ _ { doubleInner = v }
  SegSelected k -> H.modify_ _ { segment = k }
  CompareMoved p -> H.modify_ _ { comparePos = p }
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
    , HH.p [ cls "site-header__links" ]
        [ HH.a
            [ HP.href "https://github.com/afcondon/purescript-hylograph-halogen-ui"
            , HP.target "_blank"
            , cls "src-link"
            ]
            [ HH.text "Source on GitHub ↗" ]
        ]
    ]

navColumn :: forall m. H.ComponentHTML Action Slots m
navColumn =
  HH.nav [ cls "nav" ]
    [ HH.div [ cls "nav-group" ] [ HH.text "Leaf components" ]
    , navLink "vaccordion" "VAccordion"
    , navLink "haccordion" "HAccordion"
    , navLink "toggle" "Toggle"
    , navLink "stepper" "Stepper"
    , navLink "slider" "Slider"
    , navLink "knob" "Knob"
    , navLink "doubleknob" "DoubleKnob"
    , navLink "segmented" "Segmented"
    , navLink "select" "Select"
    , navLink "compare" "Compare"
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
   . MonadAff m
  => State
  -> { anchor :: String, title :: String, tier :: String, blurb :: String, code :: String }
  -> H.ComponentHTML Action Slots m
  -> H.ComponentHTML Action Slots m
story st meta demo =
  HH.section [ HP.id meta.anchor, cls "story" ]
    ( [ HH.div [ cls "story-head" ]
          [ HH.h2_ [ HH.text meta.title ]
          , HH.span [ cls "tier" ] [ HH.text meta.tier ]
          ]
      , HH.p [ cls "blurb" ] [ HH.text meta.blurb ]
      -- Part 1, the hero: the live demo, bright and large.
      , storyPart true "Demo" [ HH.div [ cls "story-stage" ] [ demo ] ]
      ]
      -- Part 2 (Hylograph only): the Sigil-typeset contract, recessed.
      <> contractBlock st meta.anchor
      -- Part 3: the usage code, recessed, syntax-highlighted as native VDOM.
      <> [ storyPart false "Usage"
             [ HH.pre [ cls "code" ] [ HH.code_ (highlight meta.code) ] ]
         ]
    )

-- | One numbered part of a story. The ordinal is supplied by a CSS counter on
-- | `.story` (so it stays contiguous whether or not the contract part is
-- | present); `hero` marks the demo, which keeps the bright card while the
-- | reference parts (contract, usage) recede.
storyPart
  :: forall m
   . Boolean -> String -> Array (H.ComponentHTML Action Slots m)
  -> H.ComponentHTML Action Slots m
storyPart hero label content =
  HH.div [ cls ("story-part " <> if hero then "story-part--hero" else "story-part--ref") ]
    [ HH.div [ cls "story-part__head" ]
        [ HH.span [ cls "story-part__label" ] [ HH.text label ] ]
    , HH.div [ cls "story-part__body" ] content
    ]

-- | The contract part: only rendered in Hylograph mode. Always visible (no
-- | longer folded), so the Sigil placeholders are in the VDOM whenever the
-- | theme is Hylograph; `renderAllContracts` fills them on the theme switch.
contractBlock
  :: forall m
   . MonadAff m
  => State -> String -> Array (H.ComponentHTML Action Slots m)
contractBlock st slug = case st.theme, findContract slug of
  Hylograph, Just c -> [ storyPart false "Type contract" [ contractPlaceholders c ] ]
  _, _ -> []

-- | Simple-path PureScript syntax highlighting: a single forward character scan
-- | that classifies comments, strings, numbers, operators, keywords, and
-- | constructors into semantic spans, emitting native VDOM (no FFI, no string
-- | injection). Term variables and punctuation stay uncoloured so the
-- | highlighted tokens read as accents, not confetti; dots are left plain, so
-- | `HH.slot` reads as a quiet module then `slot`.
highlight :: forall w i. String -> Array (HH.HTML w i)
highlight src = go 0 []
  where
  len = SCU.length src
  sub a b = SCU.take (b - a) (SCU.drop a src)

  go :: Int -> Array (HH.HTML w i) -> Array (HH.HTML w i)
  go i acc
    | i >= len = acc
    | otherwise = case SCU.charAt i src of
        Nothing -> acc
        Just c
          | isSpace c ->
              let j = takeWhile isSpace (i + 1)
              in go j (acc <> [ HH.text (sub i j) ])
          | c == '-' && SCU.charAt (i + 1) src == Just '-' ->
              let j = takeWhile (\d -> d /= '\n') (i + 1)
              in go j (acc <> [ tok "tok-comment" (sub i j) ])
          | c == '"' ->
              let j = stringEnd (i + 1)
              in go j (acc <> [ tok "tok-string" (sub i j) ])
          | isDigit c ->
              let j = takeWhile isNumChar (i + 1)
              in go j (acc <> [ tok "tok-num" (sub i j) ])
          | isIdentStart c ->
              let j = takeWhile isIdentChar (i + 1)
              in go j (acc <> [ identTok c (sub i j) ])
          | isSymbolChar c ->
              let j = takeWhile isSymbolChar (i + 1)
              in go j (acc <> [ tok "tok-op" (sub i j) ])
          | otherwise -> go (i + 1) (acc <> [ HH.text (SCU.singleton c) ])

  takeWhile :: (Char -> Boolean) -> Int -> Int
  takeWhile p k
    | k >= len = len
    | otherwise = case SCU.charAt k src of
        Just d | p d -> takeWhile p (k + 1)
        _ -> k

  stringEnd :: Int -> Int
  stringEnd k
    | k >= len = len
    | otherwise = case SCU.charAt k src of
        Just '"' -> k + 1
        Just '\\' -> stringEnd (k + 2)
        Just _ -> stringEnd (k + 1)
        Nothing -> len

  identTok :: Char -> String -> HH.HTML w i
  identTok first s
    | s `Array.elem` keywords = tok "tok-kw" s
    | isUpper first = tok "tok-con" s
    | otherwise = HH.text s

tok :: forall w i. String -> String -> HH.HTML w i
tok klass s = HH.span [ cls klass ] [ HH.text s ]

keywords :: Array String
keywords =
  [ "module", "import", "where", "do", "ado", "let", "in", "case", "of"
  , "if", "then", "else", "data", "newtype", "type", "class", "instance"
  , "derive", "forall", "infixl", "infixr", "infix" ]

symbolChars :: Array Char
symbolChars =
  [ '=', '-', '>', '<', '+', '*', '/', '\\', '|', '&', ':', '#', '$', '!', '?', '^', '~', '%', '@' ]

isSpace :: Char -> Boolean
isSpace c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

isDigit :: Char -> Boolean
isDigit c = c >= '0' && c <= '9'

isUpper :: Char -> Boolean
isUpper c = c >= 'A' && c <= 'Z'

isLower :: Char -> Boolean
isLower c = c >= 'a' && c <= 'z'

isIdentStart :: Char -> Boolean
isIdentStart c = isUpper c || isLower c || c == '_'

isIdentChar :: Char -> Boolean
isIdentChar c = isIdentStart c || isDigit c || c == '\''

isNumChar :: Char -> Boolean
isNumChar c = isDigit c || c == '.'

isSymbolChar :: Char -> Boolean
isSymbolChar c = c `Array.elem` symbolChars

btn :: forall w i. i -> String -> HH.HTML w i
btn act label =
  HH.button
    [ HE.onClick \_ -> act
    , sty "padding:7px 14px;border:1px solid #cfcabb;border-radius:7px;background:#fff;\
          \cursor:pointer;font:13px system-ui;color:#2b2b2b"
    ]
    [ HH.text label ]

-- | Three stacked panels, each independently collapsible — BITFIELD semantics,
-- | the Triggerfish case. The parent keeps a `collapsed` set; toggling one
-- | panel touches only its own membership, so any subset may be open at once.
vAccordionDemo :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
vAccordionDemo st =
  HH.div [ sty "width:100%;display:flex;flex-direction:column;gap:4px" ]
    (map panel panels)
  where
  panels =
    [ { key: "generate", label: "GENERATE", sub: Just "3 sources", body: "Seed the voice: sample sources, density, and the spawn rate." }
    , { key: "shape",    label: "SHAPE",    sub: Nothing,          body: "Envelope, grain size, and the spectral tilt of each grain." }
    , { key: "output",   label: "OUTPUT",   sub: Just "ES-9",      body: "Bus assignment, gain, and the send to the modular." }
    ]
  panel p =
    let open = not (Array.elem p.key st.vCollapsed) in
    HH.div [ sty "width:100%" ]
      [ HH.slot _accordion p.key VAccordion.component
          ((VAccordion.defaultInput p.label) { open = open, sub = p.sub })
          (\(VAccordion.Toggled o) -> VAccToggle p.key o)
      , if open
          then HH.div [ sty "padding:8px 2px 14px;color:#5a564b;font:13px/1.6 system-ui" ]
                 [ HH.text p.body ]
          else HH.text ""
      ]

-- | Three columns side-by-side, exactly one open. The folded columns are thin
-- | rotated spines; clicking a spine opens that column and folds the rest.
-- | Clicking the open column's header is a no-op (one is always open). Three
-- | (not two) so the multi-panel "one open, the others spined" shape reads at a
-- | glance. This is the Triggerfish layout in miniature.
hAccordionDemo :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
hAccordionDemo st =
  HH.div [ sty "display:flex;gap:8px;height:150px;align-items:stretch" ]
    (map col columns)
  where
  columns =
    [ { key: "sources", label: "SOURCES", body: "Arbhar · Lubadh · Morphagene — three sample sources feeding the mix." }
    , { key: "routing", label: "ROUTING", body: "Each source patched to a stereo bus, with send levels and mutes." }
    , { key: "output",  label: "OUTPUT",  body: "Master bus out to the ES-9: gain, tone, and the final limiter." }
    ]
  col c =
    let open = st.accordionHOpen == c.key in
    HH.div
      [ sty $ "display:flex;flex-direction:column;border:1px solid #d8d3c4;border-radius:6px;\
              \overflow:hidden;background:#fbfaf6;" <> (if open then "flex:1 1 auto" else "flex:0 0 32px") ]
      [ HH.slot _accordionH c.key HAccordion.component
          ((HAccordion.defaultInput c.label) { open = open })
          (\_ -> AccHSelect c.key)
      , if open
          then HH.div [ sty "padding:14px;color:#5a564b;font:13px/1.6 system-ui;flex:1" ]
                 [ HH.text c.body ]
          else HH.text ""
      ]

-- | A static faux settings panel, rendered two ways for the Compare demo:
-- | `hyper = false` is generic-startup vanilla; `hyper = true` is the
-- | Hylograph treatment. Inert PlainHTML (no actions) — exactly what a
-- | comparison wipe wants on each side.
comparePanel :: Boolean -> HH.PlainHTML
comparePanel hyper =
  let
    bg     = if hyper then "#f1ede2" else "#e8e8e8"
    card   = if hyper then "#faf7ef" else "#ffffff"
    ink    = if hyper then "#16140f" else "#333333"
    soft   = if hyper then "#6f6857" else "#8a8a8a"
    accent = if hyper then "#d6442b" else "#5b8def"
    font   = if hyper then "'Helvetica Neue',Helvetica,Arial,sans-serif" else "Georgia,'Times New Roman',serif"
    rad    = if hyper then "2px" else "8px"
    ls     = if hyper then "0.08em" else "0"
    row label val frac =
      HH.div [ sty "display:flex;align-items:center;gap:12px;margin-bottom:13px" ]
        [ HH.div [ sty $ "width:48px;font-size:12px;color:" <> soft ] [ HH.text label ]
        , HH.div [ sty $ "flex:1;height:6px;border-radius:99px;background:" <> card
                     <> ";border:1px solid rgba(0,0,0,0.08);position:relative;overflow:hidden" ]
            [ HH.div [ sty $ "position:absolute;top:0;left:0;bottom:0;width:" <> show (frac * 100.0)
                         <> "%;background:" <> accent ] [] ]
        , HH.div [ sty $ "width:36px;text-align:right;font-size:11px;color:" <> ink ] [ HH.text val ]
        ]
  in
  HH.div
    [ sty $ "width:100%;height:100%;box-sizing:border-box;padding:26px 28px;"
        <> "background:" <> bg <> ";color:" <> ink <> ";font-family:" <> font ]
    [ HH.div [ sty $ "font-size:13px;font-weight:700;letter-spacing:" <> ls <> ";margin-bottom:18px" ]
        [ HH.text (if hyper then "OUTPUT ROUTING" else "Output Settings") ]
    , row "Gain" "72%" 0.72
    , row "Tone" "40%" 0.40
    , row "Mix" "55%" 0.55
    , HH.div
        [ sty $ "margin-top:16px;display:inline-block;padding:7px 16px;border-radius:" <> rad
            <> ";background:" <> accent <> ";color:#fff;font-size:12px;font-weight:600;letter-spacing:" <> ls ]
        [ HH.text (if hyper then "APPLY" else "Apply") ]
    ]

stories :: forall m. MonadAff m => State -> Array (H.ComponentHTML Action Slots m)
stories st =
  [ story st
      { anchor: "vaccordion", title: "VAccordion", tier: "leaf · controlled-header"
      , blurb: "The vertical accordion: panels stack as rows; collapsing flips the chevron ▾→▸ and hides the parent's body. Shown here with **bitfield** semantics — the parent keeps a `collapsed` set, so each panel folds independently and any subset can be open (exactly Triggerfish). The widget is agnostic; it only emits `Toggled`."
      , code: vAccordionCode }
      ( vAccordionDemo st )
  , story st
      { anchor: "haccordion", title: "HAccordion", tier: "leaf · controlled-header"
      , blurb: "The horizontal accordion: panels sit side by side as columns, the folded ones thin rotated spines (the Triggerfish layout). Shown here with **radio** semantics — the parent keeps one open key, so opening a column folds the rest. Same widget, same `Toggled` output as VAccordion; only the parent's handler differs."
      , code: hAccordionCode }
      ( hAccordionDemo st )
  , story st
      { anchor: "toggle", title: "Toggle", tier: "leaf · controlled"
      , blurb: "The minimal instance of the contract — no ephemeral state at all. The parent owns `value`."
      , code: toggleCode }
      ( HH.slot _toggle unit Toggle.component
          ((Toggle.defaultInput st.toggleOn) { label = Just (if st.toggleOn then "Enabled" else "Disabled") })
          (\(Toggle.Changed v) -> TogChanged v)
      )
  , story st
      { anchor: "stepper", title: "Stepper", tier: "leaf · controlled"
      , blurb: "A clamped integer stepper; the arrows disable at the bounds."
      , code: stepperCode }
      ( HH.slot _stepper unit Stepper.component
          ((Stepper.defaultInput st.stepper) { min = 0, max = 12 })
          (\(Stepper.Changed v) -> StepChanged v)
      )
  , story st
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
  , story st
      { anchor: "knob", title: "Knob", tier: "leaf · controlled · debounced · SVG"
      , blurb: "A rotary knob — vertical drag changes the value (140 px = full range). Self-debounced. Geometry ported from producing-with-your-feet's Donut via Triggerfish; pure SVG, no chart library."
      , code: knobCode }
      ( HH.div [ sty "display:flex;align-items:center;gap:20px" ]
          [ HH.slot _knob unit Knob.component
              ((Knob.defaultInput st.knob)
                { min = 0.0, max = 100.0, ticks = 0
                , label = Just "GAIN"
                })
              (\(Knob.Changed v) -> KnobChanged v)
          , HH.span [ sty "font:12px 'SF Mono',Menlo,monospace;color:#5a564b" ]
              [ HH.text ("value: " <> show (Int.round st.knob)) ]
          ]
      )
  , story st
      { anchor: "doubleknob", title: "DoubleKnob", tier: "leaf · controlled · two layers · SVG"
      , blurb: "The Strymon / Chase Bliss pattern: one physical knob hosts two parameters. Outer ring + inner ring, each with its own drag and its own emitted value. `Output` is a sum tagged by layer."
      , code: doubleKnobCode }
      ( HH.div [ sty "display:flex;align-items:center;gap:24px" ]
          [ HH.slot _doubleKnob unit DoubleKnob.component
              ((DoubleKnob.defaultInput st.doubleOuter st.doubleInner)
                { label = Just "RATE  ·  DEPTH" })
              ( case _ of
                  DoubleKnob.OuterChanged v -> DoubleOuterChanged v
                  DoubleKnob.InnerChanged v -> DoubleInnerChanged v
              )
          , HH.span [ sty "font:12px 'SF Mono',Menlo,monospace;color:#5a564b" ]
              [ HH.text $ "rate: " <> show (Int.round st.doubleOuter)
                  <> "   depth: " <> show (Int.round st.doubleInner) ]
          ]
      )
  , story st
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
  , story st
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
  , story st
      { anchor: "compare", title: "Compare", tier: "leaf · controlled"
      , blurb: "A before/after comparison wipe. The two layers are static `PlainHTML` — comparing renderings, not interacting through them — which is what lets it be a leaf component: the widget owns the drag, the layers arrive inert. Drag the handle."
      , code: compareCode }
      ( HH.slot _compare unit Compare.component
          ((Compare.defaultInput (comparePanel false) (comparePanel true))
            { position = st.comparePos
            , height = "240px"
            , beforeLabel = Just "Vanilla"
            , afterLabel = Just "Hylograph"
            })
          (\(Compare.Moved p) -> CompareMoved p)
      )
  , story st
      { anchor: "panel", title: "Panel", tier: "chrome function"
      , blurb: "A titled surface wrapping caller content. A render function, polymorphic in your action — so the body threads straight through."
      , code: panelCode }
      ( Panel.panel { title: "SOURCES", sub: Just "3 active" }
          [ HH.p [ sty "margin:0;font:13px/1.5 system-ui;color:#5a564b" ]
              [ HH.text "Any caller content sits inside the titled surface." ]
          ]
      )
  , story st
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
  , story st
      { anchor: "modal", title: "Modal", tier: "chrome function"
      , blurb: "An overlay dialog. The body and the `onClose` action thread through the chrome function; it renders nothing when closed."
      , code: modalCode }
      ( btn OpenModal "Open dialog" )
  , story st
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
-- Hylograph-mode refinement: each widget's typed contract surface, typeset by
-- Sigil. Placeholder divs live in the VDOM only when theme == Hylograph;
-- Sigil's _renderInto fills them via querySelector after the next paint.
--------------------------------------------------------------------------------

-- A piece of a widget's typed surface, in a form Sigil can render.
data Fragment
  = TypeSyn String String                                            -- name, body type
  | DataDecl String (Array { name :: String, args :: Array String }) -- name, ctors
  | Signature String String                                           -- name, type

type Contract = { slug :: String, fragments :: Array Fragment }

-- One contract per widget. The slug matches the story's anchor.
allContracts :: Array Contract
allContracts =
  [ { slug: "vaccordion"
    , fragments:
        [ TypeSyn "Input" "Record ( open :: Boolean, label :: String, sub :: Maybe String, debounce :: Milliseconds, disabled :: Boolean )"
        , DataDecl "Output" [ { name: "Toggled", args: [ "Boolean" ] } ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "haccordion"
    , fragments:
        [ TypeSyn "Input" "Record ( open :: Boolean, label :: String, sub :: Maybe String, debounce :: Milliseconds, disabled :: Boolean )"
        , DataDecl "Output" [ { name: "Toggled", args: [ "Boolean" ] } ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "toggle"
    , fragments:
        [ TypeSyn "Input" "Record ( value :: Boolean, label :: Maybe String, disabled :: Boolean )"
        , DataDecl "Output" [ { name: "Changed", args: [ "Boolean" ] } ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "stepper"
    , fragments:
        [ TypeSyn "Input" "Record ( value :: Int, min :: Int, max :: Int, step :: Int, disabled :: Boolean )"
        , DataDecl "Output" [ { name: "Changed", args: [ "Int" ] } ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "slider"
    , fragments:
        [ TypeSyn "Input" "Record ( value :: Number, min :: Number, max :: Number, step :: Number, debounce :: Milliseconds, disabled :: Boolean )"
        , DataDecl "Output" [ { name: "Changed", args: [ "Number" ] } ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "knob"
    , fragments:
        [ TypeSyn "Input" "Record ( value :: Number, min :: Number, max :: Number, size :: Number, color :: String, label :: Maybe String, ticks :: Int, debounce :: Milliseconds, disabled :: Boolean )"
        , DataDecl "Output" [ { name: "Changed", args: [ "Number" ] } ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "doubleknob"
    , fragments:
        [ TypeSyn "Layer" "Record ( value :: Number, min :: Number, max :: Number, color :: String )"
        , TypeSyn "Input" "Record ( outer :: Layer, inner :: Layer, size :: Number, label :: Maybe String, debounce :: Milliseconds, disabled :: Boolean )"
        , DataDecl "Output"
            [ { name: "OuterChanged", args: [ "Number" ] }
            , { name: "InnerChanged", args: [ "Number" ] }
            ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "segmented"
    , fragments:
        [ TypeSyn "Segment" "Record ( key :: String, label :: String )"
        , TypeSyn "Input" "Record ( segments :: Array Segment, active :: String, disabled :: Boolean )"
        , DataDecl "Output" [ { name: "Selected", args: [ "String" ] } ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "select"
    , fragments:
        [ TypeSyn "Option" "Record ( value :: String, label :: String )"
        , TypeSyn "Input" "Record ( options :: Array Option, selected :: Maybe String, placeholder :: String, searchable :: Boolean, disabled :: Boolean )"
        , DataDecl "Output" [ { name: "Selected", args: [ "String" ] } ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "compare"
    , fragments:
        [ TypeSyn "Input" "Record ( position :: Number, before :: PlainHTML, after :: PlainHTML, height :: String, beforeLabel :: Maybe String, afterLabel :: Maybe String, disabled :: Boolean )"
        , DataDecl "Output" [ { name: "Moved", args: [ "Number" ] } ]
        , Signature "component" "forall m. MonadAff m => Component Query Input Output m"
        ]
    }
  , { slug: "panel"
    , fragments:
        [ TypeSyn "PanelConfig" "Record ( title :: String, sub :: Maybe String )"
        , Signature "panel" "forall w i. PanelConfig -> Array (HTML w i) -> HTML w i"
        ]
    }
  , { slug: "field"
    , fragments:
        [ TypeSyn "FieldConfig" "Record ( label :: String, hint :: Maybe String )"
        , Signature "field" "forall w i. FieldConfig -> HTML w i -> HTML w i"
        ]
    }
  , { slug: "modal"
    , fragments:
        [ TypeSyn "ModalConfig" "forall i. Record ( open :: Boolean, title :: String, onClose :: i )"
        , Signature "modal" "forall w i. ModalConfig i -> Array (HTML w i) -> HTML w i"
        ]
    }
  , { slug: "toast"
    , fragments:
        [ DataDecl "Variant"
            [ { name: "Info", args: [] }
            , { name: "Success", args: [] }
            , { name: "Warning", args: [] }
            , { name: "Error", args: [] }
            ]
        , TypeSyn "ToastConfig" "forall i. Record ( variant :: Variant, message :: String, onDismiss :: Maybe i )"
        , Signature "toast" "forall w i. ToastConfig i -> HTML w i"
        ]
    }
  ]

-- VDOM: one placeholder div per fragment, ID-stamped by slug + index.
contractPlaceholders :: forall m. Contract -> H.ComponentHTML Action Slots m
contractPlaceholders c =
  HH.div [ cls "story-contract" ]
    ( mapWithIndex
        (\i _ ->
          HH.div
            [ HP.id ("sig-" <> c.slug <> "-" <> show i)
            , cls "story-contract__frag"
            ]
            [])
        c.fragments
    )

-- Inject Sigil markup into each placeholder for one widget.
injectContract :: Contract -> Effect Unit
injectContract c = forWithIndex_ c.fragments \i frag ->
  let selector = "#sig-" <> c.slug <> "-" <> show i in
  case frag of
    TypeSyn name body -> case parseToRenderType body of
      Just ast -> renderTypeSynonymInto selector
        { name, typeParams: [], body: ast }
      Nothing -> pure unit
    DataDecl name ctors ->
      let
        parsedCtors = ctors <#> \c' ->
          { name: c'.name, args: mapMaybe parseToRenderType c'.args }
      in
      renderDataDeclInto selector
        { name, typeParams: [], constructors: parsedCtors, keyword: Nothing }
    Signature name sig -> case parseToRenderType sig of
      Just ast -> renderSignatureInto selector
        { name, ast, typeParams: [], className: Nothing }
      Nothing -> pure unit

renderAllContracts :: Effect Unit
renderAllContracts = traverse_ injectContract allContracts

-- Look up a contract by its slug (the same string the story uses as its anchor).
findContract :: String -> Maybe Contract
findContract slug = find (\c -> c.slug == slug) allContracts

--------------------------------------------------------------------------------
-- Code snippets (the "tell")
--------------------------------------------------------------------------------

vAccordionCode :: String
vAccordionCode =
  """-- BITFIELD: `collapsed` is a Set/Array; each panel folds independently.
panel p = HH.slot _accordion p.key VAccordion.component
  ((VAccordion.defaultInput p.label)
     { open = not (elem p.key state.collapsed) })
  (\(VAccordion.Toggled o) -> VAccToggle p.key o)

VAccToggle k wantOpen ->                       -- the handler decides semantics
  modify_ \s -> s { collapsed =
    if wantOpen then filter (_ /= k) s.collapsed else snoc s.collapsed k }
"""

hAccordionCode :: String
hAccordionCode =
  """-- RADIO: `open` is a single key; opening a column folds the rest.
column c = HH.slot _accordionH c.key HAccordion.component
  ((HAccordion.defaultInput c.label)
     { open = state.openColumn == c.key })
  (\_ -> AccHSelect c.key)

AccHSelect k -> modify_ _ { openColumn = k }   -- same widget; different handler
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

knobCode :: String
knobCode =
  """-- vertical drag changes the value (140 px = full range), self-debounced
HH.slot _knob unit Knob.component
  (Knob.defaultInput state.knob)
    { min = 0.0, max = 100.0
    , label = Just "GAIN"
    , ticks = 0          -- > 1 draws detent marks
    }
  (\(Knob.Changed v) -> KnobChanged v)"""

doubleKnobCode :: String
doubleKnobCode =
  """-- one widget, two parameters; Output tagged by layer
HH.slot _doubleKnob unit DoubleKnob.component
  (DoubleKnob.defaultInput state.rate state.depth)
    { label = Just "RATE · DEPTH" }
  ( case _ of
      DoubleKnob.OuterChanged v -> DoubleOuterChanged v
      DoubleKnob.InnerChanged v -> DoubleInnerChanged v
  )"""

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

compareCode :: String
compareCode =
  """-- the two layers are static PlainHTML; the parent owns the divider
HH.slot _compare unit Compare.component
  (Compare.defaultInput beforeHtml afterHtml)
    { position = state.comparePos
    , beforeLabel = Just "Vanilla"
    , afterLabel = Just "Hylograph" }
  (\(Compare.Moved p) -> CompareMoved p)"""

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
.site-header__links { margin-top: 14px !important; }
.src-link { display: inline-block; font-size: 13px; font-weight: 600; letter-spacing: 0.02em;
  text-decoration: none; color: var(--hg-accent);
  border: 1px solid var(--hg-line); border-radius: var(--hg-radius, 6px); padding: 5px 12px;
  transition: background 150ms ease, border-color 150ms ease; }
.src-link:hover { border-color: var(--hg-accent); background: color-mix(in srgb, var(--hg-accent) 8%, transparent); }
.page { display: grid; grid-template-columns: 200px 1fr; gap: 32px; max-width: 1100px; margin: 0 auto; }
.nav { position: sticky; top: 0; align-self: start; padding: 40px 0 40px 32px; }
.nav-group { margin: 18px 0 8px; font-size: 11px; font-weight: 700; letter-spacing: 0.07em;
  text-transform: uppercase; color: var(--hg-ink-soft); }
.nav-group:first-child { margin-top: 0; }
.nav-link { display: block; padding: 4px 0; text-decoration: none; font-size: 14px; }
.nav-link:hover { color: var(--hg-accent); }
.main { padding: 40px 32px 140px; min-width: 0; }
.story { margin-bottom: 72px; scroll-margin-top: 24px; counter-reset: part; }
.story-head { display: flex; align-items: baseline; gap: 12px; }
.story-head h2 { margin: 0; font-size: 22px; font-weight: 600; }
.tier { font-size: 11px; font-weight: 600; letter-spacing: 0.04em; text-transform: uppercase; color: var(--hg-ink-soft); }
.blurb { margin: 8px 0 22px; max-width: 64ch; color: var(--hg-ink-soft); font-size: 15px; line-height: 1.55; }

/* Numbered parts. A counter on `.story` numbers them contiguously, so the
 * Hylograph-only contract part never leaves a gap (light/dark read 01/02,
 * Hylograph reads 01/02/03). The hero (demo) keeps the bright card; the
 * reference parts (contract, usage) recede onto the page ground. */
.story-part { margin: 22px 0 0; }
.story-part__head { display: flex; align-items: baseline; gap: 10px; margin: 0 0 12px; }
.story-part__head::before {
  counter-increment: part;
  content: counter(part, decimal-leading-zero);
  font-family: 'JetBrains Mono', 'Fira Code', 'SF Mono', Menlo, monospace;
  font-size: 11px; font-weight: 600; color: var(--hg-accent); letter-spacing: 0.04em; }
.story-part__label { font-size: 11px; letter-spacing: 0.09em; text-transform: uppercase;
  color: var(--hg-ink-soft); font-weight: 600; }

/* Hero: the live demo gets prominence — wide, centred, generous breath. */
.story-stage { display: flex; align-items: center; justify-content: center;
  min-height: 160px; padding: 48px 32px;
  background: var(--hg-surface); border: 1px solid var(--hg-line);
  border-radius: var(--hg-radius, 12px);
  transition: background 200ms ease, border-color 200ms ease; }

/* Reference parts recede: quiet inset on the page ground, hairline, no card. */
.story-part--ref .story-part__body {
  padding: 16px 20px;
  background: var(--hg-surface-alt); border: 1px solid var(--hg-line);
  border-radius: var(--hg-radius, 8px); }
.story-contract { display: flex; flex-direction: column; gap: 10px; }
.story-contract__frag { font-size: 14px; }
.story-contract__frag:empty { display: none; }

/* Usage code, syntax-highlighted as native VDOM (no FFI). The token classes are
 * accents on otherwise-uncoloured text — keywords, constructors, strings,
 * numbers, operators; term variables and punctuation stay plain. */
pre.code { margin: 0; overflow: auto; color: var(--hg-ink);
  font-family: 'JetBrains Mono', 'Fira Code', 'SF Mono', Menlo, monospace;
  font-feature-settings: "calt" 1, "liga" 1, "ss01" 1, "ss02" 1;
  font-variant-ligatures: contextual common-ligatures;
  font-size: 12.5px; line-height: 1.7; }
pre.code .tok-comment { color: var(--hg-ink-soft); font-style: italic; opacity: 0.85; }
pre.code .tok-kw { color: var(--hg-accent); font-weight: 600; }
pre.code .tok-con { color: #3f9d6f; }
pre.code .tok-string { color: #c98a4b; }
pre.code .tok-num { color: #4a90c2; }
pre.code .tok-op { color: var(--hg-ink-soft); }

@media (max-width: 820px) {
  .page { grid-template-columns: 1fr; }
  .nav { position: static; padding: 0 32px; }
}
"""
