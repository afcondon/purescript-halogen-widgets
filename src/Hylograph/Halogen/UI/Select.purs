-- | A controlled single-select dropdown, optionally typeahead-filtered.
-- |
-- | Two kinds of state, deliberately split (see CONTRACT.md):
-- |   * `selected` is CONTROLLED — it is app-meaningful, so the parent owns it
-- |     and `Selected` requests a change.
-- |   * `open` and `query` are EPHEMERAL interaction state with no app meaning,
-- |     so the widget owns them and never surfaces them.
module Hylograph.Halogen.UI.Select
  ( Option
  , Input
  , Output(..)
  , Query(..)
  , Slot
  , component
  , defaultInput
  ) where

import Prelude

import Data.Array (filter, find)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), contains)
import Data.String.Common (toLower)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Hylograph.Halogen.UI.Style (sty, cls, ink, inkSoft, accent, surface, surfaceAlt, line, uiFont)

type Option = { value :: String, label :: String }

type Input =
  { options :: Array Option
  , selected :: Maybe String
  , placeholder :: String
  , searchable :: Boolean
  , disabled :: Boolean
  }

defaultInput :: Array Option -> Input
defaultInput options =
  { options, selected: Nothing, placeholder: "Select…", searchable: false, disabled: false }

data Output = Selected String

data Query a = Set (Maybe String) a

type Slot = H.Slot Query Output

data Action
  = Receive Input
  | Toggle
  | Pick String
  | SetQuery String

type State =
  { input :: Input
  , open :: Boolean       -- ephemeral
  , query :: String       -- ephemeral
  }

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState: \input -> { input, open: false, query: "" }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ _ { input = input }              -- leaves open/query untouched
  Toggle -> do
    { input } <- H.get
    when (not input.disabled) (H.modify_ \s -> s { open = not s.open, query = "" })
  SetQuery q ->
    H.modify_ _ { query = q }
  Pick value -> do
    H.modify_ _ { open = false, query = "" }
    H.raise (Selected value)

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Set mValue a -> do
    case mValue of
      Just value -> H.raise (Selected value)
      Nothing -> pure unit
    pure (Just a)

-- Options matching the current typeahead query (all of them when not searchable).
visibleOptions :: State -> Array Option
visibleOptions { input, query } =
  if not input.searchable || query == "" then input.options
  else filter (\o -> contains (Pattern (toLower query)) (toLower o.label)) input.options

selectedLabel :: Input -> String
selectedLabel input =
  case input.selected of
    Nothing -> input.placeholder
    Just v -> maybe input.placeholder _.label (find (\o -> o.value == v) input.options)

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div
    [ cls "hg-select"
    , sty $ "position:relative;display:inline-block;min-width:180px;font-family:" <> uiFont
        <> ";" <> if st.input.disabled then "opacity:0.5" else ""
    ]
    ( [ control ] <> if st.open then [ panel ] else [] )
  where
  isPlaceholder = st.input.selected == Nothing

  control =
    HH.div
      [ cls "hg-select__control"
      , HE.onClick \_ -> Toggle
      , sty $ "display:flex;align-items:center;justify-content:space-between;gap:8px;"
          <> "padding:6px 10px;border:1px solid " <> line <> ";border-radius:var(--hg-radius,6px);"
          <> "background:" <> surface <> ";font-size:13px;"
          <> (if st.input.disabled then "cursor:default;" else "cursor:pointer;")
          <> "color:" <> (if isPlaceholder then inkSoft else ink)
      ]
      [ HH.span_ [ HH.text (selectedLabel st.input) ]
      , HH.span [ sty $ "color:" <> inkSoft <> ";font-size:11px" ] [ HH.text "▾" ]
      ]

  panel =
    HH.div
      [ cls "hg-select__panel"
      , sty $ "position:absolute;left:0;right:0;top:calc(100% + 4px);z-index:50;"
          <> "background:" <> surface <> ";border:1px solid " <> line <> ";border-radius:var(--hg-radius,6px);"
          <> "box-shadow:0 6px 20px #00000022;max-height:240px;overflow:auto"
      ]
      ( (if st.input.searchable then [ searchBox ] else [])
          <> map optionRow (visibleOptions st)
      )

  searchBox =
    HH.input
      [ cls "hg-select__search"
      , HP.value st.query
      , HP.placeholder "Filter…"
      , HE.onValueInput SetQuery
      , sty $ "width:100%;box-sizing:border-box;padding:7px 10px;border:0;border-bottom:1px solid "
          <> line <> ";font-size:12px;outline:none;font-family:" <> uiFont
      ]

  optionRow o =
    let chosen = st.input.selected == Just o.value
    in HH.div
         [ cls ("hg-select__option" <> if chosen then " is-selected" else "")
         , HE.onClick \_ -> Pick o.value
         , sty $ "padding:7px 10px;cursor:pointer;font-size:13px;"
             <> (if chosen
                   then "background:" <> surfaceAlt <> ";color:" <> accent <> ";font-weight:600"
                   else "color:" <> ink)
         ]
         [ HH.text o.label ]
