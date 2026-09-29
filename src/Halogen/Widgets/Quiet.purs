-- | Quiet controls: form controls that draw no chrome until they are touched.
-- |
-- | A value is plain text. Hover draws a dotted hairline under it, focus a solid
-- | one in the accent colour; a select shows its caret only on hover and focus.
-- | Numbers carry a small label beside them rather than a box around them, and
-- | row tools (✕, ▷) appear only where the pointer is.
-- |
-- | These are **view functions**, not components: a ledger has hundreds of
-- | editable cells, and one component per cell would mean a slot per cell. They
-- | still obey the controlled rule (CONTRACT.md): the parent passes every value
-- | in, and each control emits an intent (`onChange`, `onToggle`) that the
-- | parent may honour. There is no hidden state.
-- |
-- | Unlike the components, these need the stylesheet: the hover and focus
-- | reveals cannot be inline styles. Load `css/halogen-widgets.css` (the
-- | `hw-quiet-*` rules). Without it they degrade to plain native controls.
module Halogen.Widgets.Quiet
  ( Align(..)
  , InputConfig
  , number
  , text
  , SelectConfig
  , select
  , Option
  , ChooseConfig
  , choose
  , ToggleConfig
  , toggle
  , MarkConfig
  , mark
  , labelled
  , note
  , fault
  , ToolConfig
  , tool
  , KeyEntry
  , key
  ) where

import Prelude

import Data.Array (elem, null, (:))
import Data.Int (toNumber)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Widgets.Style (cls, clss, sty)

-- | Which side of its cell a value sits on. Numbers read best `End`-aligned,
-- | so their digits line up down a column; names read best `Start`-aligned.
data Align = Start | End

derive instance eqAlign :: Eq Align

-- | A quiet text field. `value` is text and `onChange` hands back text: the
-- | parent parses, so a half-typed number can be no edit rather than a zero.
-- |
-- | `width` is the number of characters the field is sized for; the control
-- | adds half a character of room for its caret. `label` is the accessible
-- | name, and the tooltip.
type InputConfig i =
  { value :: String
  , width :: Int
  , align :: Align
  , label :: String
  , onChange :: String -> i
  , disabled :: Boolean
  }

-- | A number: tabular figures, so a column of them lines up digit for digit.
number :: forall w i. InputConfig i -> HH.HTML w i
number = input "hw-quiet-number"

-- | A name, or any other text.
text :: forall w i. InputConfig i -> HH.HTML w i
text = input "hw-quiet-text"

input :: forall w i. String -> InputConfig i -> HH.HTML w i
input kind c =
  HH.input
    [ clss ([ "hw-quiet-input", kind ] <> alignClass c.align)
    , HP.type_ HP.InputText
    , HP.value c.value
    , HP.disabled c.disabled
    , HP.title c.label
    , HPA.label c.label
    , HP.attr (HH.AttrName "spellcheck") "false"
    , HP.attr (HH.AttrName "autocomplete") "off"
    , sty ("width:" <> show (toNumber c.width + 0.5) <> "ch")
    , HE.onValueInput c.onChange
    ]

alignClass :: Align -> Array String
alignClass = case _ of
  Start -> []
  End -> [ "hw-quiet-end" ]

-- | A native `<select>` with its box removed. The caret shows on hover and
-- | focus only. A `value` that is not among `options` is kept and shown rather
-- | than silently replaced by the first option: a choice that has since
-- | vanished (an unplugged port) should read as itself.
type SelectConfig i =
  { value :: String
  , options :: Array String
  , label :: String
  , onChange :: String -> i
  }

select :: forall w i. SelectConfig i -> HH.HTML w i
select c =
  HH.select
    [ cls "hw-quiet-select"
    , HP.title c.label
    , HPA.label c.label
    , HE.onValueChange c.onChange
    ]
    (map opt (if elem c.value c.options then c.options else c.value : c.options))
  where
  opt o = HH.option [ HP.value o, HP.selected (o == c.value) ] [ HH.text o ]

-- | One choice in a `choose` menu: what it raises, and what it says.
type Option = { value :: String, label :: String }

-- | A faint text line, such as "+ add", that opens a choice when clicked and
-- | raises the one chosen. It keeps no selection: it reads as its prompt
-- | again after every choice. Options come in named groups (shown as the
-- | menu's headings); a group with an empty name is shown without a heading.
type ChooseConfig i =
  { prompt :: String
  , groups :: Array { name :: String, options :: Array Option }
  , label :: String
  , onChoose :: String -> i
  }

choose :: forall w i. ChooseConfig i -> HH.HTML w i
choose c =
  HH.select
    [ cls "hw-quiet-choose"
    , HP.title c.label
    , HPA.label c.label
    -- Re-asserted on every render, so the line reads as its prompt again once
    -- a choice has been raised. (Halogen compares `value` with the element's
    -- live value, not the last one it rendered.)
    , HP.value ""
    , HE.onValueChange \v -> c.onChoose v
    ]
    ( HH.option [ HP.value "", HP.disabled true, HP.selected true ] [ HH.text c.prompt ]
        : map group c.groups
    )
  where
  group g
    | g.name == "" = HH.optgroup [] (map opt g.options)
    | otherwise = HH.optgroup [ HP.prop (HH.PropName "label") g.name ] (map opt g.options)
  opt o = HH.option [ HP.value o.value ] [ HH.text o.label ]

-- | A value that is on or off, shown as a word rather than a switch. `text` is
-- | what it reads as; off, it is greyed.
type ToggleConfig i =
  { on :: Boolean
  , text :: String
  , label :: String
  , onToggle :: i
  }

toggle :: forall w i. ToggleConfig i -> HH.HTML w i
toggle c =
  HH.button
    [ cls "hw-quiet-toggle"
    , HP.type_ HP.ButtonButton
    , HP.title c.label
    , HPA.label c.label
    , HPA.pressed (if c.on then "true" else "false")
    , HE.onClick \_ -> c.onToggle
    ]
    [ HH.text c.text ]

-- | A row's mark: the category that matters most, carried by hue, weight and
-- | position. A 4px swatch, the name in its colour, then the specific kind in
-- | muted mono. The mark is also the row's switch: filled is on, an outline is
-- | off, and clicking it asks for the other.
-- |
-- | `hue` is any CSS colour, typically a `var(--…)` the app defines, so the
-- | library knows nothing about the categories themselves.
type MarkConfig i =
  { name :: String
  , detail :: String
  , hue :: String
  , on :: Boolean
  , onToggle :: i
  }

mark :: forall w i. MarkConfig i -> HH.HTML w i
mark c =
  HH.button
    [ cls "hw-quiet-mark"
    , HP.type_ HP.ButtonButton
    , sty ("--hw-mark:" <> c.hue)
    , HPA.pressed (if c.on then "true" else "false")
    , HP.title (if c.on then "on: click to switch off" else "off: click to switch on")
    , HE.onClick \_ -> c.onToggle
    ]
    ( [ HH.span [ cls "hw-quiet-mark__swatch" ] []
      , HH.span [ cls "hw-quiet-mark__name" ] [ HH.text c.name ]
      ]
        <> (if c.detail == "" then [] else [ HH.span [ cls "hw-quiet-mark__detail" ] [ HH.text c.detail ] ])
    )

-- | A small label and the values it names, kept together on one line: e.g.
-- | `SLOT 0  36 C2`. The label is mono, small, uppercase and muted.
labelled :: forall w i. String -> Array (HH.HTML w i) -> HH.HTML w i
labelled label values =
  HH.span [ cls "hw-quiet-labelled" ]
    (HH.span [ cls "hw-quiet-label" ] [ HH.text label ] : values)

-- | A muted aside beside a value, such as the note name of a pitch.
note :: forall w i. String -> HH.HTML w i
note s = HH.span [ cls "hw-quiet-note" ] [ HH.text s ]

-- | What is wrong, in the danger colour. Show it only when something is: an
-- | "ok" on every row is noise.
fault :: forall w i. String -> HH.HTML w i
fault s = HH.span [ cls "hw-quiet-fault" ] [ HH.text s ]

-- | A row tool, such as ✕: hidden until its row is hovered or it has keyboard
-- | focus. Always shown on a device that cannot hover.
type ToolConfig i =
  { glyph :: String
  , label :: String
  , onClick :: i
  }

tool :: forall w i. ToolConfig i -> HH.HTML w i
tool c =
  HH.button
    [ cls "hw-quiet-tool"
    , HP.type_ HP.ButtonButton
    , HP.title c.label
    , HPA.label c.label
    , HE.onClick \_ -> c.onClick
    ]
    [ HH.text c.glyph ]

-- | One category in a `key`: its name, its hue (as for `mark`), and how many
-- | rows carry it.
type KeyEntry = { name :: String, hue :: String, count :: Int }

-- | The key to the marks: each category in its hue, with its count.
key :: forall w i. Array KeyEntry -> HH.HTML w i
key entries
  | null entries = HH.text ""
  | otherwise =
      HH.div [ cls "hw-quiet-key" ]
        (map entry entries)
      where
      entry e =
        HH.span [ cls "hw-quiet-key__entry", sty ("--hw-mark:" <> e.hue) ]
          [ HH.span [ cls "hw-quiet-key__swatch" ] []
          , HH.text e.name
          , HH.span [ cls "hw-quiet-key__count" ] [ HH.text (show e.count) ]
          ]
