-- | A ledger: rows of every kind on one grid, so that they line up.
-- |
-- | Fixed column tracks, one continuous hairline under the column heads (empty
-- | heads carry it too), and no other rules: groups are separated by space.
-- | Rows are `display: contents`, so every cell sits on the ledger's one grid
-- | and a column means the same thing on every row. When the ledger is too
-- | narrow for its tracks it scrolls sideways inside its own container; the page
-- | never does.
-- |
-- | A view function, like `Halogen.Widgets.Quiet`: the parent owns every value in
-- | every cell, and the cells are whatever HTML the parent passes (typically
-- | quiet controls). Needs the `hw-ledger-*` rules in `css/halogen-widgets.css`.
module Halogen.Widgets.Ledger
  ( Column
  , LedgerConfig
  , Row(..)
  , ledger
  , values
  , name
  ) where

import Prelude
import Prim hiding (Row)

import Data.Array (index, length, mapWithIndex, replicate, span, (:))
import Data.Maybe (maybe)
import Data.String.Common (joinWith)
import Halogen.HTML as HH
import Halogen.Widgets.Quiet (Align(..))
import Halogen.Widgets.Style (cls, clss, sty)

-- | One column: its head (may be `""`, and still carries the rule), its CSS
-- | grid track (`"11em"`, `"minmax(16em, 1fr)"`), and which side its cells sit.
type Column = { head :: String, track :: String, align :: Align }

-- | The columns, and the width below which the ledger scrolls rather than
-- | squeezes (any CSS length, e.g. `"860px"`).
type LedgerConfig = { columns :: Array Column, minWidth :: String }

-- | A row of the ledger.
-- |
-- | - `Heading`: a full-width group heading. A ledger that opens with headings
-- |   draws them above the column heads, so the first group's name sits over
-- |   the whole table rather than under its rule.
-- | - `Entry`: one cell per column (missing cells are left empty). `first`
-- |   adds the space above a new group; `off` greys the row in place.
-- | - `Add`: a faint line, such as "+ add", spanning from column `from`
-- |   (0-based) to the end.
data Row w i
  = Heading (HH.HTML w i)
  | Entry { first :: Boolean, off :: Boolean, cells :: Array (HH.HTML w i) }
  | Add { from :: Int, content :: HH.HTML w i }

ledger :: forall w i. LedgerConfig -> Array (Row w i) -> HH.HTML w i
ledger c rows =
  HH.div [ cls "hw-ledger-scroll" ]
    [ HH.div
        [ cls "hw-ledger"
        , sty ("grid-template-columns:" <> joinWith " " (map _.track c.columns) <> ";min-width:" <> c.minWidth)
        ]
        (map row leading.init <> heads <> map row leading.rest)
    ]
  where
  leading = span isHeading rows
  ncols = length c.columns
  heads = mapWithIndex head c.columns
  head ix col =
    HH.div
      [ clss ([ "hw-ledger-head" ] <> endClass col.align <> (if ix == ncols - 1 then [ "hw-ledger-head--last" ] else [])) ]
      [ HH.text col.head ]
  row = case _ of
    Heading h -> HH.div [ cls "hw-ledger-heading" ] [ h ]
    Entry e ->
      HH.div
        [ clss ([ "hw-ledger-row" ] <> (if e.first then [ "hw-ledger-row--first" ] else []) <> (if e.off then [ "hw-ledger-row--off" ] else [])) ]
        (mapWithIndex cell (e.cells <> replicate (ncols - length e.cells) (HH.text "")))
    Add a ->
      HH.div
        [ cls "hw-ledger-add", sty ("grid-column:" <> show (a.from + 1) <> " / -1") ]
        [ a.content ]
  cell ix content =
    HH.div [ clss ([ "hw-ledger-cell" ] <> endClass (alignOf ix)) ] [ content ]
  alignOf ix = maybe Start _.align (index c.columns ix)
  endClass = case _ of
    Start -> []
    End -> [ "hw-ledger-end" ]

isHeading :: forall w i. Row w i -> Boolean
isHeading = case _ of
  Heading _ -> true
  _ -> false

-- | The varying column's content: labelled values that wrap as a group, each
-- | kept whole (see `Quiet.labelled`).
values :: forall w i. Array (HH.HTML w i) -> HH.HTML w i
values = HH.span [ cls "hw-ledger-values" ]

-- | A row's name, with an optional small line beneath it (`sub`, which may be
-- | `""`). For the first cell of a group's first row.
name :: forall w i. { name :: String, sub :: String } -> HH.HTML w i
name n =
  HH.span [ cls "hw-ledger-name" ]
    ( HH.text n.name
        : (if n.sub == "" then [] else [ HH.span [ cls "hw-ledger-name__sub" ] [ HH.text n.sub ] ])
    )
