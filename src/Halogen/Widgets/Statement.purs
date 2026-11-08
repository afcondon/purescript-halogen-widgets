-- | A statement: what a page is set up to do, said as a sentence.
-- |
-- | The idea is Bret Victor's and TangleJS's: the settings of a page read as
-- | prose, with each choice standing inside the sentence it changes, so the
-- | page says what will happen rather than offering a form for you to work it
-- | out from. A statement is that sentence and what belongs next to it, in a
-- | fixed shape, so that every page that says what it does says it in the same
-- | place:
-- |
-- | ```
-- |   gauge   │ the sentence, one paragraph per sentence   │ report
-- |           │ detail: what reads better as a table       │
-- |           │ warnings: anything that contradicts it     │
-- | ```
-- |
-- | - **sentences.** The claim. Each inner array is one paragraph; its parts
-- |   are plain text, `reading`s, and the `Quiet` controls, which draw no box
-- |   until touched and so sit inside prose without breaking it.
-- | - **detail.** What the sentence refers to but cannot say as prose: a
-- |   table of rules, a list of steps.
-- | - **warnings.** Anything that contradicts the sentence. Under it, because
-- |   it is read as a qualification of the claim and not as a claim of its own.
-- | - **gauge.** The margin read *before* acting, such as an input meter.
-- | - **report.** The margin read *after*: what came back.
-- |
-- | Every region is optional and an empty one is not drawn, so a statement can
-- | be a single sentence. This is a view function, not a component: the parent
-- | owns everything in it (CONTRACT.md's controlled rule) and the controls in
-- | the sentence raise their own intents.
-- |
-- | Layout is the stylesheet's (`hw-statement-*` in `css/halogen-widgets.css`):
-- | the regions are named grid areas, so a page restyles them without
-- | rearranging them.
module Halogen.Widgets.Statement
  ( StatementConfig
  , defaultStatement
  , statement
  , ReadingConfig
  , reading
  ) where

import Prelude

import Data.Array (null)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Widgets.Style (cls, clss)

-- | The regions of a statement. `class_` is added to the root, so a page can
-- | scope its own look to its own statements.
type StatementConfig w i =
  { class_ :: String
  , sentences :: Array (Array (HH.HTML w i))
  , detail :: Array (HH.HTML w i)
  , warnings :: Array (HH.HTML w i)
  , gauge :: Array (HH.HTML w i)
  , report :: Array (HH.HTML w i)
  }

-- | Every region empty: override the ones you have.
defaultStatement :: forall w i. StatementConfig w i
defaultStatement =
  { class_: ""
  , sentences: []
  , detail: []
  , warnings: []
  , gauge: []
  , report: []
  }

statement :: forall w i. StatementConfig w i -> HH.HTML w i
statement c =
  HH.section [ clss ([ "hw-statement" ] <> if c.class_ == "" then [] else [ c.class_ ]) ]
    ( region "hw-statement-gauge" c.gauge
        <> [ HH.div [ cls "hw-statement-say" ]
               (map (HH.p [ cls "hw-statement-line" ]) c.sentences)
           ]
        <> region "hw-statement-detail" c.detail
        <> region "hw-statement-warn" c.warnings
        <> region "hw-statement-report" c.report
    )
  where
  region name body
    | null body = []
    | otherwise = [ HH.div [ cls name ] body ]

-- | A value the sentence reports and you do not set here: a tempo, the loop
-- | in focus. `label` names it for the tooltip (the sentence's own words say
-- | what it is in context); `alarm` marks the reading that changes what the
-- | rest of the sentence means, such as "no clock".
type ReadingConfig =
  { value :: String
  , label :: String
  , alarm :: Boolean
  }

reading :: forall w i. ReadingConfig -> HH.HTML w i
reading r =
  HH.span
    [ clss ([ "hw-statement-reading" ] <> if r.alarm then [ "is-alarm" ] else [])
    , HP.title r.label
    ]
    [ HH.text r.value ]
