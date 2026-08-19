-- | Six loops, laid out the way they sit under your feet.
-- |
-- | The MC6 has no per-switch LEDs, only an LCD, so **all real feedback about
-- | the looper lives here** (`itajara-in-atlantis` §"The display"). The board
-- | supplies twelve labelled places to stand; this says what is happening.
-- |
-- | ## The grid mirrors the pedal
-- |
-- | Three across and two down, because that is the MC6's own arrangement of
-- | switches A–F, and loop 1 is switch A. Nothing here has to be looked up:
-- | the slot in the top-left of the screen is the switch at the top-left of the
-- | board. Any other arrangement — a column of six, a list — would be a second
-- | mapping to hold in your head while standing on the first one.
-- |
-- | ## What a slot draws, and why not a waveform yet
-- |
-- | Each slot shows the loop's **structure**: one track representing one cycle,
-- | divided into the slots a layer's own length cuts it into, with the ones
-- | that actually sound filled in. That is the thing the plan says matters most
-- | and the thing a waveform would not show — "two takes of the same length
-- | look identical when one of them plays one bar in four".
-- |
-- | The envelope goes *inside* those blocks later, when the daemon grows a
-- | peaks message. Drawing the structure first is not a placeholder: it needs
-- | no change to the engine, and it is the half that carries the meaning.
-- |
-- | ## The geometry is `layer_pos`, not an interpretation of it
-- |
-- | From `engine.rs`:
-- |
-- | ```rust
-- | let slot = (pos / len) % period;
-- | if slot != phase % period { return None }
-- | Some(pos % len)
-- | ```
-- |
-- | So a layer *tiles* across the cycle in blocks of its own `len`, and it
-- | sounds in block `s` exactly when `s % period == phase % period`. `period`
-- | counts blocks within the cycle, not cycles of the loop — which is why
-- | spreading a layer grows the loop around it and leaves room, rather than
-- | making it skip whole cycles.
-- |
-- | This module reproduces that expression rather than paraphrasing it. If the
-- | engine's rule changes, the drawing is wrong in the same way the sound is,
-- | which is the only kind of wrong worth having.
-- |
-- | ## The playhead, and the way that went wrong first
-- |
-- | Halogen re-renders at 10 Hz, which would step a playhead in visible jumps,
-- | so something has to interpolate between snapshots.
-- |
-- | The first attempt was a CSS animation of duration `loopSecs` with a
-- | **negative `animation-delay`** of the current position, restated on every
-- | snapshot. It ran at exactly double speed, and measuring it is the only
-- | reason that was noticed: a 6.47 s loop swept in 3.23 s. Changing
-- | `animation-delay` on a *running* animation shifts its start time without
-- | resetting elapsed time, so the position is `(elapsed - delay) / duration`
-- | — and both `elapsed` and `-delay` advance in real time. The animation and
-- | the correction were counting the same seconds twice.
-- |
-- | What it does instead is the plain thing: `left` is the phase, stated
-- | outright, with a short linear `transition` doing the interpolation. The
-- | position is then a pure function of the snapshot and cannot accumulate
-- | error, because nothing is accumulating.
-- |
-- | The one wrinkle a transition brings is the cycle boundary, where phase goes
-- | from nearly one to nearly zero and the transition would sweep the playhead
-- | *backwards* across the whole track. So the transition is suppressed for the
-- | first fraction of a cycle, which is computable from the snapshot alone and
-- | costs a jump nobody can see at the one moment a jump is correct.
module Component.Looper.Slots (render) where

import Prelude

import Data.Array as Array
import Data.Int (round, toNumber)
import Data.Maybe (Maybe(..))
import Foreign.LooperSocket (LoopState, LayerShape, LooperState)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | The six slots, in board order.
-- |
-- | Takes the whole snapshot rather than just the loops, because a slot needs
-- | the sample rate to turn frames into seconds and the selection to say which
-- | loop the flat controls are pointed at.
render :: forall w i. LooperState -> HH.HTML w i
render lp =
  HH.div [ HP.class_ (HH.ClassName "loops") ]
    [ HH.div [ HP.class_ (HH.ClassName "loops-grid") ]
        (Array.mapWithIndex (slot lp) (Array.take 6 lp.loops))
    , legend lp
    ]

-- | One loop.
slot :: forall w i. LooperState -> Int -> LoopState -> HH.HTML w i
slot top idx st =
  HH.div
    [ HP.class_ (HH.ClassName ("loop-slot " <> stateClass st
        <> (if top.selected == idx then " is-selected" else ""))) ]
    [ HH.div [ HP.class_ (HH.ClassName "loop-head") ]
        [ HH.span [ HP.class_ (HH.ClassName "loop-letter") ] [ HH.text (letter idx) ]
        , HH.span [ HP.class_ (HH.ClassName "loop-state") ] [ HH.text (stateWord st) ]
        , HH.span [ HP.class_ (HH.ClassName "loop-layers") ]
            [ HH.text (if st.layers == 0 then "" else show st.layers <> plural st.layers " layer") ]
        ]
    , track top st
    , HH.div [ HP.class_ (HH.ClassName "loop-foot") ]
        [ HH.span_ [ HH.text (lengthWord top st) ]
        , HH.span [ HP.class_ (HH.ClassName "loop-quant") ]
            [ HH.text (if st.quant then "grid" else "free") ]
        ]
    ]

-- | The cycle, its layers, and the playhead.
-- |
-- | An empty loop still draws the track. Six slots that vanish when empty would
-- | make the board's shape change under your feet, and the whole point of the
-- | grid is that it does not.
track :: forall w i. LooperState -> LoopState -> HH.HTML w i
track top st =
  HH.div [ HP.class_ (HH.ClassName "loop-track") ]
    (Array.mapWithIndex (layerRow st) st.shapes <> playhead top st)

-- | One layer, as the blocks in which it sounds.
layerRow :: forall w i. LoopState -> Int -> LayerShape -> HH.HTML w i
layerRow st i sh =
  HH.div [ HP.class_ (HH.ClassName "loop-layer") ]
    (map block (Array.range 0 (blocks - 1)))
  where
  -- How many of this layer's own lengths fit in the cycle. At least one, so a
  -- layer as long as the loop is one block rather than none.
  blocks = max 1 (if sh.len > 0 then st.loopFrames / sh.len else 1)

  period = max 1 sh.period

  -- `engine.rs`: `slot != phase % period` is silence. Same expression.
  sounds s = s `mod` period == sh.phase `mod` period

  widthPc = 100.0 / toNumber blocks

  block s =
    HH.div
      [ HP.class_ (HH.ClassName ("loop-block" <> if sounds s then " sounds" else " rest"))
      , HP.style ("width:" <> show widthPc <> "%; --layer:" <> show i)
      ]
      []

-- | Present only when there is a cycle to sweep, and positioned outright from
-- | `phase` rather than from anything that keeps its own time.
playhead :: forall w i. LooperState -> LoopState -> Array (HH.HTML w i)
playhead _ st
  | st.loopFrames <= 0 = []
  | otherwise =
      [ HH.div
          [ HP.class_ (HH.ClassName "loop-playhead")
          , HP.style $
              "left:" <> show (st.phase * 100.0) <> "%;"
                -- Just wrapped, or standing still: go straight there. Gliding
                -- would run the whole track backwards in the first case and
                -- pretend to motion in the second.
                <> (if st.phase < wrapGuard || st.state == "idle"
                      then "transition:none;"
                      else "transition:left 110ms linear;")
          ]
          []
      ]

-- | How much of a cycle counts as "just wrapped".
-- |
-- | Wide enough to cover more than one 100 ms tick on any loop long enough to
-- | play — at 6.5 s this is 130 ms, or between one and two snapshots — and
-- | narrow enough that the un-interpolated stretch is over before an eye
-- | settles on it.
wrapGuard :: Number
wrapGuard = 0.02

-- | What the flat controls are pointed at, and what the rig's clock says.
-- |
-- | Small, because it is reference rather than performance information — but
-- | present, because "which loop does the pedal face drive" is otherwise
-- | invisible, and because a Link tempo of zero is the difference between no
-- | clock and a clock reading zero.
legend :: forall w i. LooperState -> HH.HTML w i
legend lp =
  HH.div [ HP.class_ (HH.ClassName "loops-legend") ]
    [ HH.span_ [ HH.text ("controls follow " <> letter lp.selected) ]
    , HH.span_
        [ HH.text $
            if lp.linkAnchors == 0 then "no clock"
            else show (round lp.linkTempo) <> " bpm · "
                   <> show (round lp.linkQuantum) <> "/bar"
        ]
    , if lp.linkRejected == 0 then HH.text ""
      else HH.span [ HP.class_ (HH.ClassName "loops-warn") ]
             [ HH.text (show lp.linkRejected <> " clock messages refused") ]
    ]

-- | A B C on the top row, D E F below — the board's own letters, so the screen
-- | and the pedal agree without anybody translating.
letter :: Int -> String
letter i = case Array.index [ "A", "B", "C", "D", "E", "F" ] i of
  Just l -> l
  Nothing -> "?"

-- | The daemon's own words, softened for reading at a distance. `recordingFirst`
-- | is a state name; "recording" is what you need to know while playing.
stateWord :: LoopState -> String
stateWord st = case st.state of
  "armed" -> if st.pendingAt >= 0 then "waiting" else "armed"
  "recordingFirst" -> "recording"
  "overdubbing" -> "overdub"
  "multiplying" -> "multiply"
  "playing" -> "playing"
  _ -> if st.layers > 0 then "stopped" else ""

stateClass :: LoopState -> String
stateClass st = case st.state of
  "armed" -> "is-armed"
  "recordingFirst" -> "is-recording"
  "overdubbing" -> "is-recording"
  "multiplying" -> "is-recording"
  "playing" -> "is-playing"
  _ -> if st.layers > 0 then "is-stopped" else "is-empty"

-- | Length, or the countdown to one.
-- |
-- | A scheduled transition that shows nothing is the failure this display
-- | exists to prevent: a deliberate wait and a dead footswitch look identical
-- | until one of them says how long it intends to wait.
lengthWord :: LooperState -> LoopState -> String
lengthWord top st
  | st.pendingAt >= 0 =
      "in " <> secs (toNumber st.pendingAt / toNumber (max 1 top.sampleRate))
  | st.loopFrames <= 0 = "empty"
  | otherwise = secs st.loopSecs

secs :: Number -> String
secs s = show (toNumber (round (s * 10.0)) / 10.0) <> " s"

plural :: Int -> String -> String
plural n word = if n == 1 then word else word <> "s"
