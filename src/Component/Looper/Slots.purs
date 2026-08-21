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
-- | Which took a correction to actually be true. **The MC6 numbers from the
-- | bottom**: A B C is the near row and D E F the far one, so drawing the loops
-- | in index order put loop 1 at the top of the screen and under the far edge
-- | of the pedal. See `boardOrder`.
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
module Component.Looper.Slots (render, waveEdge) where

import Prelude

import Data.Array as Array
import Data.Int (round, toNumber)
import Data.String (joinWith)
import Halogen (AttrName(..), ElemName(..), Namespace(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Looper.Banks as LB
import Foreign.LooperSocket (LoopState, LayerShape, LooperState)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | The six slots, in board order.
-- |
-- | Takes the whole snapshot rather than just the loops, because a slot needs
-- | the sample rate to turn frames into seconds and the selection to say which
-- | loop the flat controls are pointed at.
-- | `shown` is the bank the MC6 is actually displaying, which the app learns
-- | from the presses themselves — every switch says which bank it came from.
-- | The legend has to follow it or it describes a board nobody is standing on.
render :: forall w i. LooperState -> LB.Face -> HH.HTML w i
render lp fc =
  HH.div [ HP.class_ (HH.ClassName "loops") ]
    [ HH.div [ HP.class_ (HH.ClassName "loops-grid") ]
        (Array.mapMaybe cell (join LB.boardRows))
    , utilities fc
    , legend lp
    ]
  where
  cell i = slot lp fc i <$> Array.index lp.loops i

-- | What the six unmarked switches do, on the bank the board is showing.
-- |
-- | **Because nothing else can say.** The MC6's LCD names its own six switches
-- | and stops there; G to L are FS3X footswitches with no display and no
-- | markings at all. Six functions were put on them and the player was left to
-- | remember which — with the predictable result that Clear was pressed for
-- | some minutes without ever being pressed, while Undo was hit until the loop
-- | gave up.
-- |
-- | It named the wrong six for a while, which was worse than naming none. The
-- | list was hand-written, and it was the LOOP bank's list shown whatever bank
-- | the board was on — so with the board on config, the screen said J was Clear
-- | while J was End Stop, and pressing it answered with something about
-- | leaving-state. That reads exactly like a switch wired to the wrong place,
-- | and sent us looking for a reversed mapping that did not exist.
-- |
-- | Two changes, and the second is the one that matters: it takes the bank as
-- | an argument, and it reads `Data.Looper.Banks` rather than restating it. A
-- | display that keeps its own copy of what the device was programmed with is a
-- | display that can be confidently wrong.
utilities :: forall w i. LB.Face -> HH.HTML w i
utilities fc =
  HH.div [ HP.class_ (HH.ClassName "loops-utils-wrap") ]
    [ HH.div [ HP.class_ (HH.ClassName "loops-utils-bank") ]
        [ HH.text (LB.faceName fc) ]
    -- Empty when the board has left the family, which draws nothing rather
    -- than drawing six labels that are not true of anything.
    , HH.div [ HP.class_ (HH.ClassName "loops-utils") ]
        (map one (LB.faceAux fc))
    ]
  where
  -- Tap, then whatever else the switch carries. The extra gestures are shown
  -- smaller and only when they exist: G to L have no markings, so this is the
  -- only place they are written down at all, and a row of empty slots would
  -- suggest a surface fuller than it is.
  one sw =
    HH.div [ HP.class_ (HH.ClassName "loops-util") ]
      [ HH.span [ HP.class_ (HH.ClassName "util-key") ] [ HH.text (LB.switchKey sw) ]
      , HH.div [ HP.class_ (HH.ClassName "util-duties") ]
          ( [ HH.span_ [ HH.text (LB.switchLabel sw) ] ]
              <> extra "\x00d7\x00d7" (LB.switchDouble sw)
              <> extra "hold" (LB.switchHold sw)
          )
      ]
  extra how = case _ of
    Nothing -> []
    Just what ->
      [ HH.span [ HP.class_ (HH.ClassName "util-alt") ]
          [ HH.span [ HP.class_ (HH.ClassName "util-how") ] [ HH.text how ]
          , HH.text what
          ]
      ]

-- | One loop.
slot :: forall w i. LooperState -> LB.Face -> Int -> LoopState -> HH.HTML w i
slot top fc idx st =
  HH.div
    [ HP.class_ (HH.ClassName ("loop-slot " <> stateClass st
        <> (if top.selected == idx then " is-selected" else ""))) ]
    [ HH.div [ HP.class_ (HH.ClassName "loop-head") ]
        [ HH.span [ HP.class_ (HH.ClassName "loop-letter") ]
            [ HH.text (LB.faceLoopKey fc idx) ]
        , HH.span [ HP.class_ (HH.ClassName "loop-state") ] [ HH.text (stateWord st) ]
        , HH.span [ HP.class_ (HH.ClassName "loop-layers") ]
            [ HH.text (if st.layers == 0 then "" else show st.layers <> plural st.layers " layer") ]
        ]
    , track top st
    , HH.div [ HP.class_ (HH.ClassName "loop-foot") ]
        [ HH.span_ [ HH.text (lengthWord top st) ]
        -- The resolutions, shown only when they are not the default. A row of
        -- "forward · centre · free" on six slots is noise; a lone "REV" is
        -- information, and the config bank is otherwise invisible from here.
        , HH.span [ HP.class_ (HH.ClassName "loop-marks") ]
            (map mark (marks st))
        ]
    ]

-- | The layer's shape, mirrored about the middle the way a waveform is read.
-- |
-- | `preserveAspectRatio="none"` so one path stretches to whatever width the
-- | block happens to be — the blocks are laid out by the tiling and their width
-- | is not this module's to know.
-- |
-- | Nothing here rescales the peaks. They arrive absolute, on a decibel curve
-- | with a -60 dBFS floor, and drawing them any other way would throw away the
-- | one thing the picture is insurance against.
wave :: forall w i. Array Int -> Array (HH.HTML w i)
wave env
  | Array.null env = []
  | otherwise =
      [ svgEl "svg"
          [ sAttr "viewBox" ("0 0 " <> show (Array.length env - 1) <> " 2")
          , sAttr "preserveAspectRatio" "none"
          -- **`sAttr`, not `HP.class_`.** See `svgEl` below: on an SVG element
          -- the property form silently does nothing, and the symptom is a
          -- picture that renders perfectly at the wrong size.
          , sAttr "class" "loop-wave"
          ]
          [ svgEl "path" [ sAttr "d" path ] [] ]
      ]
  where
  -- Out along the top and back along the bottom, so the fill is the envelope
  -- rather than an outline of half of it.
  path =
    joinWith " "
      ( [ "M0," <> show (top 0) ]
          <> Array.mapWithIndex (\i v -> "L" <> show i <> "," <> show (edge v)) env
          <> Array.reverse
              (Array.mapWithIndex (\i v -> "L" <> show i <> "," <> show (2.0 - edge v)) env)
          <> [ "Z" ]
      )
  top i = waveEdge (fromMaybe 0 (Array.index env i))
  edge = waveEdge

-- | A peak byte to the top edge of the mark, in a viewBox two units tall.
-- |
-- | **Loud is more ink.** The first version filled the block and drew the
-- | envelope in the background colour, so a loud layer was *less* mark than a
-- | quiet one — inverted, and instantly wrong to look at once it was on screen.
-- |
-- | The floor is because a layer that is quiet is still a layer: a mark you
-- | cannot see reads as one that is not there, which is the opposite of what
-- | this picture is for.
waveEdge :: Int -> Number
waveEdge v = 1.0 - max 0.06 (toNumber v / 255.0)

-- | Just enough SVG to draw one shape, on the same house pattern as
-- | `Component.Controls.Survey`: Halogen ships the namespace-aware constructor
-- | and this needs two elements and three attributes.
-- |
-- | **Classes on SVG go through `sAttr`, never `HP.class_`.** `HP.class_` sets
-- | the DOM *property*, and `SVGElement.className` is a read-only
-- | `SVGAnimatedString` — so the assignment does nothing, quietly, and every
-- | rule keyed on that class simply never applies. The symptom is not a missing
-- | element: the shape renders correctly and at completely the wrong size,
-- | because with no CSS the browser falls back to sizing the SVG from its
-- | viewBox aspect ratio. Survey already did it this way; this did not, and
-- | cost a round of looking at the geometry for a fault that was in the class
-- | attribute.
svgEl :: forall r w i. String -> Array (HH.IProp r i) -> Array (HH.HTML w i) -> HH.HTML w i
svgEl name = HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName name)

sAttr :: forall r i. String -> String -> HH.IProp r i
sAttr k v = HP.attr (AttrName k) v

mark :: forall w i. Mark -> HH.HTML w i
mark m =
  HH.span [ HP.class_ (HH.ClassName ("loop-mark " <> markClass m)) ]
    [ HH.text (markText m) ]

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

  -- Whether there is a picture to draw. A layer being recorded has none yet,
  -- and one recorded before the daemon knew how to draw them never will — both
  -- keep the solid block they always had rather than going pale and empty.
  drawn = not (Array.null sh.env)

  block s =
    HH.div
      [ HP.class_ (HH.ClassName ("loop-block"
          <> (if sounds s then " sounds" else " rest")
          <> (if drawn then " has-wave" else "")))
      -- Decay is invisible in the arena — nothing is scaled there — so the only
      -- way a receding loop can be seen is if the display asks the engine what
      -- each layer is currently worth. A floor of a tenth, because a layer on
      -- its way out is still a fact about the loop and a block you cannot see
      -- reads as a block that is not there.
      , HP.style $ "width:" <> show widthPc <> "%; --layer:" <> show i
          <> (if sh.gain >= 1.0 then ""
              else ";opacity:" <> show (max 0.1 sh.gain))
      ]
      -- **Inside the block, not instead of it.** The block says *where in the
      -- cycle* this layer sounds, which is the thing a waveform cannot show and
      -- the reason the chart was drawn this way in the first place. The
      -- envelope goes in it and answers a different question: which loop is
      -- this, and how loud.
      (if sounds s && drawn then wave sh.env else [])

-- | Present only when there is a cycle to sweep, and positioned outright from
-- | `phase` rather than from anything that keeps its own time.
playhead :: forall w i. LooperState -> LoopState -> Array (HH.HTML w i)
playhead _ st
  | st.loopFrames <= 0 = []
  -- A one-shot between passes has a playhead and nothing to show with it. The
  -- arithmetic never stops — it cannot hold still — so `phase` goes on sweeping
  -- while the loop is silent, and a bar moving under a loop nobody can hear is
  -- the display telling a story about audio that is not happening.
  | st.oneShot && not st.firing = []
  | otherwise =
      [ HH.div
          [ HP.class_ (HH.ClassName "loop-playhead")
          , HP.style $
              "left:" <> show (st.phase * 100.0) <> "%;"
                -- Just wrapped, or standing still: go straight there. Gliding
                -- would run the whole track the wrong way in the first case and
                -- pretend to motion in the second.
                -- **Both ends**, because a loop can wrap at either. A reversed
                -- loop crosses zero going the other way and reappears at the
                -- far edge, which guarding only the near edge would animate as
                -- a full sweep across the slot every time round.
                -- Note that a *stopped* loop keeps its moving playhead. That
                -- is not an oversight: stopping is phase-locked, the loop is
                -- still turning, and showing it frozen would promise it comes
                -- back at the start when it does not.
                <> ( if st.phase < wrapGuard
                       || st.phase > 1.0 - wrapGuard
                       || (st.state == "idle" && st.layers == 0) then "transition:none;"
                     else "transition:left 110ms linear;"
                   )
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
-- | Which switch a loop sits on, from the same table the aux legend uses.
letter :: Int -> String
letter i = case LB.switchLetter i of
  Just l -> l
  Nothing -> "?"

-- | The daemon's own words, softened for reading at a distance. `recordingFirst`
-- | is a state name; "recording" is what you need to know while playing.
stateWord :: LoopState -> String
stateWord st = case st.state of
  -- Layers first, because a loop undone to nothing keeps its length and its
  -- state: the engine still calls it "playing" and there is nothing to play.
  -- The footer still shows the length, which is the useful half — that is what
  -- the next take will land on.
  -- Above the emptiness guard, and it has to be: a level-armed loop is empty
  -- by definition — that is what it is waiting to stop being — so reading the
  -- layer count first made the one state the player most needs to see the one
  -- state that could never be shown.
  "armed" -> if st.pendingAt >= 0 then "waiting" else "listening"
  _ | st.layers == 0 -> if st.loopFrames > 0 then "empty" else ""
  _ | st.muted -> "stopped"
  -- Loaded and waiting for a foot, which is a different thing from stopped: a
  -- stopped loop is still turning and comes back where it would have been, and
  -- a one-shot comes back at the top.
  _ | st.oneShot -> if st.firing then "firing" else "ready"
  -- Sitting this one out. Distinct from stopped, which is a decision you made
  -- and which stays made — this one comes back by itself.
  _ | st.skipping -> "sitting out"
  "recordingFirst" -> "recording"
  "overdubbing" -> "overdub"
  "multiplying" -> "multiply"
  "playing" -> "playing"
  _ -> if st.layers > 0 then "idle" else ""

stateClass :: LoopState -> String
stateClass st = case st.state of
  -- Stopped beats even "recording": a loop being recorded into while silenced
  -- is a thing the player most needs told. Both come after emptiness, which is
  -- a fact about layers rather than about state.
  _ | st.layers == 0 && st.state /= "recordingFirst" && st.state /= "armed" -> "is-empty"
  _ | st.muted -> "is-stopped"
  _ | st.oneShot -> if st.firing then "is-playing" else "is-stopped"
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

-- | What a mark is *for*, which is why they are not one string any more.
-- |
-- | They were: a row of words joined by dots, all the same weight and all the
-- | same colour, so `1 SHOT` — which changes what your foot does — read exactly
-- | like `L 50`, which is a fact about the stereo field. Six slots of that is a
-- | wall of small grey text with the one thing you needed in the middle of it.
-- |
-- | Three kinds, and the distinction is about *when they matter* rather than
-- | what they configure:
-- |
-- | - **`Foot`** changes what the next press does. It is about your next action,
-- |   not about the sound, which is why it is the loudest thing here.
-- | - **`Live`** is doing something on its own, right now, without you — a loop
-- |   that is dropping cycles or receding is changing while you look at it.
-- | - **`Set`** is simply true until you change it.
data Mark
  = Foot String
  | Live String
  | Set String

markText :: Mark -> String
markText = case _ of
  Foot t -> t
  Live t -> t
  Set t -> t

markClass :: Mark -> String
markClass = case _ of
  Foot _ -> "mark-foot"
  Live _ -> "mark-live"
  Set _ -> "mark-set"

-- | What has been done to a loop that is not the default.
-- |
-- | Silence when nothing has: six slots each announcing "forward, centre, free"
-- | is a row of noise that hides the one loop somebody reversed.
marks :: LoopState -> Array Mark
marks st = Array.catMaybes
  -- Your feet first, because they are about what happens next rather than about
  -- what is happening. A one-shot fires where any other loop stops.
  [ if st.oneShot then Just (Foot "1 shot") else Nothing
  , if st.levelArm then Just (Foot "listen") else Nothing
  -- Then the two that move on their own. A loop at 1 in 4 or losing 3 dB a pass
  -- is not where you left it, and nothing else on screen would say so.
  , if st.chance >= 1.0 then Nothing else Just (Live (LB.chanceWord st.chance))
  , if st.decayDb >= 0.0 then Nothing else Just (Live (LB.decayWord st.decayDb))
  -- And then what is merely true.
  , if st.pendulum then Just (Set "swing") else if st.reverse then Just (Set "rev") else Nothing
  , if st.speed == 1.0 then Nothing else Just (Set (speedWord st.speed))
  , if st.pan == 64 then Nothing else Just (Set (panWord st.pan))
  , if st.quant then Just (Set "grid") else Nothing
  , if st.fadeMs <= 0.0 then Nothing else Just (Set ("~" <> LB.fadeWord st.fadeMs))
  ]

-- | Speed as the multiplier the switch was labelled with, not a decimal.
-- |
-- | The board says "x 1/2"; so does this. A display that answers a press with
-- | different words from the switch that caused it makes the player do the
-- | translation, which is the one job a display is for.
speedWord :: Number -> String
speedWord s
  | s == 0.25 = "×¼"
  | s == 0.5 = "×½"
  | s == 1.5 = "×1½"
  | s == 2.0 = "×2"
  | otherwise = "×" <> show (toNumber (round (s * 100.0)) / 100.0)

-- | Pan as a word rather than a number: 0-127 is the wire's business.
panWord :: Int -> String
panWord p
  | p <= 10 = "L"
  | p <= 52 = "l"
  | p <= 74 = "C"
  | p <= 116 = "r"
  | otherwise = "R"

secs :: Number -> String
secs s = show (toNumber (round (s * 10.0)) / 10.0) <> " s"

plural :: Int -> String -> String
plural n word = if n == 1 then word else word <> "s"
