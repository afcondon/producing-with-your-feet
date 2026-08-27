-- | The Looper page, whole.
-- |
-- | ## Why this is its own module
-- |
-- | It was three hundred and fifty lines inside `Component.App`, which is a
-- | four-thousand-line component that also owns two MIDI ports, the MC6's SysEx
-- | protocol, twelve pedals, board presets and a folder backup. Nothing about
-- | the looper page needed to be in there — it reads a snapshot and emits
-- | actions — but while it was, "what does the looper page depend on?" had no
-- | answer short of reading the whole file.
-- |
-- | Now it has one, and it is the type signature below. `State` is a row, so the
-- | call site passes `AppState` unchanged and the compiler still holds this
-- | module to exactly the fields it names. `Handlers` is the other half of the
-- | boundary: every action this page can cause, listed in one record.
-- |
-- | ## What deliberately did NOT come with it
-- |
-- | **The pedal face arrives as an argument.** It is a `HH.slot` of
-- | `Component.Pedal.View` — the same component the board grid uses — so
-- | rendering it here would drag the app's whole slot row into a module whose
-- | only other business is a snapshot. Itajara's donut is a *pedal* view that
-- | this page happens to show; it belongs to whoever owns the pedals.
-- |
-- | **The two connection facts arrive as booleans.** This page needs to know
-- | whether there is an MC6 to program and a Twister to page, and nothing else
-- | about the MIDI layer. Passing `MidiConnections` would have imported the
-- | whole rig to answer two questions.
-- |
-- | ## It holds nothing
-- |
-- | Unchanged from when it lived in `App`, and the reason the extraction was
-- | mechanical: this page renders the snapshot the daemon pushes and sends
-- | command strings back. Every decision about what a command *means* lives in
-- | one place, at the other end of the socket.
-- |
-- | Deliberately unfinished. The interface this wants is in DESIGN-LOOPER §12 —
-- | concentric rings sharing a phase pointer, and a column per layer — and the
-- | thesis is that the display should say what the *next press* will do, not
-- | merely what happened.
module Component.Looper.Page
  ( State
  , Handlers
  , Ports
  , render
  ) where

import Prelude

import Component.Looper.Board (render) as BoardSim
import Component.Looper.Slots as Slots
import Component.Looper.TwisterMap as TwisterMap
import Data.Array as Array
import Data.Int as Int
import Data.Looper as Looper
import Data.Looper.Banks as LoopBanks
import Data.Maybe (Maybe(..))
import Foreign.LooperSocket (LooperState, SocketStatus)
import Foreign.LooperSocket as LooperSocket
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | Everything the page reads.
-- |
-- | Open, so `AppState` satisfies it without a projection at the call site —
-- | but named field by field, so adding a read here is a visible change to the
-- | boundary rather than a quiet reach into a bigger record.
type State r =
  ( looper :: Maybe LooperState
  , looperStatus :: SocketStatus
  , looperSnapshotAge :: Number
  , looperFocus :: Int
  , looperShowsSlots :: Boolean
  , looperBankShown :: Maybe LoopBanks.BankSlot
  , looperLastAction :: Maybe String
  , looperProgramStatus :: Maybe String
  , mc6LooperBankNum :: Int
  , mc6LoopBankBase :: Int
  , mc6BoardBankNum :: Int
  , twisterPage :: Int
  , twisterHeardBank :: Maybe Int
  | r
  )

-- | Whether there is anything on the other end of each wire.
-- |
-- | Two booleans rather than the connection record, because those are the only
-- | two questions this page asks about MIDI: is there an MC6 to write a bank
-- | to, and is there a Twister for the layout card to describe.
type Ports = { mc6 :: Boolean, twister :: Boolean }

-- | Every action this page can cause.
-- |
-- | **The interesting one is `gesture`.** It takes a CC number, not a command —
-- | because every button here goes through the Itajara pedal's CC exactly as a
-- | footswitch does, and there is deliberately no shortcut to the socket from
-- | the UI. One route in means one place to debug. Keeping it as `Int -> i`
-- | means this module cannot take that shortcut even by accident: it has no
-- | word for the socket.
type Handlers i =
  { setFace :: Boolean -> i
  , simulate :: LoopBanks.BankSlot -> Int -> LoopBanks.Gesture -> i
  , showTwisterPage :: Int -> i
  , gesture :: Int -> i
  , setClick :: Boolean -> i
  , setArm :: String -> i
  , setFeedback :: String -> i
  , setTone :: String -> i
  , programLooperBank :: i
  , programLoopBanks :: i
  }

render
  :: forall w i r
   . Handlers i
  -> Ports
  -> HH.HTML w i
  -> Record (State r)
  -> HH.HTML w i
render h ports pedalFace state =
  HH.div [ HP.class_ (HH.ClassName "looper-view") ]
    [ HH.h2_ [ HH.text "Looper" ]
    , connectionLine
    , audioLine
    , faceToggle
    , case state.looper of
        Just lp | state.looperShowsSlots -> Slots.render lp state.looperFocus (LoopBanks.face state.looperBankShown)
        _ -> HH.text ""
    -- The board, on screen, with every switch live. Present always rather than
    -- behind a debug flag: knowing what the pedal is showing is useful when
    -- nothing is broken, and a panel you have to turn on is a panel that is off
    -- when you need it.
    , BoardSim.render h.simulate (LoopBanks.face state.looperBankShown)
    -- What the last press did, in words. Present for refusals as much as for
    -- commands: the machine names every gap it meets rather than swallowing
    -- the press, and a footswitch that silently does nothing is the failure
    -- this whole surface exists to design against.
    , case state.looperLastAction of
        Nothing -> HH.text ""
        Just msg -> HH.p [ HP.class_ (HH.ClassName "looper-lastaction") ] [ HH.text msg ]
    , HH.div [ HP.class_ (HH.ClassName "looper-columns") ]
        [ HH.div [ HP.class_ (HH.ClassName "looper-left") ]
            [ case state.looper of
                Just lp | not state.looperShowsSlots -> HH.div_ [ transport lp, readout lp ]
                _ -> HH.text ""
            , footswitchCard
            ]
        -- The pedal face, on the page it belongs to rather than in the board
        -- grid. Same component the board uses, so the knobs, the drag handling
        -- and the value routing are the ones already in service elsewhere.
        , HH.div [ HP.class_ (HH.ClassName "looper-right") ] [ pedalFace ]
        ]
    -- The Twister's layout, at the bottom because it is a reference rather than
    -- a control: you read it once, learn the page, and stop looking. Folded
    -- shut by default for the same reason.
    , TwisterMap.render ports.twister state.twisterPage state.twisterHeardBank h.showTwisterPage
    ]
  where
  st = state.looperStatus

  -- Two faces, not two pages. The old transport is the only thing that can
  -- drive the engine by hand, which is exactly what the six-slot display needs
  -- in order to have anything to show while the state machine does not exist
  -- yet — so it stays one click away rather than behind a nav item.
  faceToggle =
    HH.div [ HP.class_ (HH.ClassName "looper-face-toggle") ]
      [ tab true "Loops", tab false "Transport" ]

  tab wants label =
    HH.button
      [ HP.class_ (HH.ClassName (if state.looperShowsSlots == wants then "face-tab on" else "face-tab"))
      , HE.onClick \_ -> h.setFace wants
      ]
      [ HH.text label ]

  -- A connected socket says nothing about whether audio is running: the push
  -- thread reads shared atomics and will serve a confident snapshot from an
  -- engine whose device was unplugged. That failure cost an afternoon of
  -- hunting a MIDI fault, so it gets its own line and says what to do.
  audioLine = case state.looper of
    Just lp | not lp.audioAlive ->
      HH.p [ HP.class_ (HH.ClassName "looper-conn down") ]
        [ HH.text $
            if lp.deviceLost
              then "The daemon lost the audio device — reconnecting. Commands will not take effect until it is back."
              else "The daemon is connected but its audio has stopped. Commands will not take effect."
        ]
    Just lp | lp.reopens > 0 ->
      HH.p [ HP.class_ (HH.ClassName "looper-muted") ]
        [ HH.text $ "Audio device recovered "
            <> show lp.reopens
            <> (if lp.reopens == 1 then " time" else " times")
            <> " this session."
        ]
    _ -> HH.text ""

  -- **A picture of the past must never be presented as the present.**
  --
  -- "Connected" was true and useless: the socket was open, commands were
  -- landing, and the loops on screen were minutes old. So the line reports the
  -- age of the newest snapshot as well as the state of the socket — the daemon
  -- pushes thirty times a second, so anything approaching a second is already
  -- wrong and the player deserves to be told rather than to work it out from a
  -- playhead that has stopped moving.
  connectionLine =
    let stale = state.looperSnapshotAge > 1000.0
    in HH.p
      [ HP.class_ (HH.ClassName ("looper-conn" <>
          if st.connected && not stale then " ok" else " down")) ]
      [ HH.text $
          if st.connected && stale then
            "Connected, but the picture is "
              <> show (Int.round (state.looperSnapshotAge / 100.0) / 10) <> " s old."
          else if st.connected then "Connected to the daemon."
          else if st.everConnected then "Lost the daemon — retrying."
          else "No daemon. Start it with:  itajara loop --device AUDIO4c --ws"
      ]

  -- Every button here goes through the Itajara pedal's CC — the same path a
  -- footswitch or a Twister encoder takes. There is deliberately no shortcut to
  -- the socket from the UI: one route in means one place to debug.
  transport lp =
    HH.div [ HP.class_ (HH.ClassName "looper-transport") ]
      [ gestureBtn ("looper-btn" <> if lp.recording then " recording" else "")
          (not st.connected) 1 (nextPress lp)
      , gestureBtn "looper-btn small"
          (not st.connected || lp.loopFrames == 0) 2
          (if LooperSocket.phaseOf lp == LooperSocket.Multiplying then "End multiply" else "Multiply")
      , gestureBtn "looper-btn small" (not st.connected) 5 "Take"
      , gestureBtn "looper-btn small" (not st.connected || lp.layers == 0) 3 "Undo"
      -- Only offered when there is a length and nothing sitting in it, which is
      -- the only moment it is meaningful and also the only moment it is wanted.
      , gestureBtn "looper-btn small"
          (not st.connected || lp.layers /= 0 || lp.loopFrames == 0) 13 "Forget length"
      , HH.button
          [ HP.class_ (HH.ClassName "looper-btn small")
          , HP.disabled (not st.connected)
          , HE.onClick \_ -> h.setClick (not lp.click)
          ]
          [ HH.text (if lp.click then "Click off" else "Click on") ]
      , armThreshold lp
      , revoxFeedback
      ]

  -- | How loud a sound has to be to start a level-armed loop.
  -- |
  -- | **On the page rather than on a knob, and deliberately.** It very nearly
  -- | went on the Twister — turn to set the level, which arms the loop, press to
  -- | record regardless. Two things sent it here instead. It is *rig-wide*, so
  -- | on a page of eight loop encoders it would have been eight knobs quietly
  -- | writing one value; and it is a once-a-session calibration of the room and
  -- | the instrument, which is the same kind of thing as the residual latency
  -- | and belongs where that kind of thing lives.
  -- |
  -- | A performance surface is for what you reach for while playing. Everything
  -- | else on it is in the way.
  armThreshold lp =
    HH.span [ HP.class_ (HH.ClassName "looper-arm") ]
      [ HH.label_ [ HH.text "Listen from" ]
      , HH.input
          [ HP.type_ HP.InputRange
          , HP.min (-80.0)
          , HP.max 0.0
          , HP.step (HP.Step 1.0)
          , HP.value (show (Int.round lp.armDb))
          , HP.disabled (not st.connected)
          , HE.onValueInput \v -> h.setArm v
          ]
      , HH.span [ HP.class_ (HH.ClassName "looper-arm-value") ]
          [ HH.text (show (Int.round lp.armDb) <> " dBFS") ]
      ]

  -- | What a Revox pass leaves of what was under it.
  -- |
  -- | **Here rather than on the Twister, and only because both pages are
  -- | full.** It is a performance control — riding the feedback is how the mode
  -- | is played — so it wants a knob, and it will get one on the trim-and-shift
  -- | page when that exists. Until then a slider you can reach is better than a
  -- | cell nothing else could spare.
  -- The focused loop's, because Revox is a per-loop mode — unlike the arm
  -- threshold above it, which is the rig's.
  revoxFeedback = case state.looper >>= \l -> Array.index l.loops state.looperFocus of
    Nothing -> HH.text ""
    Just fl -> revoxSlider fl

  revoxSlider fl =
    HH.span [ HP.class_ (HH.ClassName "looper-arm") ]
      [ HH.label_ [ HH.text "Tape leaves" ]
      , HH.input
          [ HP.type_ HP.InputRange
          , HP.min (-24.0)
          , HP.max 0.0
          , HP.step (HP.Step 0.5)
          , HP.value (show fl.fbDb)
          , HP.disabled (not st.connected)
          , HE.onValueInput \v -> h.setFeedback v
          ]
      , HH.span [ HP.class_ (HH.ClassName "looper-arm-value") ]
          [ HH.text (LoopBanks.levelWord fl.fbDb <> " a pass") ]
      , HH.label_ [ HH.text "keeping" ]
      , HH.input
          [ HP.type_ HP.InputRange
          , HP.min 1000.0
          , HP.max 20000.0
          , HP.step (HP.Step 250.0)
          , HP.value (show (Int.round fl.toneHz))
          , HP.disabled (not st.connected)
          , HE.onValueInput \v -> h.setTone v
          ]
      , HH.span [ HP.class_ (HH.ClassName "looper-arm-value") ]
          [ HH.text (if fl.toneHz >= 20000.0 then "all of it"
                     else show (Int.round (fl.toneHz / 100.0) * 100) <> " Hz") ]
      ]

  gestureBtn cls disabled ccNum label =
    HH.button
      [ HP.class_ (HH.ClassName cls)
      , HP.disabled disabled
      , HE.onClick \_ -> h.gesture ccNum
      ]
      [ HH.text label ]

  -- | What the next press does, which is the thing every looper hides.
  nextPress lp = case LooperSocket.phaseOf lp of
    LooperSocket.RecordingFirst -> "Close the loop"
    LooperSocket.Overdubbing -> "Finish overdub"
    LooperSocket.Multiplying -> "End multiply"
    LooperSocket.Armed -> "Starting…"
    -- An empty loop with a length is not an overdub, whatever the engine calls
    -- the phase. Saying "Overdub" with nothing to overdub onto is what made a
    -- kept grid read as a stuck one. `Playing` and `Idle` differ only in
    -- whether the playhead is moving, and the next press is the same either
    -- way, so they share the answer rather than repeating it.
    LooperSocket.Playing -> byContents
    LooperSocket.Idle -> byContents
    where
    byContents
      | lp.loopFrames == 0 = "Record"
      | lp.layers == 0 = "Record on the grid"
      | otherwise = "Overdub"

  readout lp =
    HH.div [ HP.class_ (HH.ClassName "looper-readout") ]
      [ phaseBar lp
      , HH.table [ HP.class_ (HH.ClassName "docs-table") ]
          [ HH.tbody_
              [ row "State" lp.state
              , row "Layers" (show lp.layers <> " of " <> show lp.maxLayers)
              , row "Loop"
                  ( if lp.loopFrames == 0 then "not set"
                    else fmt2 lp.loopSecs <> " s  (" <> show lp.loopFrames <> " frames)"
                           <> (if lp.layers == 0
                                 then "  \x2014 empty, grid kept for the next take"
                                 else "")
                  )
              , row "Input" (fmt1 lp.inDb <> " dBFS")
              , row "Output" (fmt1 lp.outDb <> " dBFS")
              , row "Alignment"
                  ( if lp.calibrated then "locked, K " <> show lp.k
                    else "waiting for the first input buffer"
                  )
              ]
          ]
      ]

  -- Where we are in the cycle. Crude next to the concentric rings §12 wants,
  -- but it is the one thing a looper must never leave you guessing about.
  phaseBar lp =
    HH.div [ HP.class_ (HH.ClassName "looper-phase") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "looper-phase-fill")
          , HP.style ("width:" <> show (max 0.0 (min 100.0 (lp.phase * 100.0))) <> "%")
          ]
          []
      ]

  -- Itajara is a pedal now, so its full surface lives on its own Detail page
  -- and any switch can be assigned to any of it. What remains here is the one
  -- thing that page cannot do: put a usable bank on the hardware today.
  footswitchCard =
    HH.div [ HP.class_ (HH.ClassName "looper-footswitch") ]
      [ HH.h3_ [ HH.text "Footswitch control" ]
      , HH.p [ HP.class_ (HH.ClassName "looper-muted") ]
          [ HH.text $
              "Itajara is a pedal on channel " <> show Looper.itajaraChannel
              <> ", so the MC6 addresses it exactly as it addresses Habit or MOOD — "
              <> "and every control is on its own page, assignable to any switch. "
              <> "This writes a starter transport bank to MC6 bank "
              <> show state.mc6LooperBankNum <> "."
          ]
      , HH.table [ HP.class_ (HH.ClassName "docs-table") ]
          [ HH.tbody_ (map bankRow (Looper.looperBank state.mc6LooperBankNum state.mc6BoardBankNum).switches) ]
      , HH.button
          [ HP.class_ (HH.ClassName "files-btn")
          , HP.disabled (not ports.mc6)
          , HE.onClick \_ -> h.programLooperBank
          ]
          [ HH.text "Program MC6 looper bank" ]
      , loopFamilyCard
      , case state.looperProgramStatus of
          Nothing -> HH.text ""
          Just msg -> HH.p [ HP.class_ (HH.ClassName "looper-muted") ] [ HH.text msg ]
      ]

  -- The six-loop machine's own banks, which are a different thing wearing a
  -- similar name to the transport bank above: that one drives one loop through
  -- Itajara's pedal CCs, this one gives the app twelve labelled places to stand
  -- on each of six pages and lets the app decide what standing there means.
  loopFamilyCard =
    HH.div [ HP.class_ (HH.ClassName "looper-footswitch") ]
      [ HH.h3_ [ HH.text "Six-loop banks" ]
      , HH.p [ HP.class_ (HH.ClassName "looper-muted") ]
          [ HH.text $
              show (Array.length LoopBanks.allSlots)
              <> " banks from MC6 bank " <> show state.mc6LoopBankBase
              <> ", uploaded once. Every switch sends its own CC on channel "
              <> show LoopBanks.switchChannel
              <> ", so a press says which bank it came from and the app never has "
              <> "to remember which page the board is showing."
          ]
      , HH.table [ HP.class_ (HH.ClassName "docs-table") ]
          [ HH.tbody_ (map familyRow (LoopBanks.banks
              { base: state.mc6LoopBankBase, boardBank: state.mc6BoardBankNum })) ]
      , HH.button
          [ HP.class_ (HH.ClassName "files-btn")
          , HP.disabled (not ports.mc6)
          , HE.onClick \_ -> h.programLoopBanks
          ]
          [ HH.text "Program MC6 loop banks" ]
      ]

  familyRow cb =
    HH.tr_
      [ HH.td [ HP.class_ (HH.ClassName "docs-cc") ] [ HH.text (show cb.mc6BankNumber) ]
      , HH.td_ [ HH.text cb.name ]
      , HH.td_
          [ HH.text $ Array.intercalate ", "
              (Array.filter (_ /= "") (map _.label cb.switches)) ]
      ]

  -- Switch letters run A–F on the MC6 itself, then G/H/I on the first FS3X.
  bankRow sw =
    if sw.label == "" then HH.text ""
    else HH.tr_
      [ HH.td [ HP.class_ (HH.ClassName "docs-cc") ] [ HH.text sw.label ]
      , HH.td_ [ HH.text sw.longName ]
      ]

  row label value =
    HH.tr_ [ HH.td [ HP.class_ (HH.ClassName "docs-cc") ] [ HH.text label ], HH.td_ [ HH.text value ] ]

  fmt1 n = show (Int.toNumber (Int.round (n * 10.0)) / 10.0)
  fmt2 n = show (Int.toNumber (Int.round (n * 100.0)) / 100.0)
