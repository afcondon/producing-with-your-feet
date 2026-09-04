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
import Component.Looper.TwisterCard as TwisterCard
import Config.Registry (PedalRegistry)
import Data.Pedal (PedalId)
import Data.Twister.Scene (Scene)
import Component.Looper.Slots as Slots
import Component.Looper.TwisterMap as TwisterMap
import Data.Looper.Recipes as Recipes
import Data.Looper.Twister as TW
import Data.Array as Array
import Data.Int as Int
import Data.Looper as Looper
import Data.Looper.Banks as LoopBanks
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Engine (LogLine, LooperPanel(..))
import Foreign.LooperSocket (LooperState, SocketStatus)
import Foreign.LooperSocket as LooperSocket
import Halogen (AttrName(..), ElemName(..), Namespace(..))
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
  , looperPanel :: Maybe LooperPanel
  , looperPeaks :: Maybe LooperSocket.Peaks
  , looperEditLocal :: Map String Int
  , looperBankShown :: Maybe LoopBanks.BankSlot
  , looperLog :: Array LogLine
  , looperProgramStatus :: Maybe String
  , mc6LooperBankNum :: Int
  , mc6LoopBankBase :: Int
  , mc6BoardBankNum :: Int
  , twisterPage :: Int
  , twisterHeardBank :: Maybe Int
  -- | What the card draws: which surface the Twister is on, and the two things
  -- | needed to name its cells. `registry` rather than a prepared list, because
  -- | a scene's cells belong to several pedals and which ones is not known
  -- | until the scene is read.
  , twisterScene :: Maybe Scene
  , focusPedalId :: Maybe PedalId
  , registry :: PedalRegistry
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
  , openPanel :: Maybe LooperPanel -> i
  , gesture :: Int -> i
  , setClick :: Boolean -> i
  , setArm :: String -> i
  , setFeedback :: String -> i
  , setTone :: String -> i
  , programLooperBank :: i
  , programLoopBanks :: i
  -- | Open the printable sheet. No argument: what goes on it is generated from
  -- | the same tables this page draws from, so there is nothing to pass.
  , printSheet :: i
  -- | One layer of one loop in or out of the mix: the daemon's loop index,
  -- | the layer number the slot shows (from one), and the wanted state.
  , setLayer :: Int -> Int -> Boolean -> i
  -- | The Edit panel's five: window in and out (loop, frames), the whole
  -- | loop again, a shift of the start (loop, signed frames), and a fresh
  -- | waveform (loop).
  , windowIn :: Int -> Int -> i
  , windowOut :: Int -> Int -> i
  , clearWindow :: Int -> i
  , shiftStart :: Int -> Int -> i
  , askPeaks :: Int -> i
  -- | A slider was released: the snapshot owns its value again.
  , editDone :: String -> i
  }

render
  :: forall w i r
   . Handlers i
  -> Ports
  -> Record (State r)
  -> HH.HTML w i
render h ports state =
  HH.div [ HP.class_ (HH.ClassName "looper-view") ]
    -- **No heading, and no "connected" line in the ordinary case.** The nav
    -- above already says LOOPER, and a band across the page saying the socket
    -- is open is a band that is right all day and tells you nothing on the one
    -- day it matters. Both are gone; what stands there instead is the rig's
    -- state, which is the thing you look up.
    [ statusStrip
    , connectionLine
    , audioLine
    , HH.div [ HP.class_ (HH.ClassName "looper-body") ]
        [ HH.div [ HP.class_ (HH.ClassName "looper-main") ]
            [ faceToggle
            , case state.looper of
                Just lp | state.looperShowsSlots -> Slots.render h.setLayer lp state.looperFocus (LoopBanks.face state.looperBankShown)
                _ -> HH.text ""
            , case state.looper of
                Just lp | not state.looperShowsSlots -> HH.div_ [ transport lp, readout lp ]
                _ -> HH.text ""
            ]
        -- **The board and the log, in that order, and the order is the
        -- argument.** Press a switch here and the sentence it produced appears
        -- directly underneath it — cause above effect, in one column, without
        -- crossing the page. That is what the corner dock could not do.
        , HH.div [ HP.class_ (HH.ClassName "looper-side") ]
            [ twisterCard, logCard ]
        ]
    , panel
    ]
  where
  st = state.looperStatus

  panelBtn which label =
    HH.button
      [ HP.class_ (HH.ClassName
          ("looper-help-btn" <> if state.looperPanel == Just which then " on" else ""))
      , HE.onClick \_ -> h.openPanel (if state.looperPanel == Just which then Nothing else Just which)
      ]
      [ HH.text label ]

  panel = case state.looperPanel of
    Nothing -> HH.text ""
    Just PanelTwister ->
      -- Narrow, because the card is one page now. It was as wide as the panel
      -- would go so that both pages could sit side by side, and that width was
      -- spent covering the loops the card describes.
      modal "is-narrow" "Midifighter Twister — what each encoder does"
        [ TwisterMap.render ports.twister state.twisterPage state.twisterHeardBank h.showTwisterPage ]
    Just PanelBoard ->
      modal "is-board" "The MC6 board"
        [ BoardSim.render h.simulate (LoopBanks.face state.looperBankShown) ]
    Just PanelBanks ->
      modal "" "MC6 banks" [ footswitchCard, loopFamilyCard ]
    Just PanelEdit ->
      modal "is-edit" ("Edit — loop " <> show (state.looperFocus + 1)) [ editPanel ]
    -- **In the app rather than on paper, and that is the whole reason it
    -- exists.** Following a written sequence means looking away from the
    -- browser, and Chrome throttles a background tab — the looper stops
    -- handling Twister messages and every control reads as broken. A recipe you
    -- can follow without leaving the page is not a convenience.
    Just PanelRecipes ->
      modal "" "Recipes — what to press, and what should happen"
        [ HH.div [ HP.class_ (HH.ClassName "recipes") ]
            ( [ HH.p [ HP.class_ (HH.ClassName "recipes-preamble") ]
                  [ HH.text Recipes.preamble ] ]
                <> map recipeCard Recipes.recipes
            )
        ]

  -- | One recipe: why you would want it, the moves in order, and what each
  -- | move should say back.
  recipeCard r =
    HH.div [ HP.class_ (HH.ClassName "recipe") ]
      ( [ HH.h4_ [ HH.text r.name ]
        , HH.p [ HP.class_ (HH.ClassName "recipe-why") ] [ HH.text r.why ]
        , HH.ol [ HP.class_ (HH.ClassName "recipe-steps") ] (map recipeStep r.steps)
        ]
          <> case r.note of
               Nothing -> []
               Just n -> [ HH.p [ HP.class_ (HH.ClassName "recipe-note") ] [ HH.text n ] ]
      )

  recipeStep s =
    HH.li_
      ( [ if s.at == "" then HH.text ""
          else HH.span [ HP.class_ (HH.ClassName "recipe-at") ] [ HH.text s.at ]
        , HH.span [ HP.class_ (HH.ClassName "recipe-act") ] [ HH.text s.act ]
        ]
          <> case s.expect of
               Nothing -> []
               -- Quoted from an ack the daemon really sends, which is what makes
               -- this a test script as well as a manual.
               Just e -> [ HH.span [ HP.class_ (HH.ClassName "recipe-expect") ] [ HH.text e ] ]
      )

  -- | The Twister's current page, permanently.
  -- |
  -- | **Not behind a button, and that is a reversal made by looking at it.** It
  -- | was a corner dock you opened; seeing it open showed that it is small
  -- | enough to keep, and that a control you have to summon is one you will not
  -- | summon with a guitar in your hands. It is also the only place G to L are
  -- | written down at all — those are FS3X switches with no markings and no
  -- | LCD — which used to be the job of a legend under the loops. That legend
  -- | said the same six things less usefully, because you could not press it.
  twisterCard =
    HH.div [ HP.class_ (HH.ClassName "looper-side-card") ]
      [ cardHead "The Twister, live"
      , TwisterCard.render
          (TwisterCard.cardFor state.registry state.twisterScene
             state.focusPedalId state.twisterPage)
      ]

  -- | What the presses did, newest first.
  -- |
  -- | A log rather than the single line it replaced, because the sentence you
  -- | want is almost never the newest one — it is the two or three before the
  -- | press that surprised you, and a line that overwrites itself has thrown
  -- | those away by the time you look up.
  logCard =
    HH.div [ HP.class_ (HH.ClassName "looper-side-card") ]
      [ cardHead "What happened"
      , if Array.null state.looperLog
          then HH.p [ HP.class_ (HH.ClassName "looper-log-empty") ]
                 [ HH.text "Nothing pressed yet." ]
          else HH.ol [ HP.class_ (HH.ClassName "looper-log") ]
                 (map logLine state.looperLog)
      ]

  -- A repeat carries its count rather than a second identical line: pressing
  -- the same dead switch twice is two refusals and has to say so, but it does
  -- not have to say so twice as loudly as everything else.
  logLine l =
    HH.li [ HP.class_ (HH.ClassName "looper-log-line") ]
      ( [ HH.span_ [ HH.text l.text ] ]
          <> if l.times > 1
               then [ HH.span [ HP.class_ (HH.ClassName "looper-log-times") ]
                        [ HH.text ("\x00d7" <> show l.times) ] ]
               else []
      )

  -- The side cards' head. They are the page, not something laid over it, so
  -- there is nothing to close and no button here.
  cardHead title =
    HH.div [ HP.class_ (HH.ClassName "looper-panel-head") ]
      [ HH.span [ HP.class_ (HH.ClassName "looper-panel-title") ] [ HH.text title ] ]

  -- | A reference panel, laid over the page.
  -- |
  -- | **No title bar, since 2026-08-27.** It cost a whole band across the top
  -- | to name a panel you had just clicked a button to open, and what it pushed
  -- | below the fold was the grid you opened it for. Both bodies carry their own
  -- | headings, so nothing here was the only label for anything.
  -- |
  -- | The close moves to a floating corner button and the backdrop still
  -- | dismisses — a reference you cannot get rid of with the hand that opened it
  -- | is a reference you stop opening. `title` survives as the accessible name
  -- | rather than as furniture.
  -- | **The Edit panel: a window, a start, and a picture to set them by.**
  -- |
  -- | The daemon plays what the sliders say — an in point, an out point and
  -- | where a pass starts inside them — so what you hear while you drag is
  -- | what `Export` will write; nothing here is a preview of a different
  -- | renderer. Sliders rather than handles on the waveform: they are exact,
  -- | keyboard-nudgeable, and need no drag machinery. Steps are a beat when
  -- | the loop is on the grid, a frame when it is not.
  editPanel = case state.looper of
    Nothing -> HH.div_ [ HH.text "No daemon to edit on." ]
    Just top -> case Array.index top.loops state.looperFocus of
      Nothing -> HH.text ""
      Just lp
        | lp.loopFrames <= 0 -> HH.div_ [ HH.text "This loop has no length yet; record something first." ]
        | otherwise -> editBody top lp

  editBody top lp =
    HH.div [ HP.class_ (HH.ClassName "looper-edit") ]
      [ waveform
      -- **Past the ends is silence, and allowed.** In can go a whole loop
      -- before zero and Out a whole loop past the length; what that adds is
      -- rest, and it is how a loop grows room without a crop or a re-take.
      , sliderRow "in" "In" (negate len) (winO - 1) winI (\v -> h.windowIn li v) (timeWord winI <> (if winI < 0 then " (rest before)" else ""))
      , sliderRow "out" "Out" (winI + 1) (2 * len) winO (\v -> h.windowOut li v) (timeWord winO <> (if winO > len then " (rest after)" else ""))
      , sliderRow "rot" "Start" 0 (max 0 (span - 1)) lp.rot (\v -> h.shiftStart li (v - lp.rot))
          (timeWord (winI + lp.rot) <> " into the loop")
      , HH.div [ HP.class_ (HH.ClassName "looper-edit-actions") ]
          [ HH.button [ HP.class_ (HH.ClassName "looper-help-btn"), HE.onClick \_ -> h.shiftStart li (negate step) ] [ HH.text ("⟵ " <> stepWord) ]
          , HH.button [ HP.class_ (HH.ClassName "looper-help-btn"), HE.onClick \_ -> h.shiftStart li step ] [ HH.text (stepWord <> " ⟶") ]
          , HH.button [ HP.class_ (HH.ClassName "looper-help-btn"), HE.onClick \_ -> h.clearWindow li ] [ HH.text "Whole loop" ]
          , HH.button [ HP.class_ (HH.ClassName "looper-help-btn"), HE.onClick \_ -> h.askPeaks li ] [ HH.text "Redraw" ]
          , HH.span [ HP.class_ (HH.ClassName "looper-edit-note") ]
              [ HH.text (if windowed then "Windowed: the daemon will not record into this loop until it plays whole again." else "The whole loop.") ]
          ]
      ]
    where
    li = state.looperFocus
    len = lp.loopFrames
    windowed = lp.winOut > 0
    winI = if windowed then lp.winIn else 0
    winO = if windowed then lp.winOut else len
    span = max 1 (winO - winI)
    -- A beat, from the bar the rig reports, when the loop is on the grid;
    -- otherwise a frame, which is the exact thing.
    beat = if top.barFrames > 0 && top.linkQuantum > 0.0
      then max 1 (Int.round (Int.toNumber top.barFrames / top.linkQuantum))
      else 1
    step = if lp.quant then beat else 1
    stepWord = if step == 1 then "1 frame" else "1 beat"
    timeWord f = secsOf f <> " s"
    secsOf f = show (Int.toNumber (Int.round (Int.toNumber f / Int.toNumber top.sampleRate * 1000.0)) / 1000.0)

    -- The slider shows the hand's value while the hand is on it (see
    -- `looperEditLocal`) and the snapshot's otherwise.
    sliderRow key label lo hi v onV word =
      HH.div [ HP.class_ (HH.ClassName "looper-edit-row") ]
        [ HH.span [ HP.class_ (HH.ClassName "looper-edit-label") ] [ HH.text label ]
        , HH.input
            [ HP.type_ HP.InputRange
            , HP.class_ (HH.ClassName "looper-edit-slider")
            , HP.min (Int.toNumber lo)
            , HP.max (Int.toNumber hi)
            , HP.step (HP.Step (Int.toNumber step))
            , HP.value (show (fromMaybe v (Map.lookup key state.looperEditLocal)))
            , HE.onValueInput \raw -> onV (maybe v identity (Int.fromString raw))
            , HE.onValueChange \_ -> h.editDone key
            ]
        , HH.span [ HP.class_ (HH.ClassName "looper-edit-word") ] [ HH.text word ]
        ]

    -- The picture: the whole loop, always, with the window shaded, the
    -- start marked, and the playhead where the daemon says it is. Drawn from
    -- the last `peaks` answer; a stale one for another loop is not drawn.
    waveform = case state.looperPeaks of
      Just pk | pk.loop == li && pk.buckets > 0 ->
        let
          w = Int.toNumber pk.buckets
          -- The picture spans `from..to`, which is the loop plus the
          -- silence the window reaches into; positions map onto that.
          x f = Int.toNumber (f - pk.from) / Int.toNumber (max 1 (pk.to - pk.from)) * w
          y v = 100.0 - Int.toNumber v / 10.0
          pt i v = show (Int.toNumber i) <> "," <> show (y v)
          top' = joinWith " " (Array.mapWithIndex pt pk.hi)
          -- Positions in the picture are bucket indices, so the polygon is
          -- already in place; only the rects and lines need `x`.
          bot = joinWith " " (Array.reverse (Array.mapWithIndex pt pk.lo))
          svgEl name = HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName name)
          sAttr n v = HP.attr (AttrName n) v
        in
          svgEl "svg"
            [ sAttr "viewBox" ("0 0 " <> show pk.buckets <> " 200")
            , sAttr "preserveAspectRatio" "none"
            , sAttr "class" "looper-wave"
            ]
            -- Drawn in this order so the feedback is unmissable: the loop's
            -- extent in white, the audio, then everything *outside* the
            -- window dimmed hard on top of it, the two ends as lines, the
            -- start in red, and the playhead — which only ever moves inside
            -- the window, which is the other half of the feedback.
            [ svgEl "rect" [ sAttr "x" (show (x 0)), sAttr "y" "0", sAttr "width" (show (x len - x 0)), sAttr "height" "200", sAttr "class" "looper-wave-loop" ] []
            , svgEl "polygon" [ sAttr "points" (top' <> " " <> bot), sAttr "class" "looper-wave-body" ] []
            , svgEl "rect" [ sAttr "x" "0", sAttr "y" "0", sAttr "width" (show (x winI)), sAttr "height" "200", sAttr "class" "looper-wave-outside" ] []
            , svgEl "rect" [ sAttr "x" (show (x winO)), sAttr "y" "0", sAttr "width" (show (w - x winO)), sAttr "height" "200", sAttr "class" "looper-wave-outside" ] []
            , svgEl "line" [ sAttr "x1" (show (x winI)), sAttr "x2" (show (x winI)), sAttr "y1" "0", sAttr "y2" "200", sAttr "class" "looper-wave-edge" ] []
            , svgEl "line" [ sAttr "x1" (show (x winO)), sAttr "x2" (show (x winO)), sAttr "y1" "0", sAttr "y2" "200", sAttr "class" "looper-wave-edge" ] []
            , svgEl "line" [ sAttr "x1" (show (x (winI + lp.rot))), sAttr "x2" (show (x (winI + lp.rot))), sAttr "y1" "0", sAttr "y2" "200", sAttr "class" "looper-wave-start" ] []
            , svgEl "line" [ sAttr "x1" (show (x lp.pos)), sAttr "x2" (show (x lp.pos)), sAttr "y1" "0", sAttr "y2" "200", sAttr "class" "looper-wave-head" ] []
            ]
      _ -> HH.div [ HP.class_ (HH.ClassName "looper-wave-missing") ] [ HH.text "Waveform on its way…" ]

  modal klass title body =
    HH.div [ HP.class_ (HH.ClassName "looper-modal-overlay") ]
      [ HH.div
          [ HP.class_ (HH.ClassName "looper-modal-backdrop")
          , HE.onClick \_ -> h.openPanel Nothing
          ]
          []
      , HH.div
          [ HP.class_ (HH.ClassName ("looper-modal " <> klass))
          , HP.attr (HH.AttrName "role") "dialog"
          , HP.attr (HH.AttrName "aria-label") title
          ]
          [ HH.button
              [ HP.class_ (HH.ClassName "looper-modal-close")
              , HP.attr (HH.AttrName "aria-label") "Close"
              , HE.onClick \_ -> h.openPanel Nothing
              ]
              [ HH.text "\x00D7" ]
          , HH.div [ HP.class_ (HH.ClassName "looper-modal-body") ] body
          ]
      ]

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
  -- | **The one place to look.** Bold, and at the top, because the question it
  -- | answers is not "what is this loop doing" — the slots answer that — but
  -- | "what is the rig set to", which was previously only answerable by opening
  -- | two panels and reading three encoders.
  -- |
  -- | Some of it repeats what a slot says. That is the point: a value you have
  -- | to go and find is a value you check once and then assume, and every one
  -- | of these is a thing that being wrong about costs a take.
  statusStrip =
    HH.div [ HP.class_ (HH.ClassName "looper-status") ]
      [ HH.div [ HP.class_ (HH.ClassName "looper-status-row") ] readings
      , HH.div [ HP.class_ (HH.ClassName "looper-help-row") ]
          [ panelBtn PanelEdit "Edit"
          , panelBtn PanelRecipes "Recipes"
          , panelBtn PanelTwister "Twister"
          , panelBtn PanelBanks "MC6 banks"
          -- **Behind a button rather than on the page**, since the Twister
          -- card took its slot. The device labels A-F on its own screen and
          -- G-L mean the same six things on every bank, so there is nothing
          -- here to look up any more. What is left is that its switches can be
          -- pressed from the app, which is how the board gets exercised
          -- without feet.
          , panelBtn PanelBoard "MC6 board"
          -- **Not a panel, so not a `panelBtn`.** The other three lay a
          -- reference over this page; this one opens a document in another tab
          -- so the reference can be on paper beside the rig while this tab
          -- keeps focus — which it has to, because a background tab stops
          -- handling Twister messages.
          , HH.button
              [ HP.class_ (HH.ClassName "looper-help-btn is-print")
              , HP.title "The four pages and the recipes, in a new tab, laid \
                         \out for A4. Print it or save it as a PDF from there."
              , HE.onClick \_ -> h.printSheet
              ]
              [ HH.text "Print sheet" ]
          ]
      ]

  readings = case state.looper of
    Nothing -> [ reading "rig" "no daemon" true ]
    Just lp ->
      let focused = Array.index lp.loops state.looperFocus
          bars = maybe 0 _.cycles focused
      in
        -- Tempo and metre first, because everything else is counted in them —
        -- and marked as an alarm when absent, since "no clock" is the one
        -- reading here that changes what every other one means.
        [ reading "tempo"
            (if lp.linkAnchors == 0 then "no clock"
             else show (Int.round lp.linkTempo) <> " bpm")
            (lp.linkAnchors == 0)
        , reading "metre"
            (if lp.linkAnchors == 0 then "—" else show (Int.round lp.linkQuantum) <> "/4")
            false
        , reading "bar"
            (if lp.barFrames == 0 then "none yet"
             else secs (Int.toNumber lp.barFrames / Int.toNumber lp.sampleRate)
                    <> (if lp.linkAnchors == 0 then " · from loop 1" else ""))
            (lp.barFrames == 0)
        -- **One-based, like every other surface.** The wire counts loops from
        -- zero and this does not; the two met in the log once and cost an hour.
        , reading "selected" ("loop " <> show (state.looperFocus + 1)) false
        -- **Which jack the selected loop is listening to.** It belongs beside
        -- `selected` because it is a fact about that loop and not about the
        -- rig: the source is per loop, a new loop starts on the first one, and
        -- there is nothing anywhere else on screen that says which. Getting it
        -- wrong costs a whole take and the take is silent, which is the most
        -- expensive way to find out.
        --
        -- Stereo or mono comes with the name, because that is the half of it
        -- this page can be wrong about without saying so: a mono source is one
        -- jack read twice, and a loop recorded from one will never have width
        -- however the balance knob is turned. The channel numbers stay in the
        -- ack, which names them every time the knob moves.
        , reading "input" (inputWords lp focused) false
        , reading "length" (if bars == 0 then "not set"
                            else show bars <> (if bars == 1 then " bar" else " bars")) false
        , reading "launch" (launchWords lp.launchQ) false
        , reading "grid" (if maybe false _.quant focused then "on" else "off") false
        , twisterReading
        ]

  -- | The selected loop's source, named as the daemon names it.
  -- |
  -- | `src` is **one-based** and indexes `sources`, so the subtraction is the
  -- | seam and is done in one place. A number with no entry is reported as the
  -- | number rather than silently blanked: it would mean the daemon and this
  -- | page disagree about how many inputs exist, which is worth seeing.
  inputWords lp focused = case focused of
    Nothing -> "\x2014"
    Just f -> case Array.index lp.sources (f.src - 1) of
      Nothing -> "source " <> show f.src
      Just src -> src.name <> (if src.mono then " (mono)" else " (stereo)")

  -- | **Which page the Twister is on, on the screen as well as under the hand.**
  -- |
  -- | The controller says it twice — the pager's ring position and its colour —
  -- | and both are on the device, which is no help at all when the thing you
  -- | are looking at is the screen. The eight loop encoders are deliberately in
  -- | the same eight positions on the Loops page and the Set page so the map is
  -- | learned once, and the cost of that is a page you can be on without
  -- | noticing: press Loop 8 on the Set page and it stops or starts rather than
  -- | selecting. That produced a bug report, and this is half the answer to it
  -- | (the other half is that acting on a loop now takes it in hand).
  -- |
  -- | The name comes from `TW.pages`, so a renamed page renames itself here.
  -- | Not an alarm: being on the Set page is an ordinary place to be. It earns
  -- | its space by being the one fact on this strip that is about the *hand*
  -- | rather than the rig.
  twisterReading =
    let name = maybe "—" _.name
                 (Array.find (\pg -> pg.bank == state.twisterPage) TW.pages)
    in reading "twister"
         ("page " <> show (state.twisterPage + 1) <> " · " <> name) false

  reading label value alarm =
    HH.div [ HP.class_ (HH.ClassName ("looper-reading" <> if alarm then " is-alarm" else "")) ]
      [ HH.span [ HP.class_ (HH.ClassName "looper-reading-label") ] [ HH.text label ]
      , HH.span [ HP.class_ (HH.ClassName "looper-reading-value") ] [ HH.text value ]
      ]

  -- In beats, because that is what the setting is; a bar is spelled as a bar
  -- rather than as four, since in 3/4 it is three.
  launchWords q = case q of
    -1 -> "the bar"
    0 -> "free"
    1 -> "1 beat"
    n -> show n <> " beats"

  secs n = show (Int.toNumber (Int.round (n * 100.0)) / 100.0) <> " s"

  -- **Silent when it is right**, which is the same rule the Twister card's
  -- status line follows. A green band saying the socket is open is correct all
  -- day and worth nothing; the strip above carries a `no daemon` reading for
  -- the case that matters, and this speaks only to say what to do about it.
  connectionLine =
    let stale = state.looperSnapshotAge > 1000.0
    in if st.connected && not stale then HH.text "" else HH.p
      [ HP.class_ (HH.ClassName "looper-conn down") ]
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
