-- | The Midifighter's lights, and which page it is showing.
-- |
-- | ## Why these are not in `Component.App`
-- |
-- | They were, and they were the one part of that component that talks to a
-- | *device* rather than to the app. Everything here writes bytes at the
-- | Twister and forgets them; nothing here decides anything. Sitting among four
-- | thousand lines of MC6 protocol, board presets and pedal routing, that was
-- | invisible — and it mattered, because the rule these functions exist to
-- | enforce is easy to break by accident and impossible to see broken.
-- |
-- | ## The rule
-- |
-- | **The lights are painted from the daemon's snapshot, never from what the
-- | app just asked for.** A ring driven by the press that caused it is the app
-- | showing its own intention back to itself, and a refused command lights up
-- | exactly like an accepted one — which is the failure the whole ack path was
-- | built to end. `sendLooperLEDs` therefore takes `rigOf` and nothing else,
-- | and `rigOf` reads only `state.looper`, which is the daemon's word.
-- |
-- | ## Why they are polymorphic in the action and the slots
-- |
-- | Not generality for its own sake — it is the proof. A function whose type
-- | cannot mention `Action` cannot dispatch one, and a function whose type
-- | cannot mention `Slots` cannot query a child. So the compiler now enforces
-- | what the comments above used to merely assert: this module observes state
-- | and emits MIDI, and has no way to reach back into the app's decisions.
-- |
-- | It still takes `AppState` whole, which is the honest remaining coupling:
-- | the diff in `twisterLit` lives there because the app owns the wire, and
-- | splitting that out would buy a smaller type for a longer indirection.
module Component.Twister.Lights
  ( rigOf
  , refreshTwister
  , showTwisterPage
  , adoptTwisterPage
  , sendRingPosition
  , pinDevice
  , sendAllLEDs
  , sendLooperLEDs
  , dimAllLEDs
  , knobCC
  ) where

import Prelude

import Config.Registry as CRegistry
import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Looper as Looper
import Data.Looper.Machine as Machine
import Data.Looper.Twister as LoopTwister
import Data.Pedal (PedalId)
import Data.Twister (Knob)
import Data.Twister as TwisterData
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Engine (AppState)
import Engine.Twister as Twister
import Foreign.WebMIDI as MIDI
import Halogen as H

-- | Everything the machine is allowed to know, gathered from the newest
-- | snapshot.
-- |
-- | One place, because there are now three surfaces asking for it and a second
-- | copy of this expression is a second chance to forget a field — which is
-- | exactly how the click came to be sent as a flip.
rigOf :: AppState -> Machine.Rig
rigOf st =
  { loops: maybe [] _.loops st.looper
  , focus: st.looperFocus
  , click: maybe false _.click st.looper
  , monitor: maybe false _.monitor st.looper
  , armDb: maybe (-36.0) _.armDb st.looper
  }

-- | Hand the controller over to whatever is now in focus.
-- |
-- | **Dim first, always.** The two surfaces light different numbers of banks —
-- | a pedal uses one, the looper two — so walking from the looper to a pedal
-- | without clearing would leave the loop bank's colours burning under a page
-- | of knobs that know nothing about them. It costs one burst of messages on a
-- | focus change, which is not a thing that happens mid-take.
refreshTwister :: forall act slots o m. MonadAff m => PedalId -> H.HalogenM AppState act slots o m Unit
refreshTwister pid = do
  dimAllLEDs
  if Looper.isItajara pid
    then do
      -- **Hand it over on page 1.** Walking back to the looper and finding the
      -- controller still on the parameter page is walking back to a surface
      -- that looks like the loops and is not, which is worse than a page you
      -- have to reach for.
      --
      -- `showTwisterPage` rather than a bare block change, since 2026-08-27:
      -- the old line moved the device and left `twisterPage` wherever it had
      -- been, so after `dimAllLEDs` had zeroed every ring the app could believe
      -- it was on page 2 while the pager stood at the bottom of its travel. The
      -- next brush of that knob then read as page 1 and looked like a spurious
      -- page change. Now the page, the lights and the encoder are set together
      -- from one call, and zero is page one on all three.
      showTwisterPage 0
    else do
      -- `sendAllLEDs` has always addressed bank 0 and nothing ever put the
      -- device there, so walking from the looper to a pedal while the device
      -- sat on another block lit a page nobody was looking at. Pinning first
      -- makes the address true.
      pinDevice
      sendAllLEDs pid

-- | Turn to a page: the app moves, and the device does not.
-- |
-- | **The page is the app's, and it is now the app's alone.** Nothing about a
-- | page turn reaches the Twister except a repaint of the sixteen encoders it
-- | is already showing — see `Data.Twister.deviceBank` for why the device used to move with
-- | it and why that was the whole bug.
showTwisterPage :: forall act slots o m. MonadAff m => Int -> H.HalogenM AppState act slots o m Unit
showTwisterPage bank = do
  goToPage bank
  -- **Only here**, and that is the whole difference between this and
  -- `adoptTwisterPage`. The page moved without the knob, so the knob has to be
  -- carried to the band that now means what it is showing — otherwise the next
  -- turn is measured from wherever the hand last left it and the two disagree
  -- until something touches it.
  st <- H.get
  sendRingPosition { bank: TwisterData.deviceBank, index: LoopTwister.pagerIndex }
    (LoopTwister.pagerRing st.twisterPage)

-- | The page changed because the pager was turned there.
-- |
-- | Everything `showTwisterPage` does except move the encoder — which is
-- | already where it needs to be, because it is what moved. Writing to it here
-- | would be the app shoving a knob that a hand is holding.
-- |
-- | That sentence was *false* for a day, and only `Data.Twister.deviceBank` makes it true
-- | again: while the app moved the device between blocks, the knob that moved
-- | and the knob the device would read next were two different stores.
adoptTwisterPage :: forall act slots o m. MonadAff m => Int -> H.HalogenM AppState act slots o m Unit
adoptTwisterPage = goToPage

goToPage :: forall act slots o m. MonadAff m => Int -> H.HalogenM AppState act slots o m Unit
goToPage bank = do
  -- Clamped rather than wrapped: the pager is a position now, and a position
  -- has ends. See `LoopTwister.pageFor`.
  H.modify_ _
    { twisterPage = clamp 0 (LoopTwister.pages' - 1) bank
    , twisterLit = Map.empty
    }
  -- The page is entirely the app's; the device is only told to stay put. The
  -- repaint below is what a page turn *is* now, and it costs what the old bank
  -- change already cost, because clearing `twisterLit` forced a full repaint
  -- either way.
  pinDevice
  st <- H.get
  when (st.focusPedalId == Just Looper.itajaraId) sendLooperLEDs

-- | Put the device back on `deviceBank`, wherever it thought it was.
-- |
-- | Sent on every page turn and whenever the device is heard speaking from
-- | somewhere else, which is what makes the pin self-healing rather than an
-- | assumption: if a block button is pressed, the next message says so and this
-- | undoes it. See `Data.Twister.bankSelectMessage` — the message is still a
-- | request, but now there is only ever one thing being requested.
pinDevice :: forall act slots o m. MonadAff m => H.HalogenM AppState act slots o m Unit
pinDevice = do
  st <- H.get
  for_ st.connections.twisterOutput \out ->
    liftEffect $ MIDI.send out (TwisterData.bankSelectMessage TwisterData.deviceBank)

-- LED feedback helpers

-- | The ring, and the colour. Both addressed by `16 * bank + index`, which is
-- | the same arithmetic the device used on the way in.
sendRingPosition :: forall act slots o m. MonadAff m => Knob -> Int -> H.HalogenM AppState act slots o m Unit
sendRingPosition knob val = do
  st <- H.get
  for_ st.connections.twisterOutput \out ->
    liftEffect $ MIDI.send out [ 0xB0, knobCC knob, val ]

sendRGBColor :: forall act slots o m. MonadAff m => Knob -> Int -> H.HalogenM AppState act slots o m Unit
sendRGBColor knob hue = do
  st <- H.get
  for_ st.connections.twisterOutput \out ->
    liftEffect $ MIDI.send out [ 0xB1, knobCC knob, hue ]

knobCC :: Knob -> Int
knobCC knob = knob.bank * TwisterData.encodersPerBank + knob.index

sendAllLEDs :: forall act slots o m. MonadAff m => PedalId -> H.HalogenM AppState act slots o m Unit
sendAllLEDs pid = do
  st <- H.get
  case CRegistry.findPedal st.registry pid of
    Nothing -> pure unit
    Just def -> case Map.lookup pid st.engine of
      Nothing -> pure unit
      Just ps -> do
        let leds = Twister.computeAllLEDs def ps
        for_ leds \led -> do
          sendRGBColor { bank: 0, index: led.index } led.hue
          sendRingPosition { bank: 0, index: led.index } led.ring

-- | Every light on the controller, from the daemon's newest word.
-- |
-- | **Both banks every time, and diffed against what was last sent.** All of it
-- | is computed from the snapshot — nothing here reads what the app asked for —
-- | so the rings cannot drift from the engine no matter who moved a value: the
-- | console, a footswitch, or another client entirely.
-- |
-- | The diff is not an optimisation for its own sake. Bank one's rings are
-- | playheads, so they move every frame while a loop turns; the rest change
-- | only when something happens, and sending all 64 lights ten times a second
-- | would put 1,280 messages a second on a wire that also carries the pedals.
sendLooperLEDs :: forall act slots o m. MonadAff m => H.HalogenM AppState act slots o m Unit
sendLooperLEDs = do
  st <- H.get
  -- **Content from the app's page, address from `deviceBank`** — and the second
  -- half of that is the correction (2026-08-27). The address was `twisterPage`,
  -- and before that `twisterHeardBank`, both on the theory that the lights
  -- should land on whichever block the device was showing. There is no such
  -- question now: the device is only ever on one.
  let wanted = LoopTwister.leds (rigOf st) st.twisterPage
      changed = Array.filter (\l -> Map.lookup l.index st.twisterLit /= Just { ring: l.ring, hue: l.hue }) wanted
  unless (Array.null changed) do
    for_ changed \l -> do
      let knob = { bank: TwisterData.deviceBank, index: l.index }
      sendRGBColor knob l.hue
      -- The pager's ring is the device's — see `LoopTwister.pagerRing`. Writing
      -- it from the diff would move the page selector every time any colour on
      -- the page changed.
      unless l.ringHeld $ sendRingPosition knob l.ring
    H.modify_ \s -> s
      { twisterLit = Array.foldl
          (\m l -> Map.insert l.index { ring: l.ring, hue: l.hue } m)
          s.twisterLit
          changed
      }

dimAllLEDs :: forall act slots o m. MonadAff m => H.HalogenM AppState act slots o m Unit
dimAllLEDs = do
  for_ (Array.range 0 (TwisterData.banks - 1)) \bank ->
    for_ (Array.range 0 (TwisterData.encodersPerBank - 1)) \index -> do
      sendRGBColor { bank, index } 0
      sendRingPosition { bank, index } 0
  -- What was lit is no longer true of the device, so the diff must forget it or
  -- the next refresh will skip the lights it thinks are already right.
  H.modify_ _ { twisterLit = Map.empty }
