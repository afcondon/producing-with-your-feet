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
  , sendRingPosition
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
      -- have to reach for. Unverified — see `Data.Twister.bankSelectMessage`;
      -- if it does nothing this line is simply inert.
      sendTwisterBank 0
      sendLooperLEDs
    else sendAllLEDs pid

-- | Turn to a page: the app moves, and the device is invited.
-- |
-- | **The app's move is the one that matters.** `sendTwisterBank` is a courtesy
-- | — if the device honours it the two stay in step and the LEDs land on the
-- | block it is showing; if it ignores it, the app pages anyway and the LEDs go
-- | to the block the device *is* on, carrying the new page's content. Either
-- | way the encoders mean what the card says they mean.
showTwisterPage :: forall act slots o m. MonadAff m => Int -> H.HalogenM AppState act slots o m Unit
showTwisterPage bank = do
  -- Wrapping rather than clamping: a selector whose ends are dead is a selector
  -- you have to look at, and the ring must be rewritten on every turn or the
  -- encoder stays off its band and the next notch is measured from the wrong
  -- place.
  H.modify_ _
    { twisterPage = (bank + LoopTwister.pages') `mod` LoopTwister.pages'
    , twisterLit = Map.empty
    }
  sendTwisterBank bank
  st <- H.get
  when (st.focusPedalId == Just Looper.itajaraId) sendLooperLEDs

-- | Ask the device to show a page. See `Data.Twister.bankSelectMessage` for why
-- | this is a candidate rather than a fact.
sendTwisterBank :: forall act slots o m. MonadAff m => Int -> H.HalogenM AppState act slots o m Unit
sendTwisterBank bank = do
  st <- H.get
  for_ st.connections.twisterOutput \out ->
    liftEffect $ MIDI.send out (TwisterData.bankSelectMessage bank)

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
  -- **Both from the app's page**, and that is a correction: the address used to
  -- come from `twisterHeardBank`, the last block the device had spoken from,
  -- on the theory that the lights should land where the device really is even
  -- if it would not take a bank change.
  --
  -- It was wrong, and how it was wrong is the useful part. Turning the page
  -- from the web interface left the Twister unchanged **until you touched a
  -- knob** — which is precisely what you would see if the device *had* moved:
  -- it went to the new block, the lights went to the old one, and nothing
  -- corrected until a message arrived carrying the real bank. So the delay was
  -- the evidence that `bankSelectMessage` works.
  --
  -- Asking and then assuming is safe here because it is self-healing: if the
  -- device ever does not comply, its next message carries a different bank,
  -- `twisterPage` adopts it, `twisterLit` clears, and the following poll paints
  -- the right block. One touch, not one session.
  let wanted = LoopTwister.leds (rigOf st) st.twisterPage
      showing = st.twisterPage
      changed = Array.filter (\l -> Map.lookup l.index st.twisterLit /= Just { ring: l.ring, hue: l.hue }) wanted
  unless (Array.null changed) do
    for_ changed \l -> do
      let knob = { bank: showing, index: l.index }
      sendRGBColor knob l.hue
      sendRingPosition knob l.ring
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
