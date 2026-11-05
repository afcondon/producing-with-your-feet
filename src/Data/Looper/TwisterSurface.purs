-- | **Who should hold the Twister**: the pedals, a scene, or the looper.
-- |
-- | The answer is a Glassbox machine, `static/machines/twister-surface.json`,
-- | loaded at runtime and stepped by `Glassbox.Run.step`; this module is the
-- | host's half of the seam. It holds no rule of its own: which state follows
-- | which event is the artifact's, and changing it is an edit to the file.
-- |
-- | What it does hold is **the alphabet**. The artifact's states, events and
-- | commands must be exactly the ones below, checked once at load, so a
-- | mis-edited file is a load error with the difference named rather than a
-- | Twister that quietly stops painting (`reference_glassbox_pin_the_alphabet`:
-- | pin the alphabet, never the behaviour).
-- |
-- | The state says who **should** hold the Twister. Who **does** is Helm's
-- | hub: its claim on the Twister's output is the record, and nobody paints a
-- | Twister they do not hold. The entry commands close the gap: PWYF asks for
-- | it, or hands it to the looper's own process (plan
-- | `docs/kb/plans/quadrat-the-sampler.md`, phase 3; the table both programs
-- | read is `Data.Looper.TwisterHolder` in itajara/controllers).
-- |
-- | Data stays with the host: which pedal is focused, which scene, which of
-- | the looper's pages is showing.
module Data.Looper.TwisterSurface
  ( Surface
  , Owner(..)
  , Paint(..)
  , Event(..)
  , load
  , owner
  , feed
  , repaint
  , start
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Glassbox.Codec.JSON (parseSpec)
import Glassbox.Lint as Lint
import Glassbox.Run as Run
import Glassbox.Spec (CommandId(..), EventId(..), FactId(..), Outcome(..), RefusalId(..), Spec, StateId(..), Value(..), commandIds, entryOf, eventIds, stateIds)

data Owner
  = PedalPage
  | LooperPages
  | ScenePage

derive instance eqOwner :: Eq Owner

-- | The entry commands: what arriving in a state owes the device.
data Paint
  = PaintPedal
  | HandToLooper
  | PaintScene

derive instance eqPaint :: Eq Paint

data Event
  = FocusPedal
  | FocusLooper
  -- | Where the MC6 stands: on a scene's bank, on one of the looper's, or on
  -- | one that names no holder.
  | BankScene
  | BankLooper
  | BankPlain

-- | A machine that has passed the alphabet check, and where it is.
newtype Surface = Surface { spec :: Spec, state :: StateId }

-- | Every state, and whose it is. Two are the looper's: by focus, and by
-- | the MC6's bank (where focus changes nothing).
states :: Array (Tuple String Owner)
states =
  [ Tuple "pedal" PedalPage
  , Tuple "looper" LooperPages
  , Tuple "scene" ScenePage
  , Tuple "looper-bank" LooperPages
  ]

paintName :: Paint -> String
paintName = case _ of
  PaintPedal -> "paint-pedal"
  HandToLooper -> "hand-to-looper"
  PaintScene -> "paint-scene"

eventName :: Event -> String
eventName = case _ of
  FocusPedal -> "focus-pedal"
  FocusLooper -> "focus-looper"
  BankScene -> "bank-scene"
  BankLooper -> "bank-looper"
  BankPlain -> "bank-plain"

paints :: Array Paint
paints = [ PaintPedal, HandToLooper, PaintScene ]

events :: Array String
events = map eventName [ FocusPedal, FocusLooper, BankScene, BankLooper, BankPlain ]

-- | The facts `feed` sets, every one of them, on every event.
facts :: Array String
facts = [ "pedal-focused" ]

-- | Parse the artifact, refuse it if Glassbox's lint does (a state owing a
-- | command nobody declared, a rule missing, a state nothing reaches), and
-- | check its alphabet against this host's.
load :: String -> Either String Surface
load text = do
  spec <- parseSpec text
  let
    rejected = Lint.rejections (Lint.lint spec)
  unless (Array.null rejected) $
    Left ("twister-surface.json fails Glassbox's lint: " <> intercalate "; " (map _.message rejected))
  let
    differ what mine theirs =
      let
        missing = Array.filter (\x -> not (Array.elem x theirs)) mine
        extra = Array.filter (\x -> not (Array.elem x mine)) theirs
      in
        if Array.null missing && Array.null extra then []
        else
          [ what <> ": the machine "
              <> (if Array.null missing then "" else "lacks " <> intercalate ", " missing)
              <> (if Array.null missing || Array.null extra then "" else " and ")
              <> (if Array.null extra then "" else "has " <> intercalate ", " extra <> " this app does not know")
          ]
    problems =
      differ "states" (map (\(Tuple n _) -> n) states) (map (\(StateId s) -> s) (stateIds spec))
        <> differ "events" events (map (\(EventId s) -> s) (eventIds spec))
        <> differ "commands" (map paintName paints) (map (\(CommandId s) -> s) (commandIds spec))
        -- Facts too: a guard on a fact this host never sets reads false, and
        -- the catch-all arm fires with no complaint (review 2026-11-05).
        <> differ "facts" facts (map (\f -> case f.id of FactId s -> s) spec.facts)
  if Array.null problems then Right (Surface { spec, state: spec.initial })
  else Left ("twister-surface.json does not fit this app. " <> intercalate "; " problems)

owner :: Surface -> Owner
owner (Surface s) = ownerOf s.state

-- | Total because `load` checked the alphabet; the fallback is unreachable.
ownerOf :: StateId -> Owner
ownerOf (StateId s) = maybe PedalPage (\(Tuple _ o) -> o) (Array.find (\(Tuple n _) -> n == s) states)

paintOf :: CommandId -> Maybe Paint
paintOf (CommandId c) = Array.find (\p -> paintName p == c) paints

-- | The paints the initial state owes before anything has happened.
start :: Surface -> Array Paint
start (Surface s) = Array.mapMaybe paintOf (Run.startCommands s.spec)

-- | Paint what the current owner shows, as arriving there would: the side
-- | button's refresh, and a device that has been somewhere else.
repaint :: Surface -> Array Paint
repaint (Surface s) = Array.mapMaybe paintOf (entryOf s.spec s.state)

-- | One event. `pedalFocused` (a pedal other than the looper) is the one fact
-- | the machine reads: where the Twister goes when the MC6 leaves a bank that
-- | named a holder. With none, it goes to the looper.
feed
  :: { pedalFocused :: Boolean }
  -> Event
  -> Surface
  -> { surface :: Surface, paints :: Array Paint, refused :: Maybe String }
feed { pedalFocused } ev (Surface s) =
  let
    world = Run.setFact (FactId "pedal-focused") (VBoolean pedalFocused) (Run.worldFrom s.spec)
    Tuple next stepped = Run.step s.spec world s.state (EventId (eventName ev))
    refused = case stepped.outcome of
      Refuse (RefusalId r) -> Just (textOf r)
      _ -> Nothing
    textOf r = case Array.find (\d -> d.id == RefusalId r) s.spec.refusals of
      Just d -> d.text
      Nothing -> r
  in
    { surface: Surface (s { state = next })
    , paints: Array.mapMaybe paintOf stepped.commands
    , refused
    }
