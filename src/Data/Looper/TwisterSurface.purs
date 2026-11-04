-- | **Who the Twister is for**: the pedals, the looper, or a scene.
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
-- | The state **is** the owner: which surface may paint the rings now. That
-- | is what the hub's Twister ownership will read (plan
-- | `docs/kb/plans/quadrat-the-sampler.md`, phase 3), and why this module is
-- | Halogen-free: the looper's own process will run the same machine.
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
import Data.Maybe (Maybe(..))
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
  | PaintLooper
  | PaintScene

derive instance eqPaint :: Eq Paint

data Event
  = FocusPedal
  | FocusLooper
  | SceneOn
  | SceneOff

-- | A machine that has passed the alphabet check, and where it is.
newtype Surface = Surface { spec :: Spec, state :: StateId }

ownerName :: Owner -> String
ownerName = case _ of
  PedalPage -> "pedal"
  LooperPages -> "looper"
  ScenePage -> "scene"

paintName :: Paint -> String
paintName = case _ of
  PaintPedal -> "paint-pedal"
  PaintLooper -> "paint-looper"
  PaintScene -> "paint-scene"

eventName :: Event -> String
eventName = case _ of
  FocusPedal -> "focus-pedal"
  FocusLooper -> "focus-looper"
  SceneOn -> "scene-on"
  SceneOff -> "scene-off"

owners :: Array Owner
owners = [ PedalPage, LooperPages, ScenePage ]

paints :: Array Paint
paints = [ PaintPedal, PaintLooper, PaintScene ]

events :: Array String
events = map eventName [ FocusPedal, FocusLooper, SceneOn, SceneOff ]

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
      differ "states" (map ownerName owners) (map (\(StateId s) -> s) (stateIds spec))
        <> differ "events" events (map (\(EventId s) -> s) (eventIds spec))
        <> differ "commands" (map paintName paints) (map (\(CommandId s) -> s) (commandIds spec))
  if Array.null problems then Right (Surface { spec, state: spec.initial })
  else Left ("twister-surface.json does not fit this app. " <> intercalate "; " problems)

owner :: Surface -> Owner
owner (Surface s) = ownerOf s.state

-- | Total because `load` checked the alphabet; the fallback is unreachable.
ownerOf :: StateId -> Owner
ownerOf (StateId s) = case s of
  "looper" -> LooperPages
  "scene" -> ScenePage
  _ -> PedalPage

paintOf :: CommandId -> Maybe Paint
paintOf (CommandId c) = Array.find (\p -> paintName p == c) paints

-- | The paints the initial state owes before anything has happened.
start :: Surface -> Array Paint
start (Surface s) = Array.mapMaybe paintOf (Run.startCommands s.spec)

-- | Paint what the current owner shows, as arriving there would: the side
-- | button's refresh, and a device that has been somewhere else.
repaint :: Surface -> Array Paint
repaint (Surface s) = Array.mapMaybe paintOf (entryOf s.spec s.state)

-- | One event. `looperFocused` is the one fact the machine reads: where the
-- | Twister goes when a scene ends.
feed
  :: { looperFocused :: Boolean }
  -> Event
  -> Surface
  -> { surface :: Surface, paints :: Array Paint, refused :: Maybe String }
feed { looperFocused } ev (Surface s) =
  let
    world = Run.setFact (FactId "looper-focused") (VBoolean looperFocused) (Run.worldFrom s.spec)
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
