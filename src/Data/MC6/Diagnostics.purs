-- | Generated MC6 banks for finding out what is actually wired up.
-- |
-- | A pedal that does not respond could be a bad cable, a TRS polarity mismatch
-- | on the breakout box, a wrong channel, or a pedal that is simply not
-- | listening. Distinguishing those means poking each pedal in turn, and doing
-- | that through a point-and-click editor twelve times is how an afternoon
-- | disappears.
-- |
-- | So: generate it. One switch per pedal, each toggling that pedal's bypass,
-- | laid out across as many banks as it takes. Stomp along the row and whatever
-- | stays silent is where the fault is.
module Data.MC6.Diagnostics
  ( bypassBanks
  , switchesPerBank
  , gestureProbeBank
  , gestureProbeChannel
  ) where

import Prelude

import Config.Registry (PedalRegistry, registryPedals)
import Data.Array as Array
import Data.MC6.Message as MC6Msg
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch, ccToggleMessages)
import Data.MC6.Types (MC6Action(..), MC6Message)
import Data.Midi (unCC)
import Data.Pedal (PedalDef)
import Data.Pedal.Engage (EngageConfig(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.String.CodeUnits as SCU

-- | The MC6 bank the app models is a 3×3 grid: six footswitches plus three
-- | two-switch combinations. One slot is spent on getting back out again.
switchesPerBank :: Int
switchesPerBank = 9

returnIndex :: Int
returnIndex = 8

pedalsPerBank :: Int
pedalsPerBank = returnIndex

-- | One bank per group of eight pedals, numbered from `firstBank`.
-- |
-- | Every switch is a toggle: press to bypass, press again to re-engage. That
-- | is the useful shape for a sweep, because you can walk the row and leave
-- | the board as you found it.
bypassBanks :: Int -> Int -> PedalRegistry -> Array ControlBank
bypassBanks firstBank returnBankNum reg =
  Array.mapWithIndex toBank (chunk pedalsPerBank (registryPedals reg))
  where
  toBank i pedals =
    { id: "diag-bypass-" <> show i
    , name: "Bypass test " <> show (i + 1)
    , description: "Generated: one switch per pedal bypass"
    , mc6BankNumber: firstBank + i
    , returnSwitchIndex: returnIndex
    , switches: pad (map pedalSwitch pedals)
    }

  -- The bank must have a slot for every position, or the return switch lands
  -- in the wrong place. Unused positions are written blank rather than left
  -- alone, so a regenerated bank does not keep whatever was there before.
  pad switches =
    Array.take returnIndex (switches <> Array.replicate pedalsPerBank blank)
      <> [ backSwitch ]

  blank = { label: "", longName: "", toToggle: false, messages: [] }
  -- Carries its own jump rather than relying on the compiler to fill it in, so
  -- a generated bank says what all nine of its switches do.
  backSwitch =
    { label: "< Back", longName: "Back to board bank", toToggle: false
    , messages: [ MC6Msg.bankJumpMessage returnBankNum ActionPress ] }

pedalSwitch :: PedalDef -> ControlBankSwitch
pedalSwitch def =
  { label: clip 8 def.meta.shortName
  , longName: clip 24 (def.meta.name <> " bypass")
  , toToggle: true
  , messages: engageMessages def.meta.defaultChannel def.engage
  }

-- | Dual-engage pedals get both sides toggled together.
-- |
-- | For a wiring test that is what you want: the question is whether the pedal
-- | hears anything at all, so hit both and watch for any response. It costs
-- | four messages of the sixteen available, which is affordable.
engageMessages :: Int -> EngageConfig -> Array MC6Message
engageMessages ch = case _ of
  SingleEngage cc -> ccToggleMessages ch (unCC cc)
  DualEngage { a, b } ->
    ccToggleMessages ch (unCC a.cc) <> ccToggleMessages ch (unCC b.cc)

-- | MC6 short names are 8 characters and long names 24; overrunning them is
-- | rejected by the device rather than truncated for you.
clip :: Int -> String -> String
clip n s = if String.length s <= n then s else SCU.take n s

chunk :: forall a. Int -> Array a -> Array (Array a)
chunk n xs
  | n <= 0 = [ xs ]
  | Array.null xs = []
  | otherwise = Array.cons (Array.take n xs) (chunk n (Array.drop n xs))

-- | A bank for finding out what the MC6 actually does with a gesture.
-- |
-- | **Written because two people disagreed and neither had looked.** The
-- | question is whether the device fires a switch's `Press` action when a
-- | second press follows inside its double-tap window, or withholds it. It
-- | decides something real: if the single always fires, then a switch cannot
-- | carry both Undo and Redo, because a double tap would send Undo and then
-- | Redo and land exactly where it started. If it withholds, the device can do
-- | the recognition and the app's own recogniser — with its orphan-release and
-- | phantom-hold failure modes — is unnecessary weight.
-- |
-- | Every action a switch can carry gets its own CC, so the answer is whatever
-- | arrives. Nothing here interprets anything; read the log.
-- |
-- | Layout, one question per switch:
-- |
-- |   * **A** — every action at once. Tap it, double it, hold it, and see which
-- |     of 100/101/102/103 appear and in what order.
-- |   * **B** — press and double only, so a double tap cannot hide behind a
-- |     release. If 110 arrives twice before 111, the single is not withheld.
-- |   * **C** — the release-side pair, which is what this app would actually
-- |     use if the device turned out to do the work.
-- |   * **D** — long press against its release, for the same question one
-- |     gesture over.
-- |
-- | On its own channel so nothing here can be mistaken for a pedal or for the
-- | looper's own switch namespace.
-- |
-- | **The CCs are decades, and they are all under 128.** The first version used
-- | 100/110/120/130, and 130 does not fit in seven bits: encoded into the preset
-- | frame it becomes `0x82`, which is a *status byte*, and a status byte inside
-- | a SysEx message ends the message. Switch D's frame was therefore malformed
-- | and took the rest of the upload session with it — D, E and F all silently
-- | never arrived while A, B and C landed perfectly. It looked exactly like a
-- | device that stops acking, and cost an hour of blaming the browser.
gestureProbeBank :: Int -> Int -> ControlBank
gestureProbeBank bankNum boardBank =
  { id: "gesture-probe"
  , name: "Gestures"
  , description: "Every gesture on its own CC, to settle what the device sends"
  , mc6BankNumber: bankNum
  , returnSwitchIndex: 5
  , switches:
      [ probe "A 60s" "Press 60 Rel 61 Dbl 62 Long 63"
          [ Tuple ActionPress 60
          , Tuple ActionRelease 61
          , Tuple ActionDoubleTap 62
          , Tuple ActionLongPress 63
          ]
      , probe "B 70s" "Press 70, DoubleTap 71"
          [ Tuple ActionPress 70, Tuple ActionDoubleTap 71 ]
      , probe "C 80s" "Release 80, DoubleTapRelease 81"
          [ Tuple ActionRelease 80, Tuple ActionDoubleTapRelease 81 ]
      , probe "D 90s" "LongPress 90, LongPressRelease 91"
          [ Tuple ActionLongPress 90, Tuple ActionLongPressRelease 91 ]
      , { label: "", longName: "", toToggle: false, messages: [] }
      , { label: "< Board", longName: "Back to the board bank", toToggle: false
        , messages: [ MC6Msg.bankJumpMessage boardBank ActionPress ]
        }
      ]
  }
  where
  probe :: String -> String -> Array (Tuple MC6Action Int) -> ControlBankSwitch
  probe label longName pairs =
    { label
    , longName
    , toToggle: false
    -- 127 every time: the value carries nothing, only the CC number and the
    -- order of arrival are being measured.
    , messages: map
        (\(Tuple action cc) -> MC6Msg.ccMessage gestureProbeChannel cc 127 action)
        pairs
    }

-- | Channel 4 — not the app's switch namespace and not a pedal's, so anything
-- | arriving here came from this bank and nowhere else.
gestureProbeChannel :: Int
gestureProbeChannel = 4
