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
-- | question was whether the device fires a switch's `Press` action when a
-- | second press follows inside its double-tap window, or withholds it. It
-- | decided something real: if the single always fires, a switch cannot carry
-- | both Undo and Redo, because a double tap would send Undo and then Redo and
-- | land exactly where it started.
-- |
-- | Every action a switch can carry gets its own CC, so the answer is whatever
-- | arrives. Nothing here interprets anything; read the log.
-- |
-- | ## What it answered, 2026-08-21
-- |
-- | ```
-- | single tap   Press and Release arrive 1 ms apart  (deferred, not at press-down)
-- | double tap   DoubleTap alone — no Press at all, three trials of three
-- | double tap   DoubleTapRelease alone — Release suppressed too
-- | long press   Press, then LongPress ~600 ms later, and no Release
-- | window       under 414 ms: two presses that far apart read as two singles
-- | ```
-- |
-- | The device withholds the single until it knows. So `Release`,
-- | `DoubleTapRelease` and `LongPress` are a mutually exclusive triple, the app
-- | stopped recognising gestures, and `Data.Looper.Gestures` was deleted.
-- |
-- | ## The open question, and why it decides a redesign
-- |
-- | **Is the deferral global, or only on a switch that carries a double?**
-- |
-- | Every switch above has a double-tap message bound or is multi-bound, so
-- | none of them can answer it. But the answer decides how the whole looper
-- | surface should be laid out. A long press does *not* force a wait — a hold
-- | starts as a press, which is why D sees `Press` and then `LongPress`. Only
-- | the double is ambiguous at press-down. So if the device defers per binding
-- | rather than globally, **a loop switch carrying nothing but a press is
-- | instantaneous**, and moving the double and the hold onto a page of their own
-- | buys back every millisecond the recogniser used to cost.
-- |
-- | Switch **E** is that experiment, and it needs no timing gear:
-- |
-- | > Press E and **hold your foot down for a slow two**. If `40` appears in the
-- | > log while the switch is still down, the press fired at press-down and the
-- | > deferral is per-binding. If nothing appears until you lift — and then `40`
-- | > and `41` arrive together — it is global.
-- |
-- | Do the same on **A** as the control: A carries a double, so it must defer.
-- | If A waits and E does not, the answer is per-binding.
-- |
-- | The **double-tap window is also still unpinned** — only bounded at 414 ms —
-- | and `Engine.looperDeferral` is guessing at it. Two presses at a known
-- | spacing against C would settle that one.
-- |
-- | Layout, one question per switch:
-- |
-- |   * **A** — every action at once. Tap it, double it, hold it, and see which
-- |     of 60/61/62/63 appear and in what order. Also the control for E.
-- |   * **B** — press and double only, so a double tap cannot hide behind a
-- |     release. If 70 arrives twice before 71, the single is not withheld.
-- |   * **C** — the release-side pair, which is what this app actually uses.
-- |   * **D** — long press against its release, for the same question one
-- |     gesture over.
-- |   * **E** — press and release and **nothing else**, which is the shape a
-- |     redesigned loop switch would have. The one switch here with no
-- |     ambiguity for the device to resolve.
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
      [ probe "A 60s" "P60 R61 D62 L63"
          [ Tuple ActionPress 60
          , Tuple ActionRelease 61
          , Tuple ActionDoubleTap 62
          , Tuple ActionLongPress 63
          ]
      , probe "B 70s" "Press 70, DoubleTap 71"
          [ Tuple ActionPress 70, Tuple ActionDoubleTap 71 ]
      , probe "C 80s" "Rel 80, DblRel 81"
          [ Tuple ActionRelease 80, Tuple ActionDoubleTapRelease 81 ]
      , probe "D 90s" "Long 90, LongRel 91"
          [ Tuple ActionLongPress 90, Tuple ActionLongPressRelease 91 ]
      -- Nothing on the double and nothing on the hold, so the device has no
      -- ambiguity to resolve. Hold it down for a slow two: if 40 arrives before
      -- your foot lifts, a press-only switch is instantaneous.
      , probe "E 40s" "Press 40 Rel 41, no more"
          [ Tuple ActionPress 40, Tuple ActionRelease 41 ]
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
