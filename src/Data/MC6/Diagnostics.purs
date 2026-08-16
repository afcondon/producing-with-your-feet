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
    , sharedOverrides: []
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
