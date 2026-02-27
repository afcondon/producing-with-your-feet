module Data.MC6
  ( SwitchId(..)
  , SwitchAction(..)
  , MC6Switch
  , MC6Bank
  , Waveform(..)
  , WaveformConfig
  , switchLabel
  , allSwitchIds
  , emptySwitches
  , emptyBank
  ) where

import Prelude

import Data.Array (replicate) as Array
import Data.Loopy (LoopyAction)
import Data.Midi (CC, Channel, MidiValue)
import Data.Pedal (PedalId)
import Data.Pedal.Engage (EngageState)
import Data.Preset (PresetId)

-- | Physical switch position A–I (6 MC6 + 3 FS3X)
data SwitchId
  = SwitchA
  | SwitchB
  | SwitchC
  | SwitchD
  | SwitchE
  | SwitchF
  | SwitchG -- FS3X
  | SwitchH -- FS3X
  | SwitchI -- FS3X

derive instance Eq SwitchId
derive instance Ord SwitchId

switchLabel :: SwitchId -> String
switchLabel = case _ of
  SwitchA -> "A"
  SwitchB -> "B"
  SwitchC -> "C"
  SwitchD -> "D"
  SwitchE -> "E"
  SwitchF -> "F"
  SwitchG -> "G"
  SwitchH -> "H"
  SwitchI -> "I"

allSwitchIds :: Array SwitchId
allSwitchIds =
  [ SwitchA, SwitchB, SwitchC, SwitchD, SwitchE, SwitchF
  , SwitchG, SwitchH, SwitchI
  ]

-- | MC6's built-in LFO waveform shapes
data Waveform
  = WaveOff
  | WaveSawUp
  | WaveSawDown
  | WaveTriangle
  | WaveSine
  | WaveSquare
  | WaveRandom

derive instance Eq Waveform

type WaveformConfig =
  { engineSlot :: Int -- 0–3
  , waveform :: Waveform
  , cc :: CC
  , channel :: Channel
  , min :: MidiValue
  , max :: MidiValue
  , cpm :: Int -- cycles per minute
  }

-- | What a switch does when pressed
data SwitchAction
  = RecallBoard PresetId
  | RecallPedalPreset PedalId PresetId
  | SendCC PedalId CC MidiValue
  | SendMomentary PedalId CC MidiValue
  | ToggleEngage PedalId EngageState
  | LoopyCommand LoopyAction
  | StartWaveform WaveformConfig
  | StopWaveform Int
  | BankJump Int
  | TogglePage
  | Unassigned

derive instance Eq SwitchAction

type MC6Switch =
  { label :: String
  , action :: SwitchAction
  }

type MC6Bank =
  { id :: String
  , name :: String
  , description :: String
  , bankNumber :: Int -- 0–29
  , switches :: Array MC6Switch -- length 9, indexed A–I
  , created :: String
  , modified :: String
  }

emptySwitches :: Array MC6Switch
emptySwitches = Array.replicate 9 { label: "", action: Unassigned }

emptyBank :: MC6Bank
emptyBank =
  { id: ""
  , name: ""
  , description: ""
  , bankNumber: 0
  , switches: emptySwitches
  , created: ""
  , modified: ""
  }
