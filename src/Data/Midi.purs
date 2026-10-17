-- | MIDI's vocabulary for this app: the range-checked numbers come from Helm
-- | (`Helm.Midi`, shared with Triggerfish) and are re-exported here so the
-- | sixty modules that import `Data.Midi` need not know; what a *pedal* control
-- | means by its range stays here.
module Data.Midi
  ( module Helm.Midi
  , ValueKind(..)
  , MidiMsg(..)
  ) where

import Helm.Midi (CC, Channel, MidiValue, ProgramNumber, controlChange, makeCC, makeChannel, makeMidiValue, makeProgramNumber, programChange, unCC, unChannel, unMidiValue, unProgramNumber, unsafeCC, unsafeChannel, unsafeMidiValue)

-- | How a control interprets the 0..127 range
data ValueKind
  = Continuous
  | Detente MidiValue
  | Discrete (Array { label :: String, value :: MidiValue })

-- | Core MIDI messages the app sends
data MidiMsg
  = ControlChange Channel CC MidiValue
  | ProgramChange Channel ProgramNumber
