module Data.Midi
  ( Channel
  , makeChannel
  , unChannel
  , CC
  , makeCC
  , unsafeCC
  , unCC
  , MidiValue
  , makeMidiValue
  , unsafeMidiValue
  , unMidiValue
  , ProgramNumber
  , makeProgramNumber
  , unProgramNumber
  , ValueKind(..)
  , MidiMsg(..)
  ) where

import Prelude

import Data.Maybe (Maybe(..))

-- | MIDI channel 1..16
newtype Channel = Channel Int

derive newtype instance Eq Channel
derive newtype instance Ord Channel
derive newtype instance Show Channel

makeChannel :: Int -> Maybe Channel
makeChannel n
  | n >= 1 && n <= 16 = Just (Channel n)
  | otherwise = Nothing

unChannel :: Channel -> Int
unChannel (Channel n) = n

-- | MIDI CC number 0..127
newtype CC = CC Int

derive newtype instance Eq CC
derive newtype instance Ord CC
derive newtype instance Show CC

makeCC :: Int -> Maybe CC
makeCC n
  | n >= 0 && n <= 127 = Just (CC n)
  | otherwise = Nothing

-- | Unsafe CC constructor for static pedal definitions where values are known at compile time
unsafeCC :: Int -> CC
unsafeCC = CC

unCC :: CC -> Int
unCC (CC n) = n

-- | MIDI value 0..127
newtype MidiValue = MidiValue Int

derive newtype instance Eq MidiValue
derive newtype instance Ord MidiValue
derive newtype instance Show MidiValue

makeMidiValue :: Int -> Maybe MidiValue
makeMidiValue n
  | n >= 0 && n <= 127 = Just (MidiValue n)
  | otherwise = Nothing

-- | Unsafe MidiValue constructor for static pedal definitions
unsafeMidiValue :: Int -> MidiValue
unsafeMidiValue = MidiValue

unMidiValue :: MidiValue -> Int
unMidiValue (MidiValue n) = n

-- | MIDI program number 0..127
newtype ProgramNumber = ProgramNumber Int

derive newtype instance Eq ProgramNumber
derive newtype instance Ord ProgramNumber
derive newtype instance Show ProgramNumber

makeProgramNumber :: Int -> Maybe ProgramNumber
makeProgramNumber n
  | n >= 0 && n <= 127 = Just (ProgramNumber n)
  | otherwise = Nothing

unProgramNumber :: ProgramNumber -> Int
unProgramNumber (ProgramNumber n) = n

-- | How a control interprets the 0..127 range
data ValueKind
  = Continuous
  | Detente MidiValue
  | Discrete (Array { label :: String, value :: MidiValue })

-- | Core MIDI messages the app sends
data MidiMsg
  = ControlChange Channel CC MidiValue
  | ProgramChange Channel ProgramNumber
