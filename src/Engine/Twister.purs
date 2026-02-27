module Engine.Twister
  ( handleEncoder
  , handleButton
  , handleSideButton
  , handleSideButtonPrev
  , computeAllLEDs
  , pedalHue
  , ringValueForEncoder
  ) where

import Prelude

import Data.Array as Array
import Data.Int (round, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, unMidiValue, unsafeMidiValue)
import Data.Pedal (PedalDef, PedalId)
import Data.Twister (TwisterButton(..), TwisterEncoder(..))
import Engine (PedalState)

intAbs :: Int -> Int
intAbs n = if n < 0 then negate n else n

type EncoderResult =
  { cc :: CC
  , value :: MidiValue
  , ringSnap :: Maybe Int
  }

handleEncoder
  :: Int -> Int -> PedalDef -> PedalState
  -> Maybe EncoderResult
handleEncoder encoderIndex rawValue def _ps = do
  tw <- def.twister
  mEnc <- Array.index tw.encoders encoderIndex
  enc <- mEnc
  case enc of
    TwisterCC { cc, center: Nothing, options: Nothing } ->
      Just { cc, value: unsafeMidiValue rawValue, ringSnap: Nothing }

    TwisterCC { cc, center: Just c, options: Nothing } ->
      let centerVal = unMidiValue c
          snapped = if intAbs (rawValue - centerVal) <= 2 then centerVal else rawValue
      in Just { cc, value: unsafeMidiValue snapped, ringSnap: Nothing }

    TwisterCC { cc, center: _, options: Just opts } ->
      let n = Array.length opts
      in if n == 0 then Nothing
         else do
           let idx = round (toNumber rawValue / 127.0 * toNumber (n - 1))
               clampedIdx = clamp 0 (n - 1) idx
           optVal <- Array.index opts clampedIdx
           let ringPos = if n <= 1 then 0
                         else round (toNumber clampedIdx / toNumber (n - 1) * 127.0)
           Just { cc, value: optVal, ringSnap: Just ringPos }

type ButtonResult = Array { cc :: CC, value :: MidiValue }

handleButton
  :: Int -> PedalDef -> PedalState
  -> Maybe ButtonResult
handleButton encoderIndex def ps = do
  tw <- def.twister
  mBtn <- Array.index tw.buttons encoderIndex
  btn <- mBtn
  case btn of
    TwisterToggle { cc } ->
      let current = fromMaybe (unsafeMidiValue 0) (Map.lookup cc ps.values)
          toggled = if unMidiValue current > 0 then unsafeMidiValue 0 else unsafeMidiValue 127
      in Just [ { cc, value: toggled } ]

    TwisterMomentary { cc } ->
      Just [ { cc, value: unsafeMidiValue 127 } ]

    TwisterSet { cc, value } ->
      Just [ { cc, value } ]

handleSideButton :: Maybe PedalId -> Array PedalId -> Maybe PedalId
handleSideButton mCurrent cardOrder =
  case mCurrent of
    Nothing ->
      Array.head cardOrder
    Just current ->
      let mIdx = Array.findIndex (_ == current) cardOrder
          n = Array.length cardOrder
      in case mIdx of
        Nothing -> Array.head cardOrder
        Just idx -> Array.index cardOrder ((idx + 1) `mod` n)

-- | Cycle focus backward through card order
handleSideButtonPrev :: Maybe PedalId -> Array PedalId -> Maybe PedalId
handleSideButtonPrev mCurrent cardOrder =
  case mCurrent of
    Nothing ->
      Array.last cardOrder
    Just current ->
      let mIdx = Array.findIndex (_ == current) cardOrder
          n = Array.length cardOrder
      in case mIdx of
        Nothing -> Array.last cardOrder
        Just idx -> Array.index cardOrder ((idx - 1 + n) `mod` n)

type LEDState = { index :: Int, ring :: Int, hue :: Int }

computeAllLEDs :: PedalDef -> PedalState -> Array LEDState
computeAllLEDs def ps =
  case def.twister of
    Nothing -> []
    Just tw ->
      Array.mapWithIndex (\i mEnc -> encoderLED i mEnc def ps) tw.encoders

encoderLED :: Int -> Maybe TwisterEncoder -> PedalDef -> PedalState -> LEDState
encoderLED index mEnc def ps =
  case mEnc of
    Nothing -> { index, ring: 0, hue: 0 }
    Just enc ->
      let hue = case def.twister of
                  Just tw -> tw.hue
                  Nothing -> 0
      in case enc of
        TwisterCC { cc, center, options } ->
          let currentVal = fromMaybe 0 (map unMidiValue (Map.lookup cc ps.values))
              ring = case options of
                Just opts -> optionRing currentVal opts
                Nothing -> case center of
                  Nothing -> currentVal
                  Just c -> ringValue currentVal (unMidiValue c)
          in { index, ring, hue }

optionRing :: Int -> Array MidiValue -> Int
optionRing currentVal opts =
  let n = Array.length opts
      mIdx = Array.findIndex (\mv -> unMidiValue mv == currentVal) opts
  in case mIdx of
    Just idx
      | n <= 1 -> 0
      | otherwise -> round (toNumber idx / toNumber (n - 1) * 127.0)
    Nothing ->
      -- Nearest match: find closest option
      let distances = Array.mapWithIndex (\i mv -> { i, d: intAbs (currentVal - unMidiValue mv) }) opts
          sorted = Array.sortWith _.d distances
      in case Array.head sorted of
        Nothing -> 0
        Just best
          | n <= 1 -> 0
          | otherwise -> round (toNumber best.i / toNumber (n - 1) * 127.0)

-- | Map value to ring position, remapping when center /= 64
ringValue :: Int -> Int -> Int
ringValue value center
  | center == 64 = value
  | center == 0 = value
  | value <= center = round (toNumber value / toNumber center * 64.0)
  | otherwise = round (64.0 + (toNumber (value - center) / toNumber (127 - center)) * 63.0)

-- | Get ring value for a specific encoder mapping, given a CC value
ringValueForEncoder :: Maybe TwisterEncoder -> Int -> Int
ringValueForEncoder mEnc val =
  case mEnc of
    Nothing -> 0
    Just (TwisterCC { center, options }) ->
      case options of
        Just opts -> optionRing val opts
        Nothing -> case center of
          Nothing -> val
          Just c -> ringValue val (unMidiValue c)

pedalHue :: PedalDef -> Int
pedalHue def = case def.twister of
  Just tw -> tw.hue
  Nothing -> 0
