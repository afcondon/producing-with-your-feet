-- | HATS-based Hedra piano keyboard visualization
-- |
-- | Instead of a skeuomorphic knob donut, Hedra shows WHAT YOU HEAR:
-- | a fixed 1-octave piano keyboard with voice target notes,
-- | delay time bars, and config controls.
module Component.Pedal.HedraTree
  ( hedraTree
  ) where

import Prelude

import Component.Pedal.DonutTree (PedalCallbacks)
import Data.Array (filter, findIndex, index, length, mapWithIndex) as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, unMidiValue, unsafeCC)
import Data.Map as Map
import Engine (PedalState)
import Hylograph.HATS (Tree, elem, forEach, staticStr, staticNum, thunkedStr, thunkedNum, withBehaviors, onClick, onPointerDown, siblings, empty)
import Hylograph.Internal.Element.Types (ElementType(..))

-- ============================================================================
-- MIDI CC constants
-- ============================================================================

cc :: Int -> CC
cc = unsafeCC

ccKey :: CC
ccKey = cc 16

ccScale :: CC
ccScale = cc 22

ccVoice1 :: CC
ccVoice1 = cc 19

ccVoice2 :: CC
ccVoice2 = cc 20

ccVoice3 :: CC
ccVoice3 = cc 21

ccDiv1 :: CC
ccDiv1 = cc 25

ccDiv2 :: CC
ccDiv2 = cc 26

ccDiv3 :: CC
ccDiv3 = cc 27

ccTempo :: CC
ccTempo = cc 15

ccMix :: CC
ccMix = cc 18

ccFeedback :: CC
ccFeedback = cc 24

ccDelayMode :: CC
ccDelayMode = cc 29

ccBypass :: CC
ccBypass = cc 14

ccHalfSpeed :: CC
ccHalfSpeed = cc 9

-- ============================================================================
-- Music theory
-- ============================================================================

-- | Scale patterns as semitone offsets from root
type ScaleInfo = { name :: String, pattern :: Array Int, isPentatonic :: Boolean }

scaleFromCC :: Int -> ScaleInfo
scaleFromCC v
  | v <= 11   = { name: "Major",       pattern: [0, 2, 4, 5, 7, 9, 11],  isPentatonic: false }
  | v <= 37   = { name: "Minor",       pattern: [0, 2, 3, 5, 7, 8, 10],  isPentatonic: false }
  | v <= 58   = { name: "Mel Min",     pattern: [0, 2, 3, 5, 7, 9, 11],  isPentatonic: false }
  | v <= 78   = { name: "Harm Min",    pattern: [0, 2, 3, 5, 7, 8, 11],  isPentatonic: false }
  | v <= 97   = { name: "Dbl Harm",    pattern: [0, 1, 4, 5, 7, 8, 11],  isPentatonic: false }
  | v <= 119  = { name: "Lyd Pent",    pattern: [0, 4, 6, 7, 11],        isPentatonic: true  }
  | otherwise = { name: "Min Pent",    pattern: [0, 3, 5, 7, 10],        isPentatonic: true  }

-- | CC 16 MIDI value → semitone (0=C through 11=B)
-- | Values: 0→C, 7→Db, 18→D, 31→Eb, 42→E, 53→F, 62→Gb, 72→G, 82→Ab, 92→A, 103→Bb, 114→B
keyToSemitone :: Int -> Int
keyToSemitone v
  | v <= 3   = 0   -- C
  | v <= 12  = 1   -- Db
  | v <= 24  = 2   -- D
  | v <= 36  = 3   -- Eb
  | v <= 47  = 4   -- E
  | v <= 57  = 5   -- F
  | v <= 67  = 6   -- Gb
  | v <= 77  = 7   -- G
  | v <= 87  = 8   -- Ab
  | v <= 97  = 9   -- A
  | v <= 108 = 10  -- Bb
  | v <= 121 = 11  -- B
  | otherwise = 0  -- chromatic (123) — treat as C for display

isChromatic :: Int -> Boolean
isChromatic v = v >= 122

-- | Semitone → note name
semitoneToName :: Int -> String
semitoneToName = case _ of
  0  -> "C"
  1  -> "D\x266D"
  2  -> "D"
  3  -> "E\x266D"
  4  -> "E"
  5  -> "F"
  6  -> "G\x266D"
  7  -> "G"
  8  -> "A\x266D"
  9  -> "A"
  10 -> "B\x266D"
  11 -> "B"
  _  -> "?"

-- | Resolve a voice CC value to a semitone offset from root (or Nothing if Off)
type VoiceResult = { semitoneOffset :: Int, intervalLabel :: String }

resolveVoice :: Boolean -> Array Int -> Int -> Maybe VoiceResult
resolveVoice isPentatonic scalePattern voiceVal
  | voiceVal == 0 = Nothing  -- Off
  | voiceVal >= 1 && voiceVal <= 11 =
      -- Octave shifts (negative)
      if voiceVal <= 7
        then Just { semitoneOffset: -24, intervalLabel: "-2Oct" }
        else Just { semitoneOffset: -12, intervalLabel: "-1Oct" }
  | voiceVal >= 116 =
      -- Octave shifts (positive)
      if voiceVal <= 119
        then Just { semitoneOffset: 12, intervalLabel: "+1Oct" }
        else Just { semitoneOffset: 24, intervalLabel: "+2Oct" }
  | isPentatonic =
      -- Pentatonic: val 28..99, step size 8, center at degree 4 (unison)
      let degreeIndex = (voiceVal - 28) / 8
          offset = degreeIndex - 4  -- scale steps from unison
          n = Array.length scalePattern
      in if n == 0 then Nothing
         else
           let semitones = scaleStepsToSemitones scalePattern offset
               label = if offset == 0 then "U"
                       else if offset > 0 then "+" <> show offset
                       else show offset
           in Just { semitoneOffset: semitones, intervalLabel: label }
  | otherwise =
      -- Diatonic: val 12..115, step size 8, center at degree 6 (unison)
      let degreeIndex = (voiceVal - 12) / 8
          offset = degreeIndex - 6  -- scale steps from unison
          n = Array.length scalePattern
      in if n == 0 then Nothing
         else
           let semitones = scaleStepsToSemitones scalePattern offset
               label = if offset == 0 then "U"
                       else if offset > 0 then "+" <> show offset
                       else show offset
           in Just { semitoneOffset: semitones, intervalLabel: label }

-- | Convert scale steps (positive or negative) to semitone offset
scaleStepsToSemitones :: Array Int -> Int -> Int
scaleStepsToSemitones pattern steps =
  let n = Array.length pattern
  in if n == 0 then 0
     else if steps >= 0 then
       let octaves = steps / n
           remainder = steps `mod` n
           semis = fromMaybe 0 (Array.index pattern remainder)
       in octaves * 12 + semis
     else
       let posSteps = -steps
           octaves = posSteps / n
           remainder = posSteps `mod` n
           semis = fromMaybe 0 (Array.index pattern remainder)
       in -(octaves * 12 + semis)

-- | Get all scale notes (semitones mod 12) from root
scaleNotes :: Int -> Array Int -> Array Int
scaleNotes rootSemitone pattern =
  map (\offset -> (rootSemitone + offset) `mod` 12) pattern

-- | CC value → note MIDI value table for root key clicks
-- | Returns the CC value to send for a given semitone
semitoneToKeyCC :: Int -> Int
semitoneToKeyCC = case _ of
  0  -> 0     -- C
  1  -> 7     -- Db
  2  -> 18    -- D
  3  -> 31    -- Eb
  4  -> 42    -- E
  5  -> 53    -- F
  6  -> 62    -- Gb
  7  -> 72    -- G
  8  -> 82    -- Ab
  9  -> 92    -- A
  10 -> 103   -- Bb
  11 -> 114   -- B
  _  -> 0

-- ============================================================================
-- Piano keyboard geometry
-- ============================================================================

type KeyGeom =
  { semitone :: Int     -- 0-11 within octave
  , absoluteSemitone :: Int -- position from start
  , isBlack :: Boolean
  , x :: Number
  , w :: Number
  , h :: Number
  }

isBlackKey :: Int -> Boolean
isBlackKey s = case s `mod` 12 of
  1  -> true  -- Db
  3  -> true  -- Eb
  6  -> true  -- Gb
  8  -> true  -- Ab
  10 -> true  -- Bb
  _  -> false

-- | Generate key geometry for N octaves starting from a given semitone.
-- | White keys are evenly spaced; black keys overlay them.
-- | Heights are derived from width using real piano proportions.
pianoKeyGeometry :: Number -> Number -> Int -> Int -> Array KeyGeom
pianoKeyGeometry startX totalWidth startSemitone numSemitones =
  let
    allSemitones = map (\i -> startSemitone + i) (range 0 (numSemitones - 1))
    whiteCount = Array.length (Array.filter (\s -> not (isBlackKey s)) allSemitones)
    whiteW = if whiteCount > 0 then totalWidth / toNumber whiteCount else 10.0
    whiteH = whiteW * keyAspectRatio
    blackW = whiteW * blackKeyWidthRatio
    blackH = whiteH * blackKeyHeightRatio
  in
    buildKeys startX whiteW blackW whiteH blackH startSemitone numSemitones

buildKeys :: Number -> Number -> Number -> Number -> Number -> Int -> Int -> Array KeyGeom
buildKeys startX whiteW blackW whiteH blackH startSemitone numSemitones =
  let
    go :: Int -> Int -> Number -> Array KeyGeom -> Array KeyGeom
    go i whiteIdx currentX acc
      | i >= numSemitones = acc
      | otherwise =
          let absSemi = startSemitone + i
              semi = absSemi `mod` 12
              black = isBlackKey semi
          in if black
            then
              -- Black key is placed relative to previous white key
              let bx = currentX - blackW / 2.0
                  key = { semitone: semi, absoluteSemitone: absSemi, isBlack: true
                        , x: bx, w: blackW, h: blackH }
              in go (i + 1) whiteIdx currentX (acc <> [key])
            else
              let wx = startX + toNumber whiteIdx * whiteW
                  key = { semitone: semi, absoluteSemitone: absSemi, isBlack: false
                        , x: wx, w: whiteW, h: whiteH }
              in go (i + 1) (whiteIdx + 1) (wx + whiteW) (acc <> [key])
  in go 0 0 startX []

-- | Find the center X of a key by its semitone (mod 12) in a key geometry array
findKeyX :: Array KeyGeom -> Int -> Maybe Number
findKeyX keys targetSemi =
  case Array.findIndex (\k -> k.semitone == (targetSemi `mod` 12)) keys of
    Just idx -> case Array.index keys idx of
      Just k -> Just (k.x + k.w / 2.0)
      Nothing -> Nothing
    Nothing -> Nothing

-- ============================================================================
-- Voice colors
-- ============================================================================

voiceColor :: Int -> String
voiceColor = case _ of
  0 -> "#e07048"  -- coral
  1 -> "#4888c0"  -- blue
  2 -> "#50a060"  -- green
  _ -> "#888"

rootColor :: String
rootColor = "#8a8f94"

-- ============================================================================
-- Layout constants
-- ============================================================================

viewW :: Number
viewW = 340.0

viewH :: Number
viewH = 390.0

kbStartX :: Number
kbStartX = 10.0

kbWidth :: Number
kbWidth = 320.0

-- Piano key proportions (matches real piano)
keyAspectRatio :: Number
keyAspectRatio = 6.0

blackKeyHeightRatio :: Number
blackKeyHeightRatio = 0.65

blackKeyWidthRatio :: Number
blackKeyWidthRatio = 0.55

-- Main keyboard: fixed 1-octave (182px = 7 x 26px white keys, centered)
mainKbWidth :: Number
mainKbWidth = 182.0

mainKbStartX :: Number
mainKbStartX = kbStartX + (kbWidth - mainKbWidth) / 2.0

mainWhiteW :: Number
mainWhiteW = mainKbWidth / 7.0

mainWhiteH :: Number
mainWhiteH = mainWhiteW * keyAspectRatio

mainBlackH :: Number
mainBlackH = mainWhiteH * blackKeyHeightRatio

-- Vertical positions
barChartY :: Number
barChartY = 30.0

barChartH :: Number
barChartH = 50.0

mainKbY :: Number
mainKbY = 88.0

legendY :: Number
legendY = 258.0

configY :: Number
configY = 294.0

footswitchY :: Number
footswitchY = 344.0

-- ============================================================================
-- CC value lookup
-- ============================================================================

lookupCC :: CC -> PedalState -> Int
lookupCC ccNum ps = fromMaybe 0 (map unMidiValue (Map.lookup ccNum ps.values))

-- ============================================================================
-- Delay division labels
-- ============================================================================

divisionLabel :: Int -> String
divisionLabel v
  | v <= 10  = "Off"
  | v <= 30  = "Whole"
  | v <= 50  = "3/4"
  | v <= 70  = "1/2"
  | v <= 85  = "3/8"
  | v <= 100 = "D.8th"
  | v <= 115 = "1/4"
  | otherwise = "1/4"

-- ============================================================================
-- HATS Tree construction
-- ============================================================================

-- | Build the complete Hedra visualization tree
hedraTree :: PedalState -> PedalCallbacks -> Tree
hedraTree ps callbacks =
  let
    keyVal = lookupCC ccKey ps
    scaleVal = lookupCC ccScale ps
    rootSemitone = keyToSemitone keyVal
    chromatic = isChromatic keyVal
    scale = scaleFromCC scaleVal
    scaleSemitones = scaleNotes rootSemitone scale.pattern

    -- Voice resolution
    v1Val = lookupCC ccVoice1 ps
    v2Val = lookupCC ccVoice2 ps
    v3Val = lookupCC ccVoice3 ps
    v1 = resolveVoice scale.isPentatonic scale.pattern v1Val
    v2 = resolveVoice scale.isPentatonic scale.pattern v2Val
    v3 = resolveVoice scale.isPentatonic scale.pattern v3Val

    -- Main keyboard: fixed 1 octave (always C-B)
    mainKeys = pianoKeyGeometry mainKbStartX mainKbWidth 0 12
  in
    elem SVG
      [ staticStr "viewBox" ("0 0 " <> show viewW <> " " <> show viewH)
      , staticStr "class" "pedal-svg"
      ]
      [ -- Scale display header
        scaleHeader rootSemitone scale chromatic
        -- Bar chart (delay time bars)
      , barChart mainKeys rootSemitone v1 v2 v3 ps
        -- Main keyboard (interactive)
      , mainKeyboard mainKeys rootSemitone scaleSemitones chromatic v1 v2 v3 callbacks
        -- Voice legend (TangleJS draggable)
      , voiceLegend rootSemitone ps callbacks
        -- Separator
      , sectionLine 280.0
        -- Config controls
      , configControls ps callbacks
        -- Separator
      , sectionLine 326.0
        -- Footswitches
      , footswitches ps callbacks
      ]

-- ============================================================================
-- Scale header
-- ============================================================================

scaleHeader :: Int -> ScaleInfo -> Boolean -> Tree
scaleHeader rootSemitone scale chromatic =
  let
    rootName = semitoneToName rootSemitone
    label = if chromatic then "Chromatic" else rootName <> " " <> scale.name
  in
    elem Text
      [ staticNum "x" 170.0
      , staticNum "y" 14.0
      , staticStr "text-anchor" "middle"
      , staticStr "font-size" "10"
      , staticStr "font-weight" "600"
      , staticStr "fill" "#666"
      , thunkedStr "textContent" label
      ] []


-- ============================================================================
-- Bar chart (delay time bars above voice keys)
-- ============================================================================

barChart :: Array KeyGeom -> Int -> Maybe VoiceResult -> Maybe VoiceResult -> Maybe VoiceResult -> PedalState -> Tree
barChart mainKeys rootSemitone v1 v2 v3 ps =
  let
    d1 = lookupCC ccDiv1 ps
    d2 = lookupCC ccDiv2 ps
    d3 = lookupCC ccDiv3 ps
    -- Collect active bar entries
    mkEntry idx vr dv = case vr of
      Nothing -> []
      Just r -> [{ voiceIdx: idx, targetSemi: (rootSemitone + r.semitoneOffset) `mod` 12, divVal: dv }]
    entries = mkEntry 0 v1 d1 <> mkEntry 1 v2 d2 <> mkEntry 2 v3 d3
    -- Add horizontal offsets for overlapping bars
    withOffsets = map (\e ->
      let sameKey = Array.filter (\u -> u.targetSemi == e.targetSemi) entries
          total = Array.length sameKey
          idx = fromMaybe 0 (Array.findIndex (\u -> u.voiceIdx == e.voiceIdx) sameKey)
      in { voiceIdx: e.voiceIdx, targetSemi: e.targetSemi, divVal: e.divVal
         , xOffset: spreadOffset total idx 8.0 10.0 }
    ) entries
    renderBar entry =
      case findKeyX mainKeys entry.targetSemi of
        Nothing -> empty
        Just kx ->
          let barH = barChartH * toNumber entry.divVal / 127.0
              barW = 14.0
              cx = kx + entry.xOffset
              bx = cx - barW / 2.0
              by = barChartY + barChartH - barH
              color = voiceColor entry.voiceIdx
              label = divisionLabel entry.divVal
          in siblings
            [ elem Rect
                [ thunkedNum "x" bx
                , thunkedNum "y" by
                , staticNum "width" barW
                , thunkedNum "height" barH
                , thunkedStr "fill" color
                , staticStr "opacity" "0.7"
                , staticStr "rx" "2"
                ] []
            , if barH > 12.0
                then elem Text
                  [ thunkedNum "x" cx
                  , thunkedNum "y" (by + barH / 2.0 + 3.0)
                  , staticStr "text-anchor" "middle"
                  , staticStr "font-size" "6"
                  , staticStr "font-weight" "600"
                  , staticStr "fill" "#fff"
                  , staticStr "pointer-events" "none"
                  , thunkedStr "textContent" label
                  ] []
                else empty
            ]
  in
    siblings (map renderBar withOffsets)

-- ============================================================================
-- Main keyboard (3 octaves, interactive)
-- ============================================================================

mainKeyboard :: Array KeyGeom -> Int -> Array Int -> Boolean -> Maybe VoiceResult -> Maybe VoiceResult -> Maybe VoiceResult -> PedalCallbacks -> Tree
mainKeyboard keys rootSemitone scaleSemitones chromatic v1 v2 v3 callbacks =
  let
    whiteKeys = Array.filter (\k -> not k.isBlack) keys
    blackKeys = Array.filter (\k -> k.isBlack) keys
    voiceTargets = voiceTargetSemitones rootSemitone [v1, v2, v3]
  in
    siblings
      [ -- White keys (clickable)
        forEach "main-white" Group whiteKeys mainKeyId \k ->
          let isRoot = k.semitone == rootSemitone `mod` 12
              ccVal = semitoneToKeyCC k.semitone
          in withBehaviors [ onClick (callbacks.onSegmentClick ccKey ccVal) ]
             $ elem Rect
               [ thunkedNum "x" k.x
               , thunkedNum "y" mainKbY
               , thunkedNum "width" (k.w - 1.0)
               , thunkedNum "height" k.h
               , thunkedStr "fill" (if isRoot then rootColor else "#fafafa")
               , thunkedStr "opacity" (if isRoot then "0.25" else "1")
               , staticStr "stroke" "#bbb"
               , staticStr "stroke-width" "0.5"
               , staticStr "rx" "2"
               , staticStr "style" "cursor:pointer"
               ] []
      , -- Black keys (clickable)
        forEach "main-black" Group blackKeys mainKeyId \k ->
          let ccVal = semitoneToKeyCC k.semitone
          in withBehaviors [ onClick (callbacks.onSegmentClick ccKey ccVal) ]
             $ elem Rect
               [ thunkedNum "x" k.x
               , thunkedNum "y" mainKbY
               , thunkedNum "width" k.w
               , thunkedNum "height" k.h
               , staticStr "fill" "#333"
               , staticStr "rx" "1"
               , staticStr "style" "cursor:pointer"
               ] []
      , -- Scale note names on keys
        scaleNoteLabels keys scaleSemitones rootSemitone chromatic
      , -- Voice markers
        voiceMarkers keys voiceTargets
      , -- Root label
        case findKeyX keys (rootSemitone `mod` 12) of
          Just rx -> elem Text
            [ thunkedNum "x" rx
            , thunkedNum "y" (mainKbY + mainWhiteH - 6.0)
            , staticStr "text-anchor" "middle"
            , staticStr "font-size" "9"
            , staticStr "font-weight" "700"
            , staticStr "fill" rootColor
            , staticStr "pointer-events" "none"
            , thunkedStr "textContent" (semitoneToName rootSemitone)
            ] []
          Nothing -> empty
      ]

mainKeyId :: KeyGeom -> String
mainKeyId k = "mk" <> show k.absoluteSemitone

-- | Compute voice target semitones (mod 12) with voice index and offset
voiceTargetSemitones :: Int -> Array (Maybe VoiceResult) -> Array { voiceIdx :: Int, targetSemi :: Int, semitoneOffset :: Int }
voiceTargetSemitones rootSemitone voices =
  let
    go :: Int -> Array (Maybe VoiceResult) -> Array { voiceIdx :: Int, targetSemi :: Int, semitoneOffset :: Int }
    go _ [] = []
    go idx arr = case Array.index arr 0 of
      Nothing -> go (idx + 1) (drop1 arr)
      Just Nothing -> go (idx + 1) (drop1 arr)
      Just (Just vr) ->
        let target = { voiceIdx: idx, targetSemi: (rootSemitone + vr.semitoneOffset) `mod` 12, semitoneOffset: vr.semitoneOffset }
        in [target] <> go (idx + 1) (drop1 arr)
  in go 0 voices

drop1 :: forall a. Array a -> Array a
drop1 arr = case Array.length arr of
  0 -> []
  _ -> unsafeSlice 1 (Array.length arr - 1) arr

-- | Voice markers on the keyboard (with vertical offset for overlapping notes)
voiceMarkers :: Array KeyGeom -> Array { voiceIdx :: Int, targetSemi :: Int, semitoneOffset :: Int } -> Tree
voiceMarkers keys targets =
  let withOffsets = addVoiceOffsets targets
  in siblings (map (renderVoiceMarker keys) withOffsets)

renderVoiceMarker :: Array KeyGeom -> { voiceIdx :: Int, targetSemi :: Int, semitoneOffset :: Int, yOffset :: Number } -> Tree
renderVoiceMarker keys target =
  case findKeyX keys target.targetSemi of
    Nothing -> empty
    Just kx ->
      let color = voiceColor target.voiceIdx
          isBlack = isBlackKey target.targetSemi
          baseY = if isBlack then mainKbY + mainBlackH - 10.0 else mainKbY + mainWhiteH - 22.0
          cy = baseY + target.yOffset
          octaves = absInt target.semitoneOffset / 12
      in siblings
        [ elem Circle
            [ thunkedNum "cx" kx
            , thunkedNum "cy" cy
            , staticNum "r" 5.0
            , thunkedStr "fill" color
            , staticStr "stroke" "#fff"
            , staticStr "stroke-width" "1.5"
            , staticStr "pointer-events" "none"
            ] []
        , if octaves >= 1
            then
              let symbol = if target.semitoneOffset > 0 then "\x2191" else "\x2193"
              in elem Text
                [ thunkedNum "x" kx
                , thunkedNum "y" (cy - 10.0)
                , staticStr "text-anchor" "middle"
                , staticStr "font-size" "6"
                , staticStr "font-weight" "600"
                , thunkedStr "fill" color
                , staticStr "pointer-events" "none"
                , thunkedStr "textContent" (symbol <> show octaves)
                ] []
            else empty
        ]

-- | Add vertical offsets when multiple voices target the same note
addVoiceOffsets :: Array { voiceIdx :: Int, targetSemi :: Int, semitoneOffset :: Int } -> Array { voiceIdx :: Int, targetSemi :: Int, semitoneOffset :: Int, yOffset :: Number }
addVoiceOffsets targets =
  map (\t ->
    let sameKey = Array.filter (\u -> u.targetSemi == t.targetSemi) targets
        total = Array.length sameKey
        idx = fromMaybe 0 (Array.findIndex (\u -> u.voiceIdx == t.voiceIdx) sameKey)
    in { voiceIdx: t.voiceIdx, targetSemi: t.targetSemi, semitoneOffset: t.semitoneOffset, yOffset: spreadOffset total idx 7.0 9.0 }
  ) targets

-- ============================================================================
-- Voice legend
-- ============================================================================

voiceLegend :: Int -> PedalState -> PedalCallbacks -> Tree
voiceLegend rootSemitone ps callbacks =
  let
    scaleVal = lookupCC ccScale ps
    scale = scaleFromCC scaleVal
    v1Val = lookupCC ccVoice1 ps
    v2Val = lookupCC ccVoice2 ps
    v3Val = lookupCC ccVoice3 ps
    d1Val = lookupCC ccDiv1 ps
    d2Val = lookupCC ccDiv2 ps
    d3Val = lookupCC ccDiv3 ps
    v1 = resolveVoice scale.isPentatonic scale.pattern v1Val
    v2 = resolveVoice scale.isPentatonic scale.pattern v2Val
    v3 = resolveVoice scale.isPentatonic scale.pattern v3Val

    renderLegendItem :: Number -> Int -> CC -> Int -> CC -> Int -> Maybe VoiceResult -> Tree
    renderLegendItem x voiceIdx voiceCC voiceVal divCC divVal voiceResult =
      let
        color = voiceColor voiceIdx
        label = "V" <> show (voiceIdx + 1)
        detail = case voiceResult of
          Nothing -> "Off"
          Just vr ->
            let noteName = semitoneToName ((rootSemitone + vr.semitoneOffset) `mod` 12)
            in vr.intervalLabel <> " (" <> noteName <> ") \x00B7 " <> divisionLabel divVal
      in withBehaviors [ onPointerDown (callbacks.onKnobDragStart2D divCC divVal voiceCC voiceVal) ]
        $ elem Group [ staticStr "style" "cursor:move" ]
          [ elem Circle
              [ thunkedNum "cx" (x + 5.0)
              , thunkedNum "cy" (legendY - 3.0)
              , staticNum "r" 3.5
              , thunkedStr "fill" color
              ] []
          , elem Text
              [ thunkedNum "x" (x + 12.0)
              , thunkedNum "y" legendY
              , staticStr "text-anchor" "start"
              , staticStr "font-size" "9"
              , staticStr "font-weight" "600"
              , thunkedStr "fill" color
              , staticStr "text-decoration" "underline dotted"
              , thunkedStr "textContent" (label <> ": " <> detail)
              ] []
          ]
  in
    siblings
      [ renderLegendItem 15.0  0 ccVoice1 v1Val ccDiv1 d1Val v1
      , renderLegendItem 120.0 1 ccVoice2 v2Val ccDiv2 d2Val v2
      , renderLegendItem 225.0 2 ccVoice3 v3Val ccDiv3 d3Val v3
      ]

-- ============================================================================
-- Config controls (Tempo, Mix, Feedback sliders + Delay Mode toggle)
-- ============================================================================

configControls :: PedalState -> PedalCallbacks -> Tree
configControls ps callbacks =
  let
    sliderW = 50.0
    renderSlider :: Number -> String -> CC -> Tree
    renderSlider x label theCC =
      let val = lookupCC theCC ps
          fillW = sliderW * toNumber val / 127.0
          bx = x - sliderW / 2.0
          by = configY - 3.0
          barH = 6.0
      in withBehaviors [ onPointerDown (callbacks.onKnobDragStart theCC val) ]
         $ elem Group [ staticStr "style" "cursor:ns-resize" ]
           [ elem Rect
               [ thunkedNum "x" bx, thunkedNum "y" by
               , staticNum "width" sliderW, staticNum "height" barH
               , staticStr "rx" "3", staticStr "ry" "3"
               , staticStr "fill" "#e8e8e8"
               ] []
           , elem Rect
               [ thunkedNum "x" bx, thunkedNum "y" by
               , thunkedNum "width" fillW, staticNum "height" barH
               , staticStr "rx" "3", staticStr "ry" "3"
               , staticStr "fill" "#888"
               ] []
           , elem Text
               [ thunkedNum "x" x, thunkedNum "y" (configY + 14.0)
               , staticStr "text-anchor" "middle"
               , staticStr "font-size" "7"
               , staticStr "fill" "#999"
               , staticStr "pointer-events" "none"
               , thunkedStr "textContent" label
               ] []
           ]

    -- Delay Mode: 4-way segmented toggle
    renderDelayMode :: Number -> Tree
    renderDelayMode x =
      let val = lookupCC ccDelayMode ps
          modes = [ { lo: 0, hi: 31, send: 15, label: "S+PF" }
                  , { lo: 32, hi: 63, send: 47, label: "Ser" }
                  , { lo: 64, hi: 95, send: 79, label: "D+CF" }
                  , { lo: 96, hi: 127, send: 111, label: "Dual" }
                  ]
          dotSpacing = 10.0
          startDotX = x - 1.5 * dotSpacing
      in siblings (Array.mapWithIndex (\i mode ->
            let dx = startDotX + toNumber i * dotSpacing
                isActive = val >= mode.lo && val <= mode.hi
            in withBehaviors [ onClick (callbacks.onSegmentClick ccDelayMode mode.send) ]
               $ elem Circle
                 [ thunkedNum "cx" dx
                 , thunkedNum "cy" configY
                 , staticNum "r" 4.0
                 , thunkedStr "fill" (if isActive then "#555" else "#ddd")
                 , staticStr "style" "cursor:pointer"
                 ] []
          ) modes)
        <> elem Text
             [ thunkedNum "x" x, thunkedNum "y" (configY + 14.0)
             , staticStr "text-anchor" "middle"
             , staticStr "font-size" "7"
             , staticStr "fill" "#999"
             , staticStr "pointer-events" "none"
             , thunkedStr "textContent" "Mode"
             ] []
  in
    siblings
      [ renderSlider 50.0 "Tempo" ccTempo
      , renderSlider 130.0 "Mix" ccMix
      , renderSlider 210.0 "Fdbk" ccFeedback
      , renderDelayMode 290.0
      ]

-- ============================================================================
-- Footswitches
-- ============================================================================

footswitches :: PedalState -> PedalCallbacks -> Tree
footswitches ps callbacks =
  let
    renderFootswitch :: Number -> String -> CC -> Tree
    renderFootswitch x label theCC =
      let val = lookupCC theCC ps
          isOn = val > 63
          fillColor = if isOn then rootColor else "#ddd"
          textColor = if isOn then "#fff" else "#999"
      in withBehaviors [ onClick (callbacks.onToggleClick theCC val) ]
         $ elem Group
           [ thunkedStr "transform" ("translate(" <> show x <> "," <> show footswitchY <> ")")
           , staticStr "style" "cursor:pointer"
           ]
           [ elem Circle
               [ staticNum "cx" 0.0, staticNum "cy" 0.0
               , staticNum "r" 20.0
               , thunkedStr "fill" fillColor
               , staticStr "stroke" rootColor
               , staticStr "stroke-width" "2"
               ] []
           , elem Text
               [ staticNum "x" 0.0, staticNum "y" 4.0
               , staticStr "text-anchor" "middle"
               , staticStr "font-size" "8"
               , staticStr "font-weight" "600"
               , thunkedStr "fill" textColor
               , staticStr "pointer-events" "none"
               , thunkedStr "textContent" label
               ] []
           ]
  in
    siblings
      [ renderFootswitch 80.0 "Half Spd" ccHalfSpeed
      , renderFootswitch 260.0 "Bypass" ccBypass
      ]

-- ============================================================================
-- Scale degree markers on main keyboard
-- ============================================================================

-- | Note name labels near the top of scale-degree keys
scaleNoteLabels :: Array KeyGeom -> Array Int -> Int -> Boolean -> Tree
scaleNoteLabels keys scaleSemitones _rootSemitone chromatic
  | chromatic = empty
  | otherwise =
      siblings (map (\k ->
        let inScale = elemInt k.semitone scaleSemitones
        in if not inScale then empty
           else
             let cx = k.x + k.w / 2.0
                 cy = if k.isBlack then mainKbY + 12.0 else mainKbY + 14.0
                 fill = if k.isBlack then "#fff" else "#333"
                 fontSize = if k.isBlack then "6" else "7"
             in elem Text
               [ thunkedNum "x" cx
               , thunkedNum "y" cy
               , staticStr "text-anchor" "middle"
               , thunkedStr "font-size" fontSize
               , staticStr "font-weight" "600"
               , thunkedStr "fill" fill
               , staticStr "pointer-events" "none"
               , thunkedStr "textContent" (semitoneToName k.semitone)
               ] []
      ) keys)

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Spread N items symmetrically: 1→0, 2→±small, 3→[-big, 0, +big]
spreadOffset :: Int -> Int -> Number -> Number -> Number
spreadOffset total idx small big
  | total <= 1 = 0.0
  | total == 2 = if idx == 0 then negate small else small
  | otherwise = case idx of
      0 -> negate big
      1 -> 0.0
      _ -> big

sectionLine :: Number -> Tree
sectionLine y =
  elem Line
    [ staticNum "x1" 20.0, thunkedNum "y1" y
    , staticNum "x2" 320.0, thunkedNum "y2" y
    , staticStr "stroke" "#e0e0e0"
    , staticStr "stroke-width" "0.5"
    ] []

-- | Check if an Int is in an array
elemInt :: Int -> Array Int -> Boolean
elemInt x arr = case Array.findIndex (\a -> a == x) arr of
  Just _ -> true
  Nothing -> false

-- | Absolute value for Int
absInt :: Int -> Int
absInt n = if n >= 0 then n else negate n


-- | Range helper
range :: Int -> Int -> Array Int
range lo hi
  | lo > hi = []
  | lo == hi = [lo]
  | otherwise = [lo] <> range (lo + 1) hi

-- | Unsafe array slice (PureScript Data.Array.slice is inclusive)
unsafeSlice :: forall a. Int -> Int -> Array a -> Array a
unsafeSlice start len arr = map (\i -> unsafeIdx arr (start + i)) (range 0 (len - 1))

-- | Unsafe index (panics if out of bounds — only used internally)
unsafeIdx :: forall a. Array a -> Int -> a
unsafeIdx arr i = case Array.index arr i of
  Just x -> x
  Nothing -> unsafeIdx arr 0  -- unreachable in practice
