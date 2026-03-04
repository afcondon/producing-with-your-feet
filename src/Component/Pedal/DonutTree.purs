-- | HATS-based pedal donut view
-- |
-- | Builds a HATS Tree from PedalLayout + PedalState.
-- | The tree is rendered into the DOM by the interpreter (rerender).
module Component.Pedal.DonutTree
  ( pedalTree
  , PedalCallbacks
  ) where

import Prelude

import Data.Array (find, index, length, mapWithIndex, null) as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Midi (CC, unMidiValue)
import Data.Number (cos, sin, pi, abs)
import Data.Pedal.Label (LabelSource(..))
import Data.Pedal.Layout (ConfigControlType(..), ConfigDef, DipBankDef, FootswitchDef, GroupStyle, KnobDef, KnobLayer(..), PedalLayout)
import Effect (Effect)
import Engine (PedalState)
import Hylograph.HATS (Tree, elem, forEach, staticStr, staticNum, thunkedStr, thunkedNum, withBehaviors, onClick, onPointerDown, siblings, empty)
import Hylograph.Internal.Element.Types (ElementType(..))

-- | Callbacks for interactive elements
type PedalCallbacks =
  { onKnobDragStart :: CC -> Int -> Effect Unit  -- CC, current value
  , onSegmentClick :: CC -> Int -> Effect Unit
  , onToggleClick :: CC -> Int -> Effect Unit
  }

-- | Knob sweep range: 300° from 7 o'clock to 5 o'clock
minAngle :: Number
minAngle = -5.0 * pi / 6.0

maxAngle :: Number
maxAngle = 5.0 * pi / 6.0

sweepRange :: Number
sweepRange = maxAngle - minAngle

-- | Convert MIDI value 0..127 to angle within the 300° knob sweep
valToAngle :: Int -> Number
valToAngle v = minAngle + (toNumber v / 127.0) * sweepRange

-- | Position helpers (same logic as Donut.purs)
colXFor :: Number -> Int -> Int -> Number
colXFor viewWidth numCols col
  | numCols <= 1 = viewWidth / 2.0
  | otherwise =
      let pad = viewWidth * 3.0 / 16.0
          span = viewWidth - 2.0 * pad
      in pad + toNumber col * span / toNumber (numCols - 1)

rowYFor :: Int -> Number
rowYFor = case _ of
  0 -> 60.0
  1 -> 160.0
  2 -> 250.0
  r -> 60.0 + toNumber r * 90.0

fsRowY :: Int -> Number
fsRowY knobRows = rowYFor (knobRows - 1) + 80.0

fsXFor :: Number -> Int -> Int -> Number
fsXFor viewWidth numCols fsCol
  | numCols <= 1 = viewWidth / 2.0
  | fsCol < numCols - 1 = (colXFor viewWidth numCols fsCol + colXFor viewWidth numCols (fsCol + 1)) / 2.0
  | fsCol > 0 = (colXFor viewWidth numCols (fsCol - 1) + colXFor viewWidth numCols fsCol) / 2.0
  | otherwise = colXFor viewWidth numCols fsCol

-- | Group color lookup
groupColor :: Array GroupStyle -> String -> String
groupColor groups gid =
  case Array.find (\g -> g.id == gid) groups of
    Just g -> g.color
    Nothing -> "#999"

groupMutedColor :: Array GroupStyle -> String -> String
groupMutedColor groups gid =
  case Array.find (\g -> g.id == gid) groups of
    Just g -> g.mutedColor
    Nothing -> "#ccc"

-- | CC value lookup from PedalState
lookupCC :: CC -> PedalState -> Int
lookupCC ccNum ps = fromMaybe 0 (map unMidiValue (Map.lookup ccNum ps.values))

-- | Resolve a LabelSource to a concrete string
resolveLabel :: PedalState -> LabelSource -> String
resolveLabel ps = case _ of
  Static s -> s
  ModeMap r ->
    case Map.lookup r.cc ps.values of
      Just val -> fromMaybe "?" (Map.lookup val r.labels)
      Nothing -> "?"
  ChannelMode _ -> "?"

-- | Layer helpers
isSegmented :: KnobLayer -> Boolean
isSegmented SegmentedKnob = true
isSegmented _ = false

layerCenter :: KnobLayer -> Maybe Int
layerCenter (ContinuousKnob r) = r.center
layerCenter SegmentedKnob = Nothing

-- | Arc path generation (same as Donut.purs)
donutSegmentPath :: Number -> Number -> Number -> Number -> Number -> Number -> String
donutSegmentPath cx cy outerR innerR startAngle endAngle =
  let
    toSvg a = a - pi / 2.0
    sa = toSvg startAngle
    ea = toSvg endAngle
    ox0 = cx + outerR * cos sa
    oy0 = cy + outerR * sin sa
    ox1 = cx + outerR * cos ea
    oy1 = cy + outerR * sin ea
    ix0 = cx + innerR * cos sa
    iy0 = cy + innerR * sin sa
    ix1 = cx + innerR * cos ea
    iy1 = cy + innerR * sin ea
    diff = endAngle - startAngle
    largeArc = if diff > pi then "1" else "0"
  in
    "M" <> show ox0 <> "," <> show oy0 <>
    "A" <> show outerR <> "," <> show outerR <> " 0 " <> largeArc <> ",1 " <> show ox1 <> "," <> show oy1 <>
    "L" <> show ix1 <> "," <> show iy1 <>
    "A" <> show innerR <> "," <> show innerR <> " 0 " <> largeArc <> ",0 " <> show ix0 <> "," <> show iy0 <>
    "Z"

-- | Knob dimensions
outerRadius :: Number
outerRadius = 40.0

innerRadius :: Number
innerRadius = 16.0

gapRadius :: Number
gapRadius = 28.0

-- ============================================================================
-- HATS Tree construction
-- ============================================================================

-- | Build the complete pedal donut Tree
pedalTree :: PedalLayout -> PedalState -> PedalCallbacks -> Tree
pedalTree layout ps callbacks =
  let
    vb = layout.viewBox
    fsY = fsRowY layout.knobRows
    configY = fsY + 44.0
    dipBaseY = configY + 36.0
    hasConfig = not (Array.null layout.config)
    hasDips = not (Array.null layout.dipBanks)
  in
    elem SVG
      [ staticStr "viewBox" ("0 0 " <> show vb.width <> " " <> show vb.height)
      , staticStr "class" "pedal-svg"
      ]
      [ columnHeaders layout
      , knobsTree layout ps callbacks
      , footswitchesTree layout ps callbacks
      , if hasConfig
          then sectionLine (configY - 18.0) <> configRow configY layout ps callbacks
          else empty
      , if hasDips
          then sectionLine (dipBaseY - 14.0) <> dipGrid dipBaseY layout ps callbacks
          else empty
      ]

-- | Column headers from group labels
columnHeaders :: PedalLayout -> Tree
columnHeaders layout =
  forEach "column-headers" Group layout.groups (\g -> g.id) \group ->
    let idx = fromMaybe 0 (findIndex_ (\g -> g.id == group.id) layout.groups)
        x = colXFor layout.viewBox.width layout.columns idx
    in elem Text
      [ thunkedNum "x" x
      , staticNum "y" 12.0
      , staticStr "text-anchor" "middle"
      , staticStr "font-size" "10"
      , staticStr "font-weight" "700"
      , staticStr "fill" "#999"
      , staticStr "letter-spacing" "0.05em"
      , thunkedStr "textContent" group.label
      ] []

-- | All knobs
knobsTree :: PedalLayout -> PedalState -> PedalCallbacks -> Tree
knobsTree layout ps callbacks =
  forEach "knobs" Group layout.knobs knobKey \knob ->
    knobTree layout knob ps callbacks

knobKey :: KnobDef -> String
knobKey k = show k.col <> ":" <> show k.row

-- | Single knob (dual or single layer)
knobTree :: PedalLayout -> KnobDef -> PedalState -> PedalCallbacks -> Tree
knobTree layout knob ps callbacks =
  let
    cx = 0.0
    cy = 0.0
    color = groupColor layout.groups knob.group
    colorMuted = groupMutedColor layout.groups knob.group
    primaryVal = lookupCC knob.primaryCC ps
    primaryLabel = resolveLabel ps knob.primaryLabel
    x = colXFor layout.viewBox.width layout.columns knob.col
    y = rowYFor knob.row
  in
    elem Group
      [ thunkedStr "transform" ("translate(" <> show x <> "," <> show y <> ")") ]
      [ case knob.hiddenCC of
          Just hCC -> dualLayerKnob cx cy color colorMuted knob primaryVal primaryLabel hCC ps callbacks
          Nothing -> singleLayerKnob cx cy color knob primaryVal primaryLabel ps callbacks
      ]

-- | Dual layer knob (outer + inner arcs)
dualLayerKnob :: Number -> Number -> String -> String -> KnobDef -> Int -> String -> CC -> PedalState -> PedalCallbacks -> Tree
dualLayerKnob cx cy color colorMuted knob primaryVal primaryLabel hCC ps callbacks =
  let
    hiddenVal = lookupCC hCC ps
    hiddenLabel = case knob.hiddenLabel of
      Just hl -> resolveLabel ps hl
      Nothing -> ""
    hiddenLayer = fromMaybe (ContinuousKnob { center: Nothing }) knob.hiddenLayer
    primarySeg = isSegmented knob.primaryLayer
    hiddenSeg = isSegmented hiddenLayer
    hasCenterDetente = isJust (layerCenter knob.primaryLayer) || isJust (layerCenter hiddenLayer)
    outerCursor = if primarySeg then "pointer" else "ns-resize"
    innerCursor = if hiddenSeg then "pointer" else "ns-resize"
  in
    siblings
      [ -- Background tracks (drag targets for continuous knobs)
        (if primarySeg then identity
         else withBehaviors [ onPointerDown (callbacks.onKnobDragStart knob.primaryCC primaryVal) ])
        $ elem Path
          [ thunkedStr "d" (donutSegmentPath cx cy outerRadius gapRadius minAngle maxAngle)
          , staticStr "fill" "#e8e8e8"
          , thunkedStr "style" ("cursor:" <> outerCursor)
          ] []
      , (if hiddenSeg then identity
         else withBehaviors [ onPointerDown (callbacks.onKnobDragStart hCC hiddenVal) ])
        $ elem Path
          [ thunkedStr "d" (donutSegmentPath cx cy gapRadius innerRadius minAngle maxAngle)
          , staticStr "fill" "#f0f0f0"
          , thunkedStr "style" ("cursor:" <> innerCursor)
          ] []
      -- Noon tick
      , if hasCenterDetente
          then elem Line
            [ thunkedNum "x1" cx, thunkedNum "y1" (cy - innerRadius)
            , thunkedNum "x2" cx, thunkedNum "y2" (cy - outerRadius)
            , staticStr "stroke" "#ccc"
            , staticStr "stroke-width" "1"
            ] []
          else empty
      -- Outer active arc (primary)
      , if primarySeg
          then segmentedArc cx cy gapRadius outerRadius color primaryVal knob.primaryCC callbacks
          else continuousArc cx cy gapRadius outerRadius color "0.85" primaryVal (layerCenter knob.primaryLayer)
      -- Inner active arc (hidden)
      , if hiddenSeg
          then segmentedArc cx cy innerRadius gapRadius "#888" hiddenVal hCC callbacks
          else continuousArc cx cy innerRadius gapRadius color "0.45" hiddenVal (layerCenter hiddenLayer)
      -- Center: dual values
      , elem Text
          [ thunkedNum "x" cx, thunkedNum "y" (cy + 4.0)
          , staticStr "text-anchor" "middle"
          , staticStr "font-size" "8"
          , staticStr "font-weight" "600"
          , thunkedStr "textContent" (show primaryVal <> " / " <> show hiddenVal)
          , thunkedStr "fill" color
          ] []
      -- Primary label
      , labelText cx (cy + 35.0) "7" "600" color primaryLabel
      -- Hidden label
      , labelText cx (cy + 44.0) "6" "" colorMuted hiddenLabel
      ]

-- | Single layer knob
singleLayerKnob :: Number -> Number -> String -> KnobDef -> Int -> String -> PedalState -> PedalCallbacks -> Tree
singleLayerKnob cx cy color knob primaryVal primaryLabel _ps callbacks =
  let
    primarySeg = isSegmented knob.primaryLayer
    outerCursor = if primarySeg then "pointer" else "ns-resize"
  in
    siblings
      [ -- Background track
        (if primarySeg then identity
         else withBehaviors [ onPointerDown (callbacks.onKnobDragStart knob.primaryCC primaryVal) ])
        $ elem Path
          [ thunkedStr "d" (donutSegmentPath cx cy outerRadius innerRadius minAngle maxAngle)
          , staticStr "fill" "#e8e8e8"
          , thunkedStr "style" ("cursor:" <> outerCursor)
          ] []
      -- Noon tick
      , if isJust (layerCenter knob.primaryLayer)
          then elem Line
            [ thunkedNum "x1" cx, thunkedNum "y1" (cy - innerRadius)
            , thunkedNum "x2" cx, thunkedNum "y2" (cy - outerRadius)
            , staticStr "stroke" "#ccc"
            , staticStr "stroke-width" "1"
            ] []
          else empty
      -- Active arc
      , if primarySeg
          then segmentedArc cx cy innerRadius outerRadius color primaryVal knob.primaryCC callbacks
          else continuousArc cx cy innerRadius outerRadius color "0.85" primaryVal (layerCenter knob.primaryLayer)
      -- Center value
      , elem Text
          [ thunkedNum "x" cx, thunkedNum "y" (cy + 4.0)
          , staticStr "text-anchor" "middle"
          , staticStr "font-size" "10"
          , staticStr "font-weight" "600"
          , thunkedStr "fill" color
          , thunkedStr "textContent" (show primaryVal)
          ] []
      -- Label
      , labelText cx (cy + 35.0) "7" "600" color primaryLabel
      ]

-- | Continuous arc with optional center detente
continuousArc :: Number -> Number -> Number -> Number -> String -> String -> Int -> Maybe Int -> Tree
continuousArc cx cy innerR outerR color opacity val center =
  let angle = valToAngle val
  in case center of
    Nothing ->
      if abs (angle - minAngle) > 0.01
        then elem Path
          [ thunkedStr "d" (donutSegmentPath cx cy outerR innerR minAngle angle)
          , thunkedStr "fill" color
          , thunkedStr "opacity" opacity
          ] []
        else empty
    Just c ->
      let centerAngle = valToAngle c
      in if val == c then empty
         else if val > c then
           elem Path
             [ thunkedStr "d" (donutSegmentPath cx cy outerR innerR centerAngle angle)
             , thunkedStr "fill" color
             , thunkedStr "opacity" opacity
             ] []
         else
           elem Path
             [ thunkedStr "d" (donutSegmentPath cx cy outerR innerR angle centerAngle)
             , thunkedStr "fill" color
             , thunkedStr "opacity" opacity
             ] []

-- | Segmented arc (3 positions)
segmentedArc :: Number -> Number -> Number -> Number -> String -> Int -> CC -> PedalCallbacks -> Tree
segmentedArc cx cy innerR outerR color val theCC callbacks =
  let
    seg = if val < 43 then 0 else if val < 85 then 1 else 2
    gap = 0.06
    segStart i = case i of
      0 -> minAngle
      1 -> -pi / 3.0
      _ -> pi / 3.0
    segEnd i = case i of
      0 -> -pi / 3.0
      1 -> pi / 3.0
      _ -> maxAngle
    segMidiVal i = case i of
      0 -> 0
      1 -> 64
      _ -> 127
    renderSeg i =
      let
        startA = segStart i + gap
        endA = segEnd i - gap
        active = i == seg
        op = if active then "0.85" else "0.12"
      in
        withBehaviors [ onClick (callbacks.onSegmentClick theCC (segMidiVal i)) ]
        $ elem Path
          [ thunkedStr "d" (donutSegmentPath cx cy outerR innerR startA endA)
          , thunkedStr "fill" color
          , thunkedStr "opacity" op
          , staticStr "style" "cursor:pointer"
          ] []
  in siblings [ renderSeg 0, renderSeg 1, renderSeg 2 ]

-- | Label text with halo
labelText :: Number -> Number -> String -> String -> String -> String -> Tree
labelText x y fontSize fontWeight color text =
  elem Text
    ([ thunkedNum "x" x, thunkedNum "y" y
    , staticStr "text-anchor" "middle"
    , thunkedStr "font-size" fontSize
    , thunkedStr "fill" color
    , staticStr "paint-order" "stroke"
    , staticStr "stroke" "rgba(245,245,245,0.85)"
    , staticStr "stroke-width" "3"
    , staticStr "stroke-linejoin" "round"
    , thunkedStr "textContent" text
    ] <> if fontWeight /= "" then [ thunkedStr "font-weight" fontWeight ] else [])
    []

-- | Footswitches
footswitchesTree :: PedalLayout -> PedalState -> PedalCallbacks -> Tree
footswitchesTree layout ps callbacks =
  forEach "footswitches" Group layout.footswitches (\fs -> show fs.col) \fs ->
    footswitchTree layout fs ps callbacks

footswitchTree :: PedalLayout -> FootswitchDef -> PedalState -> PedalCallbacks -> Tree
footswitchTree layout fs ps callbacks =
  let
    cx = fsXFor layout.viewBox.width layout.columns fs.col
    cy = fsRowY layout.knobRows
    fsVal = lookupCC fs.cc ps
    ledVal = case fs.ledCC of
      Just lcc -> lookupCC lcc ps
      Nothing -> 0
    isOn = fsVal > 63
    ledOn = ledVal > 63
    strokeColor = groupColor layout.groups fs.group
    fillColor
      | not isOn  = "#ddd"
      | ledOn     = fs.ledColor
      | otherwise = fs.engagedColor
    textColor
      | isOn      = "#fff"
      | otherwise = "#999"
  in
    withBehaviors [ onClick (callbacks.onToggleClick fs.cc fsVal) ]
    $ elem Group
      [ thunkedStr "transform" ("translate(" <> show cx <> "," <> show cy <> ")")
      , staticStr "style" "cursor:pointer"
      ]
      [ elem Circle
          [ staticNum "cx" 0.0, staticNum "cy" 0.0
          , staticNum "r" 22.0
          , thunkedStr "fill" fillColor
          , thunkedStr "stroke" strokeColor
          , staticStr "stroke-width" "2"
          ] []
      , elem Text
          [ staticNum "x" 0.0, staticNum "y" 5.0
          , staticStr "text-anchor" "middle"
          , staticStr "font-size" "11"
          , staticStr "font-weight" "600"
          , thunkedStr "fill" textColor
          , staticStr "pointer-events" "none"
          , thunkedStr "textContent" fs.label
          ] []
      ]

-- | Config row
configRow :: Number -> PedalLayout -> PedalState -> PedalCallbacks -> Tree
configRow baseY layout ps callbacks =
  let
    configs = layout.config
    n = Array.length configs
    spacing = if n <= 1 then 0.0 else (layout.viewBox.width - 100.0) / toNumber (n - 1)
    startX = 50.0
  in
    siblings (Array.mapWithIndex (renderConfig baseY startX spacing ps callbacks) configs)

renderConfig :: Number -> Number -> Number -> PedalState -> PedalCallbacks -> Int -> ConfigDef -> Tree
renderConfig baseY startX spacing ps callbacks idx cfg =
  let
    x = startX + toNumber idx * spacing
    val = lookupCC cfg.cc ps
  in case cfg.controlType of
    CfgToggle ->
      let isOn = val > 63
      in withBehaviors [ onClick (callbacks.onToggleClick cfg.cc val) ]
         $ elem Group [ staticStr "style" "cursor:pointer" ]
           [ elem Circle
               [ thunkedNum "cx" x, thunkedNum "cy" baseY
               , staticNum "r" 4.0
               , thunkedStr "fill" (if isOn then "#555" else "#ddd")
               ] []
           , elem Text
               [ thunkedNum "x" x, thunkedNum "y" (baseY + 15.0)
               , staticStr "text-anchor" "middle"
               , staticStr "font-size" "6"
               , staticStr "fill" "#999"
               , staticStr "pointer-events" "none"
               , thunkedStr "textContent" cfg.label
               ] []
           ]
    CfgSlider ->
      let
        barW = 40.0
        barH = 6.0
        fillW = barW * toNumber val / 127.0
        bx = x - barW / 2.0
        by = baseY - barH / 2.0
      in
        withBehaviors [ onPointerDown (callbacks.onKnobDragStart cfg.cc val) ]
        $ elem Group [ staticStr "style" "cursor:ns-resize" ]
          [ elem Rect
              [ thunkedNum "x" bx, thunkedNum "y" by
              , staticNum "width" barW, staticNum "height" barH
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
              [ thunkedNum "x" x, thunkedNum "y" (baseY + 15.0)
              , staticStr "text-anchor" "middle"
              , staticStr "font-size" "6"
              , staticStr "fill" "#999"
              , staticStr "pointer-events" "none"
              , thunkedStr "textContent" cfg.label
              ] []
          ]

-- | DIP switch grid
dipGrid :: Number -> PedalLayout -> PedalState -> PedalCallbacks -> Tree
dipGrid baseY layout ps callbacks =
  let
    banks = layout.dipBanks
    numBanks = Array.length banks
    dotY = baseY + 12.0
    spacing = 17.0
  in
    siblings (Array.mapWithIndex (renderBank layout.viewBox.width numBanks dotY spacing ps callbacks) banks)

renderBank :: Number -> Int -> Number -> Number -> PedalState -> PedalCallbacks -> Int -> DipBankDef -> Tree
renderBank viewWidth numBanks dotY spacing ps callbacks bankIdx bank =
  let
    bankCenterX = viewWidth * (2.0 * toNumber bankIdx + 1.0) / (2.0 * toNumber numBanks)
    numSwitches = Array.length bank.switches
    bankStartX = bankCenterX - toNumber (numSwitches - 1) * spacing / 2.0
    header = elem Text
      [ thunkedNum "x" bankCenterX, thunkedNum "y" (dotY - 12.0)
      , staticStr "text-anchor" "middle"
      , staticStr "font-size" "7"
      , staticStr "font-weight" "600"
      , staticStr "fill" "#999"
      , thunkedStr "textContent" bank.label
      ] []
  in
    header <> siblings (map (renderSwitch bankStartX dotY spacing ps callbacks) bank.switches)

renderSwitch :: Number -> Number -> Number -> PedalState -> PedalCallbacks -> { cc :: CC, label :: String, index :: Int } -> Tree
renderSwitch bankStartX dotY spacing ps callbacks sw =
  let
    bx = bankStartX + toNumber sw.index * spacing
    val = lookupCC sw.cc ps
    isOn = val > 63
    labelX = bx + 2.0
    labelBaseY = dotY + 8.0
  in
    withBehaviors [ onClick (callbacks.onToggleClick sw.cc val) ]
    $ elem Group [ staticStr "style" "cursor:pointer" ]
      [ elem Circle
          [ thunkedNum "cx" bx, thunkedNum "cy" dotY
          , staticNum "r" 4.0
          , thunkedStr "fill" (if isOn then "#555" else "#ddd")
          ] []
      , elem Text
          [ thunkedNum "x" labelX, thunkedNum "y" labelBaseY
          , thunkedStr "transform" ("rotate(45," <> show labelX <> "," <> show labelBaseY <> ")")
          , staticStr "text-anchor" "start"
          , staticStr "font-size" "6"
          , thunkedStr "fill" (if isOn then "#555" else "#bbb")
          , staticStr "pointer-events" "none"
          , thunkedStr "textContent" sw.label
          ] []
      ]

-- | Thin horizontal separator line
sectionLine :: Number -> Tree
sectionLine y =
  elem Line
    [ staticNum "x1" 40.0, thunkedNum "y1" y
    , staticNum "x2" 280.0, thunkedNum "y2" y
    , staticStr "stroke" "#e0e0e0"
    , staticStr "stroke-width" "0.5"
    ] []

-- | Find index of first matching element
findIndex_ :: forall a. (a -> Boolean) -> Array a -> Maybe Int
findIndex_ p arr = go 0
  where
  go i = case Array.index arr i of
    Nothing -> Nothing
    Just x -> if p x then Just i else go (i + 1)
