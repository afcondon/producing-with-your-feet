module Component.Pedal.Donut
  ( renderDonut
  , renderFootswitch
  , DonutConfig
  ) where

import Prelude

import Component.Pedal.MoodLayout (Channel(..), KnobPair, KnobType(..), Footswitch)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (cos, sin, pi, abs)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | SVG namespace
svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

-- | Knob sweep range: 300° from 7 o'clock to 5 o'clock
-- | minAngle = -5π/6 (7 o'clock, value 0)
-- | maxAngle = +5π/6 (5 o'clock, value 127)
-- | noon = 0 (12 o'clock, ~value 63)
minAngle :: Number
minAngle = -5.0 * pi / 6.0

maxAngle :: Number
maxAngle = 5.0 * pi / 6.0

sweepRange :: Number
sweepRange = maxAngle - minAngle

-- | Channel-based fill colors
channelColor :: Channel -> String
channelColor = case _ of
  Wet -> "#7b4f8a"    -- MOOD purple
  ML -> "#4a7b6a"     -- complementary green
  Shared -> "#666"    -- neutral gray

channelColorMuted :: Channel -> String
channelColorMuted = case _ of
  Wet -> "#d4c0dc"
  ML -> "#b8d4c8"
  Shared -> "#ccc"

type DonutConfig =
  { outerRadius :: Number
  , innerRadius :: Number
  , gapRadius :: Number
  }

defaultConfig :: DonutConfig
defaultConfig =
  { outerRadius: 40.0
  , innerRadius: 16.0
  , gapRadius: 28.0
  }

-- Arc path generation (inlined from Hylograph.Shape.Arc)
donutSegmentPath :: Number -> Number -> Number -> Number -> Number -> Number -> Number -> String
donutSegmentPath cx cy outerR innerR startAngle endAngle _sweep =
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

-- | Convert MIDI value 0..127 to angle within the 300° knob sweep
valToAngle :: Int -> Number
valToAngle v = minAngle + (toNumber v / 127.0) * sweepRange

-- | SVG element helpers
svgElement :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElement name = HH.elementNS (HH.Namespace svgNS) (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr name val = HP.attr (HH.AttrName name) val

-- | Render a continuous arc with optional center detente
renderContinuousArc :: forall w i. Number -> Number -> Number -> Number -> String -> String -> Int -> Maybe Int -> HH.HTML w i
renderContinuousArc cx cy innerR outerR color opacity val center =
  let
    angle = valToAngle val
  in
    case center of
      Nothing ->
        -- No center: arc from minAngle to value angle
        if abs (angle - minAngle) > 0.01
          then svgElement "path"
            [ svgAttr "d" (donutSegmentPath cx cy outerR innerR minAngle angle 1.0)
            , svgAttr "fill" color
            , svgAttr "opacity" opacity
            ] []
          else svgElement "g" [] []
      Just c ->
        let
          centerAngle = valToAngle c
        in
          if val == c then
            -- At center: no active arc
            svgElement "g" [] []
          else if val > c then
            -- Above center: arc from noon (centerAngle) clockwise to value
            svgElement "path"
              [ svgAttr "d" (donutSegmentPath cx cy outerR innerR centerAngle angle 1.0)
              , svgAttr "fill" color
              , svgAttr "opacity" opacity
              ] []
          else
            -- Below center: arc from value up to noon (centerAngle)
            svgElement "path"
              [ svgAttr "d" (donutSegmentPath cx cy outerR innerR angle centerAngle 1.0)
              , svgAttr "fill" color
              , svgAttr "opacity" opacity
              ] []

-- | Render a nested donut for one knob pair position
renderDonut :: forall w i. KnobPair -> { primaryVal :: Int, hiddenVal :: Int } -> HH.HTML w i
renderDonut knob vals =
  let
    cfg = defaultConfig
    cx = 0.0
    cy = 0.0
    color = channelColor knob.channel
    colorMuted = channelColorMuted knob.channel
    isToggle typ = typ == Segmented
  in
    svgElement "g"
      [ svgAttr "transform" ("translate(" <> show (colX knob.col) <> "," <> show (rowY knob.row) <> ")") ]
      -- Background tracks (300° arcs matching knob sweep)
      [ svgElement "path"
          [ svgAttr "d" (donutSegmentPath cx cy cfg.outerRadius cfg.gapRadius minAngle maxAngle 1.0)
          , svgAttr "fill" "#e8e8e8"
          ] []
      , svgElement "path"
          [ svgAttr "d" (donutSegmentPath cx cy cfg.gapRadius cfg.innerRadius minAngle maxAngle 1.0)
          , svgAttr "fill" "#f0f0f0"
          ] []
      -- Outer active arc (primary value)
      , if isToggle knob.primaryType
          then renderSegmentedArc cx cy cfg.gapRadius cfg.outerRadius color vals.primaryVal
          else renderContinuousArc cx cy cfg.gapRadius cfg.outerRadius color "0.85" vals.primaryVal knob.primaryCenter
      -- Inner active arc (hidden value)
      , if isToggle knob.hiddenType
          then renderSegmentedArc cx cy cfg.innerRadius cfg.gapRadius "#888" vals.hiddenVal
          else renderContinuousArc cx cy cfg.innerRadius cfg.gapRadius color "0.45" vals.hiddenVal knob.hiddenCenter
      -- Center label: primary
      , svgElement "text"
          [ svgAttr "x" (show cx), svgAttr "y" (show (cy - 2.0))
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "8"
          , svgAttr "font-weight" "600"
          , svgAttr "fill" color
          ] [ HH.text knob.primaryLabel ]
      -- Center value
      , svgElement "text"
          [ svgAttr "x" (show cx), svgAttr "y" (show (cy + 8.0))
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "7"
          , svgAttr "font-weight" "500"
          , svgAttr "fill" "#666"
          ] [ HH.text (show vals.primaryVal) ]
      -- Outer label: hidden param name
      , svgElement "text"
          [ svgAttr "x" (show cx), svgAttr "y" (show (cy + cfg.outerRadius + 10.0))
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "6"
          , svgAttr "fill" colorMuted
          ] [ HH.text knob.hiddenLabel ]
      ]

-- | For segmented (toggle) controls, render a 3-position indicator
-- | Segments match knob sweep: left (7→10), center (10→2), right (2→5)
renderSegmentedArc :: forall w i. Number -> Number -> Number -> Number -> String -> Int -> HH.HTML w i
renderSegmentedArc cx cy innerR outerR color val =
  let
    -- Map the value to one of 3 segments (0-42 = 0, 43-84 = 1, 85-127 = 2)
    seg = if val < 43 then 0 else if val < 85 then 1 else 2
    gap = 0.06
    -- Three segments within the knob sweep:
    -- Left:   -5π/6 to -π/3  (7 o'clock to 10 o'clock)
    -- Center: -π/3  to +π/3  (10 o'clock to 2 o'clock)
    -- Right:  +π/3  to +5π/6 (2 o'clock to 5 o'clock)
    segStart i = case i of
      0 -> minAngle            -- -5π/6
      1 -> -pi / 3.0           -- -π/3
      _ -> pi / 3.0            -- +π/3
    segEnd i = case i of
      0 -> -pi / 3.0           -- -π/3
      1 -> pi / 3.0            -- +π/3
      _ -> maxAngle            -- +5π/6
    renderSeg i =
      let
        startA = segStart i + gap
        endA = segEnd i - gap
        active = i == seg
        opacity = if active then "0.85" else "0.12"
      in
        svgElement "path"
          [ svgAttr "d" (donutSegmentPath cx cy outerR innerR startA endA 1.0)
          , svgAttr "fill" color
          , svgAttr "opacity" opacity
          ] []
  in
    svgElement "g" [] [ renderSeg 0, renderSeg 1, renderSeg 2 ]

-- | Render a footswitch indicator
renderFootswitch :: forall w i. Footswitch -> Int -> HH.HTML w i
renderFootswitch fs val =
  let
    cx = fsX fs.col
    cy = rowY 3
    isOn = val > 63
    color = channelColor fs.channel
    fillColor = if isOn then color else "#ddd"
  in
    svgElement "g"
      [ svgAttr "transform" ("translate(" <> show cx <> "," <> show cy <> ")") ]
      [ svgElement "circle"
          [ svgAttr "cx" "0", svgAttr "cy" "0"
          , svgAttr "r" "22"
          , svgAttr "fill" fillColor
          , svgAttr "stroke" color
          , svgAttr "stroke-width" "2"
          ] []
      , svgElement "text"
          [ svgAttr "x" "0", svgAttr "y" "5"
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "11"
          , svgAttr "font-weight" "600"
          , svgAttr "fill" (if isOn then "#fff" else "#999")
          ] [ HH.text fs.label ]
      ]

-- | Footswitch X positions — centered between knob columns
fsX :: Int -> Number
fsX 0 = 110.0  -- midpoint of col 0 (60) and col 1 (160) → Wet
fsX _ = 210.0  -- midpoint of col 1 (160) and col 2 (260) → ML

-- | Column X positions (3 columns, centered in viewBox)
colX :: Int -> Number
colX 0 = 60.0   -- Wet (left)
colX 1 = 160.0  -- Shared (center)
colX _ = 260.0  -- ML (right)

-- | Row Y positions (4 rows: A, B, C knobs + D footswitches)
rowY :: Int -> Number
rowY 0 = 60.0   -- Row A
rowY 1 = 160.0  -- Row B
rowY 2 = 250.0  -- Row C
rowY _ = 330.0   -- Row D (footswitches)
