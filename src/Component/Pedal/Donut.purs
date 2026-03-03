module Component.Pedal.Donut
  ( renderDonut
  , renderFootswitch
  , DonutConfig
  ) where

import Prelude

import Component.Pedal.MoodLayout (Channel(..), KnobPair, KnobType(..), Footswitch)
import Data.Int (toNumber)
import Data.Number (cos, sin, pi)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | SVG namespace
svgNS :: String
svgNS = "http://www.w3.org/2000/svg"

-- | Full circle
tau :: Number
tau = 2.0 * pi

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

-- | Convert MIDI value 0..127 to angle 0..tau
valToAngle :: Int -> Number
valToAngle v = (toNumber v / 127.0) * tau

-- | SVG element helpers
svgElement :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElement name = HH.elementNS (HH.Namespace svgNS) (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr name val = HP.attr (HH.AttrName name) val

-- | Render a nested donut for one knob pair position
renderDonut :: forall w i. KnobPair -> { primaryVal :: Int, hiddenVal :: Int } -> HH.HTML w i
renderDonut knob vals =
  let
    cfg = defaultConfig
    cx = 0.0
    cy = 0.0
    color = channelColor knob.channel
    colorMuted = channelColorMuted knob.channel
    primaryAngle = valToAngle vals.primaryVal
    hiddenAngle = valToAngle vals.hiddenVal
    isToggle typ = typ == Segmented
  in
    svgElement "g"
      [ svgAttr "transform" ("translate(" <> show (colX knob.col) <> "," <> show (rowY knob.row) <> ")") ]
      -- Background tracks (full circle outlines)
      [ svgElement "circle"
          [ svgAttr "cx" (show cx), svgAttr "cy" (show cy)
          , svgAttr "r" (show ((cfg.outerRadius + cfg.gapRadius) / 2.0))
          , svgAttr "fill" "none"
          , svgAttr "stroke" "#e8e8e8"
          , svgAttr "stroke-width" (show (cfg.outerRadius - cfg.gapRadius))
          ] []
      , svgElement "circle"
          [ svgAttr "cx" (show cx), svgAttr "cy" (show cy)
          , svgAttr "r" (show ((cfg.gapRadius + cfg.innerRadius) / 2.0))
          , svgAttr "fill" "none"
          , svgAttr "stroke" "#f0f0f0"
          , svgAttr "stroke-width" (show (cfg.gapRadius - cfg.innerRadius))
          ] []
      -- Outer active arc (primary value)
      , if primaryAngle > 0.001
          then if isToggle knob.primaryType
            then renderSegmentedArc cx cy cfg.gapRadius cfg.outerRadius color vals.primaryVal
            else svgElement "path"
              [ svgAttr "d" (donutSegmentPath cx cy cfg.outerRadius cfg.gapRadius 0.0 primaryAngle 1.0)
              , svgAttr "fill" color
              , svgAttr "opacity" "0.85"
              ] []
          else svgElement "g" [] []
      -- Inner active arc (hidden value)
      , if hiddenAngle > 0.001
          then if isToggle knob.hiddenType
            then renderSegmentedArc cx cy cfg.innerRadius cfg.gapRadius "#888" vals.hiddenVal
            else svgElement "path"
              [ svgAttr "d" (donutSegmentPath cx cy cfg.gapRadius cfg.innerRadius 0.0 hiddenAngle 1.0)
              , svgAttr "fill" color
              , svgAttr "opacity" "0.45"
              ] []
          else svgElement "g" [] []
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
renderSegmentedArc :: forall w i. Number -> Number -> Number -> Number -> String -> Int -> HH.HTML w i
renderSegmentedArc cx cy innerR outerR color val =
  let
    -- Map the value to one of 3 segments (0-42 = 0, 43-84 = 1, 85-127 = 2)
    seg = if val < 43 then 0 else if val < 85 then 1 else 2
    segAngle = tau / 3.0
    gap = 0.06
    renderSeg i =
      let
        startA = toNumber i * segAngle + gap
        endA = toNumber (i + 1) * segAngle - gap
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
    cx = colX fs.col
    cy = rowY 3
    isOn = val > 63
    color = channelColor fs.channel
    fillColor = if isOn then color else "#ddd"
  in
    svgElement "g"
      [ svgAttr "transform" ("translate(" <> show cx <> "," <> show cy <> ")") ]
      [ svgElement "circle"
          [ svgAttr "cx" "0", svgAttr "cy" "0"
          , svgAttr "r" "12"
          , svgAttr "fill" fillColor
          , svgAttr "stroke" color
          , svgAttr "stroke-width" "2"
          ] []
      , svgElement "text"
          [ svgAttr "x" "0", svgAttr "y" "4"
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "8"
          , svgAttr "font-weight" "600"
          , svgAttr "fill" (if isOn then "#fff" else "#999")
          ] [ HH.text fs.label ]
      ]

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
