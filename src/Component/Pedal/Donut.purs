module Component.Pedal.Donut
  ( renderDonut
  , renderFootswitch
  , renderConfigRow
  , renderDipGrid
  , renderSectionLine
  , DonutConfig
  , DonutEvent(..)
  ) where

import Prelude

import Component.Pedal.MoodLayout (Channel(..), KnobPair, KnobType(..), Footswitch, DIPBank(..), DIPSwitch, ConfigType(..), ConfigControl)
import Data.Array (zipWith) as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Midi (CC)
import Data.Number (cos, sin, pi, abs)
import Data.Pedal (LabelSource(..))
import Engine (PedalState)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.UIEvent.MouseEvent (MouseEvent)

-- | Events emitted by interactive donut elements
data DonutEvent
  = KnobDragStart CC Int MouseEvent  -- CC, current value, mouse event (for clientY)
  | SegmentClick CC Int               -- CC, target MIDI value
  | ToggleClick CC Int                -- CC, current value (caller flips)

-- | SVG-compatible event handlers (no row constraints)
svgOnMouseDown :: forall r i. (MouseEvent -> i) -> HH.IProp r i
svgOnMouseDown f = HE.handler (EventType "mousedown") (unsafeCoerce f)

svgOnClick :: forall r i. (MouseEvent -> i) -> HH.IProp r i
svgOnClick f = HE.handler (EventType "click") (unsafeCoerce f)

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

-- | Resolve a LabelSource to a concrete string given the current pedal state
resolveLabel :: PedalState -> LabelSource -> String
resolveLabel ps = case _ of
  Static s -> s
  ModeMap r ->
    case Map.lookup r.cc ps.values of
      Just val -> fromMaybe "?" (Map.lookup val r.labels)
      Nothing -> "?"
  ChannelMode _ -> "?"

-- | Render a nested donut for one knob pair position
renderDonut :: forall w i. KnobPair -> PedalState -> { primaryVal :: Int, hiddenVal :: Int } -> (DonutEvent -> i) -> HH.HTML w i
renderDonut knob ps vals toAction =
  let
    cfg = defaultConfig
    cx = 0.0
    cy = 0.0
    color = channelColor knob.channel
    colorMuted = channelColorMuted knob.channel
    isToggle typ = typ == Segmented
    primaryLabel = resolveLabel ps knob.primaryLabel
    hiddenLabel = resolveLabel ps knob.hiddenLabel
    -- Event helpers for outer arc (primary)
    outerEvt = if isToggle knob.primaryType
      then segmentClickEvents knob.primaryCC toAction
      else dragStartEvents knob.primaryCC vals.primaryVal toAction
    -- Event helpers for inner arc (hidden)
    innerEvt = if isToggle knob.hiddenType
      then segmentClickEvents knob.hiddenCC toAction
      else dragStartEvents knob.hiddenCC vals.hiddenVal toAction
    outerCursor = if isToggle knob.primaryType then "pointer" else "ns-resize"
    innerCursor = if isToggle knob.hiddenType then "pointer" else "ns-resize"
  in
    svgElement "g"
      [ svgAttr "transform" ("translate(" <> show (colX knob.col) <> "," <> show (rowY knob.row) <> ")") ]
      -- Background tracks (300° arcs matching knob sweep)
      [ svgElement "path"
          ([ svgAttr "d" (donutSegmentPath cx cy cfg.outerRadius cfg.gapRadius minAngle maxAngle 1.0)
          , svgAttr "fill" "#e8e8e8"
          , svgAttr "style" ("cursor:" <> outerCursor)
          ] <> outerEvt) []
      , svgElement "path"
          ([ svgAttr "d" (donutSegmentPath cx cy cfg.gapRadius cfg.innerRadius minAngle maxAngle 1.0)
          , svgAttr "fill" "#f0f0f0"
          , svgAttr "style" ("cursor:" <> innerCursor)
          ] <> innerEvt) []
      -- Noon tick for center-detente knobs
      , if isJust knob.primaryCenter || isJust knob.hiddenCenter
          then svgElement "line"
            [ svgAttr "x1" (show cx), svgAttr "y1" (show (cy - cfg.innerRadius))
            , svgAttr "x2" (show cx), svgAttr "y2" (show (cy - cfg.outerRadius))
            , svgAttr "stroke" "#ccc"
            , svgAttr "stroke-width" "1"
            ] []
          else svgElement "g" [] []
      -- Outer active arc (primary value)
      , if isToggle knob.primaryType
          then renderSegmentedArc cx cy cfg.gapRadius cfg.outerRadius color vals.primaryVal knob.primaryCC toAction
          else renderContinuousArc cx cy cfg.gapRadius cfg.outerRadius color "0.85" vals.primaryVal knob.primaryCenter
      -- Inner active arc (hidden value)
      , if isToggle knob.hiddenType
          then renderSegmentedArc cx cy cfg.innerRadius cfg.gapRadius "#888" vals.hiddenVal knob.hiddenCC toAction
          else renderContinuousArc cx cy cfg.innerRadius cfg.gapRadius color "0.45" vals.hiddenVal knob.hiddenCenter
      -- Center: dual values (color-coded)
      , svgElement "text"
          [ svgAttr "x" (show cx), svgAttr "y" (show (cy + 4.0))
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "8"
          , svgAttr "font-weight" "600"
          ]
          [ svgElement "tspan" [ svgAttr "fill" color ] [ HH.text (show vals.primaryVal) ]
          , svgElement "tspan" [ svgAttr "fill" "#999" ] [ HH.text " / " ]
          , svgElement "tspan" [ svgAttr "fill" colorMuted ] [ HH.text (show vals.hiddenVal) ]
          ]
      -- Primary label (in the 60° gap at bottom of arc)
      , svgElement "text"
          [ svgAttr "x" (show cx), svgAttr "y" (show (cy + 35.0))
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "7"
          , svgAttr "font-weight" "600"
          , svgAttr "fill" color
          , svgAttr "paint-order" "stroke"
          , svgAttr "stroke" "rgba(245,245,245,0.85)"
          , svgAttr "stroke-width" "3"
          , svgAttr "stroke-linejoin" "round"
          ] [ HH.text primaryLabel ]
      -- Hidden label (just below, still in gap)
      , svgElement "text"
          [ svgAttr "x" (show cx), svgAttr "y" (show (cy + 44.0))
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "6"
          , svgAttr "fill" colorMuted
          , svgAttr "paint-order" "stroke"
          , svgAttr "stroke" "rgba(245,245,245,0.85)"
          , svgAttr "stroke-width" "3"
          , svgAttr "stroke-linejoin" "round"
          ] [ HH.text hiddenLabel ]
      ]

-- | Event attrs for drag-start on a continuous knob
dragStartEvents :: forall r i. CC -> Int -> (DonutEvent -> i) -> Array (HH.IProp r i)
dragStartEvents cc val toAction =
  [ svgOnMouseDown \me -> toAction (KnobDragStart cc val me) ]

-- | Event attrs for click on a segmented knob (delegates to background arc)
segmentClickEvents :: forall r i. CC -> (DonutEvent -> i) -> Array (HH.IProp r i)
segmentClickEvents _ _ = []

-- | For segmented (toggle) controls, render a 3-position indicator
-- | Segments match knob sweep: left (7→10), center (10→2), right (2→5)
renderSegmentedArc :: forall w i. Number -> Number -> Number -> Number -> String -> Int -> CC -> (DonutEvent -> i) -> HH.HTML w i
renderSegmentedArc cx cy innerR outerR color val cc toAction =
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
    -- MIDI values for each segment position
    segMidiVal i = case i of
      0 -> 0
      1 -> 64
      _ -> 127
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
          , svgAttr "style" "cursor:pointer"
          , svgOnClick \_ -> toAction (SegmentClick cc (segMidiVal i))
          ] []
  in
    svgElement "g" [] [ renderSeg 0, renderSeg 1, renderSeg 2 ]

-- | Render a footswitch indicator with LED-driven fill color
-- | fsVal: footswitch CC value, ledVal: LED CC value
renderFootswitch :: forall w i. Footswitch -> Int -> Int -> (DonutEvent -> i) -> HH.HTML w i
renderFootswitch fs fsVal ledVal toAction =
  let
    cx = fsX fs.col
    cy = rowY 3
    isOn = fsVal > 63
    ledOn = ledVal > 63
    strokeColor = channelColor fs.channel
    fillColor
      | not isOn  = "#ddd"
      | ledOn     = fs.ledColor
      | otherwise = fs.engagedColor
    textColor
      | isOn      = "#fff"
      | otherwise = "#999"
  in
    svgElement "g"
      [ svgAttr "transform" ("translate(" <> show cx <> "," <> show cy <> ")")
      , svgAttr "style" "cursor:pointer"
      , svgOnClick \_ -> toAction (ToggleClick fs.cc fsVal)
      ]
      [ svgElement "circle"
          [ svgAttr "cx" "0", svgAttr "cy" "0"
          , svgAttr "r" "22"
          , svgAttr "fill" fillColor
          , svgAttr "stroke" strokeColor
          , svgAttr "stroke-width" "2"
          ] []
      , svgElement "text"
          [ svgAttr "x" "0", svgAttr "y" "5"
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "11"
          , svgAttr "font-weight" "600"
          , svgAttr "fill" textColor
          , svgAttr "pointer-events" "none"
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

-- | Render the config row (toggles + speed slider)
renderConfigRow :: forall w i. Array ConfigControl -> (ConfigControl -> Int) -> (DonutEvent -> i) -> HH.HTML w i
renderConfigRow configs lookupVal toAction =
  let
    xs = [ 50.0, 105.0, 160.0, 215.0, 270.0 ]
    cy = 374.0
    renderOne cfg x =
      let val = lookupVal cfg
      in case cfg.controlType of
        ConfigToggle ->
          let isOn = val > 63
          in svgElement "g"
            [ svgAttr "style" "cursor:pointer"
            , svgOnClick \_ -> toAction (ToggleClick cfg.cc val)
            ]
            [ svgElement "circle"
                [ svgAttr "cx" (show x), svgAttr "cy" (show cy)
                , svgAttr "r" "4"
                , svgAttr "fill" (if isOn then "#555" else "#ddd")
                ] []
            , svgElement "text"
                [ svgAttr "x" (show x), svgAttr "y" (show (cy + 15.0))
                , svgAttr "text-anchor" "middle"
                , svgAttr "font-size" "6"
                , svgAttr "fill" "#999"
                , svgAttr "pointer-events" "none"
                ] [ HH.text cfg.label ]
            ]
        ConfigSlider ->
          let
            barW = 40.0
            barH = 6.0
            fillW = barW * toNumber val / 127.0
            bx = x - barW / 2.0
            by = cy - barH / 2.0
          in svgElement "g"
            [ svgAttr "style" "cursor:ns-resize"
            , svgOnMouseDown \me -> toAction (KnobDragStart cfg.cc val me)
            ]
            [ svgElement "rect"
                [ svgAttr "x" (show bx), svgAttr "y" (show by)
                , svgAttr "width" (show barW), svgAttr "height" (show barH)
                , svgAttr "rx" "3", svgAttr "ry" "3"
                , svgAttr "fill" "#e8e8e8"
                ] []
            , svgElement "rect"
                [ svgAttr "x" (show bx), svgAttr "y" (show by)
                , svgAttr "width" (show fillW), svgAttr "height" (show barH)
                , svgAttr "rx" "3", svgAttr "ry" "3"
                , svgAttr "fill" "#888"
                ] []
            , svgElement "text"
                [ svgAttr "x" (show x), svgAttr "y" (show (cy + 15.0))
                , svgAttr "text-anchor" "middle"
                , svgAttr "font-size" "6"
                , svgAttr "fill" "#999"
                , svgAttr "pointer-events" "none"
                ] [ HH.text cfg.label ]
            ]
  in
    svgElement "g" [] (Array.zipWith renderOne configs xs)

-- | Render DIP switch grid: two banks of 8 with rotated labels
renderDipGrid :: forall w i. Array DIPSwitch -> (DIPSwitch -> Int) -> (DonutEvent -> i) -> HH.HTML w i
renderDipGrid dips lookupVal toAction =
  let
    headerY = 410.0
    dotY = 422.0
    bankStartX Ramping = 15.0
    bankStartX Customize = 175.0
    spacing = 17.0
    renderDip dip =
      let
        bx = bankStartX dip.bank + toNumber dip.index * spacing
        val = lookupVal dip
        isOn = val > 63
        labelX = bx + 2.0
        labelBaseY = dotY + 8.0
      in svgElement "g"
        [ svgAttr "style" "cursor:pointer"
        , svgOnClick \_ -> toAction (ToggleClick dip.cc val)
        ]
        [ svgElement "circle"
            [ svgAttr "cx" (show bx), svgAttr "cy" (show dotY)
            , svgAttr "r" "4"
            , svgAttr "fill" (if isOn then "#555" else "#ddd")
            ] []
        , svgElement "text"
            [ svgAttr "x" (show labelX), svgAttr "y" (show labelBaseY)
            , svgAttr "transform" ("rotate(45," <> show labelX <> "," <> show labelBaseY <> ")")
            , svgAttr "text-anchor" "start"
            , svgAttr "font-size" "6"
            , svgAttr "fill" (if isOn then "#555" else "#bbb")
            , svgAttr "pointer-events" "none"
            ] [ HH.text dip.label ]
        ]
  in
    svgElement "g" []
      ([ svgElement "text"
          [ svgAttr "x" "80", svgAttr "y" (show headerY)
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "7"
          , svgAttr "font-weight" "600"
          , svgAttr "fill" "#999"
          ] [ HH.text "Ramping" ]
      , svgElement "text"
          [ svgAttr "x" "240", svgAttr "y" (show headerY)
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "7"
          , svgAttr "font-weight" "600"
          , svgAttr "fill" "#999"
          ] [ HH.text "Customize" ]
      ] <> map renderDip dips)

-- | Thin horizontal separator line
renderSectionLine :: forall w i. Number -> HH.HTML w i
renderSectionLine y =
  svgElement "line"
    [ svgAttr "x1" "40", svgAttr "y1" (show y)
    , svgAttr "x2" "280", svgAttr "y2" (show y)
    , svgAttr "stroke" "#e0e0e0"
    , svgAttr "stroke-width" "0.5"
    ] []

