module Component.Pedal.Donut
  ( renderDonut
  , renderFootswitch
  , renderConfigRow
  , renderDipGrid
  , renderSectionLine
  , colXFor
  , fsRowY
  , DonutConfig
  , DonutEvent(..)
  ) where

import Prelude

import Data.Array (concat, find, length, mapWithIndex) as Array
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Midi (CC, unMidiValue)
import Data.Number (cos, sin, pi, abs)
import Data.Pedal.Label (LabelSource(..))
import Data.Pedal.Layout (ConfigControlType(..), FootswitchDef, GroupStyle, KnobDef, KnobLayer(..), PedalLayout)
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
minAngle :: Number
minAngle = -5.0 * pi / 6.0

maxAngle :: Number
maxAngle = 5.0 * pi / 6.0

sweepRange :: Number
sweepRange = maxAngle - minAngle

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

-- | Arc path generation
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

-- | Position helpers

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

-- | Resolve a LabelSource to a concrete string given the current pedal state
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

-- | Render a continuous arc with optional center detente
renderContinuousArc :: forall w i. Number -> Number -> Number -> Number -> String -> String -> Int -> Maybe Int -> HH.HTML w i
renderContinuousArc cx cy innerR outerR color opacity val center =
  let
    angle = valToAngle val
  in
    case center of
      Nothing ->
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
            svgElement "g" [] []
          else if val > c then
            svgElement "path"
              [ svgAttr "d" (donutSegmentPath cx cy outerR innerR centerAngle angle 1.0)
              , svgAttr "fill" color
              , svgAttr "opacity" opacity
              ] []
          else
            svgElement "path"
              [ svgAttr "d" (donutSegmentPath cx cy outerR innerR angle centerAngle 1.0)
              , svgAttr "fill" color
              , svgAttr "opacity" opacity
              ] []

-- | Event attrs for drag-start on a continuous knob
dragStartEvents :: forall r i. CC -> Int -> (DonutEvent -> i) -> Array (HH.IProp r i)
dragStartEvents theCC val toAction =
  [ svgOnMouseDown \me -> toAction (KnobDragStart theCC val me) ]

-- | Event attrs for click on a segmented knob (delegates to background arc)
segmentClickEvents :: forall r i. CC -> (DonutEvent -> i) -> Array (HH.IProp r i)
segmentClickEvents _ _ = []

-- | For segmented (toggle) controls, render a 3-position indicator
renderSegmentedArc :: forall w i. Number -> Number -> Number -> Number -> String -> Int -> CC -> (DonutEvent -> i) -> HH.HTML w i
renderSegmentedArc cx cy innerR outerR color val theCC toAction =
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
        opacity = if active then "0.85" else "0.12"
      in
        svgElement "path"
          [ svgAttr "d" (donutSegmentPath cx cy outerR innerR startA endA 1.0)
          , svgAttr "fill" color
          , svgAttr "opacity" opacity
          , svgAttr "style" "cursor:pointer"
          , svgOnClick \_ -> toAction (SegmentClick theCC (segMidiVal i))
          ] []
  in
    svgElement "g" [] [ renderSeg 0, renderSeg 1, renderSeg 2 ]

-- | Render a nested donut for one knob position (dual or single layer)
renderDonut :: forall w i. PedalLayout -> KnobDef -> PedalState -> (DonutEvent -> i) -> HH.HTML w i
renderDonut layout knob ps toAction =
  let
    cfg = defaultConfig
    cx = 0.0
    cy = 0.0
    color = groupColor layout.groups knob.group
    colorMuted = groupMutedColor layout.groups knob.group
    primaryVal = lookupCC knob.primaryCC ps
    primaryLabel = resolveLabel ps knob.primaryLabel
    x = colXFor layout.viewBox.width layout.columns knob.col
    y = rowYFor knob.row
  in case knob.hiddenCC of
    -- Dual layer (nested arcs)
    Just hCC ->
      let
        hiddenVal = lookupCC hCC ps
        hiddenLabel = case knob.hiddenLabel of
          Just hl -> resolveLabel ps hl
          Nothing -> ""
        hiddenLayer = fromMaybe (ContinuousKnob { center: Nothing }) knob.hiddenLayer
        primarySeg = isSegmented knob.primaryLayer
        hiddenSeg = isSegmented hiddenLayer
        outerEvt = if primarySeg
          then segmentClickEvents knob.primaryCC toAction
          else dragStartEvents knob.primaryCC primaryVal toAction
        innerEvt = if hiddenSeg
          then segmentClickEvents hCC toAction
          else dragStartEvents hCC hiddenVal toAction
        outerCursor = if primarySeg then "pointer" else "ns-resize"
        innerCursor = if hiddenSeg then "pointer" else "ns-resize"
        hasCenterDetente = isJust (layerCenter knob.primaryLayer) || isJust (layerCenter hiddenLayer)
      in
        svgElement "g"
          [ svgAttr "transform" ("translate(" <> show x <> "," <> show y <> ")") ]
          -- Background tracks
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
          , if hasCenterDetente
              then svgElement "line"
                [ svgAttr "x1" (show cx), svgAttr "y1" (show (cy - cfg.innerRadius))
                , svgAttr "x2" (show cx), svgAttr "y2" (show (cy - cfg.outerRadius))
                , svgAttr "stroke" "#ccc"
                , svgAttr "stroke-width" "1"
                ] []
              else svgElement "g" [] []
          -- Outer active arc (primary value)
          , if primarySeg
              then renderSegmentedArc cx cy cfg.gapRadius cfg.outerRadius color primaryVal knob.primaryCC toAction
              else renderContinuousArc cx cy cfg.gapRadius cfg.outerRadius color "0.85" primaryVal (layerCenter knob.primaryLayer)
          -- Inner active arc (hidden value)
          , if hiddenSeg
              then renderSegmentedArc cx cy cfg.innerRadius cfg.gapRadius "#888" hiddenVal hCC toAction
              else renderContinuousArc cx cy cfg.innerRadius cfg.gapRadius color "0.45" hiddenVal (layerCenter hiddenLayer)
          -- Center: dual values (color-coded)
          , svgElement "text"
              [ svgAttr "x" (show cx), svgAttr "y" (show (cy + 4.0))
              , svgAttr "text-anchor" "middle"
              , svgAttr "font-size" "8"
              , svgAttr "font-weight" "600"
              ]
              [ svgElement "tspan" [ svgAttr "fill" color ] [ HH.text (show primaryVal) ]
              , svgElement "tspan" [ svgAttr "fill" "#999" ] [ HH.text " / " ]
              , svgElement "tspan" [ svgAttr "fill" colorMuted ] [ HH.text (show hiddenVal) ]
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

    -- Single layer (full-width arc, no hidden knob)
    Nothing ->
      let
        primarySeg = isSegmented knob.primaryLayer
        outerEvt = if primarySeg
          then segmentClickEvents knob.primaryCC toAction
          else dragStartEvents knob.primaryCC primaryVal toAction
        outerCursor = if primarySeg then "pointer" else "ns-resize"
      in
        svgElement "g"
          [ svgAttr "transform" ("translate(" <> show x <> "," <> show y <> ")") ]
          [ svgElement "path"
              ([ svgAttr "d" (donutSegmentPath cx cy cfg.outerRadius cfg.innerRadius minAngle maxAngle 1.0)
              , svgAttr "fill" "#e8e8e8"
              , svgAttr "style" ("cursor:" <> outerCursor)
              ] <> outerEvt) []
          , if isJust (layerCenter knob.primaryLayer)
              then svgElement "line"
                [ svgAttr "x1" (show cx), svgAttr "y1" (show (cy - cfg.innerRadius))
                , svgAttr "x2" (show cx), svgAttr "y2" (show (cy - cfg.outerRadius))
                , svgAttr "stroke" "#ccc"
                , svgAttr "stroke-width" "1"
                ] []
              else svgElement "g" [] []
          , if primarySeg
              then renderSegmentedArc cx cy cfg.innerRadius cfg.outerRadius color primaryVal knob.primaryCC toAction
              else renderContinuousArc cx cy cfg.innerRadius cfg.outerRadius color "0.85" primaryVal (layerCenter knob.primaryLayer)
          , svgElement "text"
              [ svgAttr "x" (show cx), svgAttr "y" (show (cy + 4.0))
              , svgAttr "text-anchor" "middle"
              , svgAttr "font-size" "10"
              , svgAttr "font-weight" "600"
              , svgAttr "fill" color
              ] [ HH.text (show primaryVal) ]
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
          ]

-- | Render a footswitch indicator with LED-driven fill color
renderFootswitch :: forall w i. PedalLayout -> FootswitchDef -> PedalState -> (DonutEvent -> i) -> HH.HTML w i
renderFootswitch layout fs ps toAction =
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

-- | Render the config row (toggles + sliders)
renderConfigRow :: forall w i. Number -> PedalLayout -> PedalState -> (DonutEvent -> i) -> HH.HTML w i
renderConfigRow baseY layout ps toAction =
  let
    configs = layout.config
    n = Array.length configs
    spacing = if n <= 1 then 0.0 else (layout.viewBox.width - 100.0) / toNumber (n - 1)
    startX = 50.0
    renderOne idx cfg =
      let
        x = startX + toNumber idx * spacing
        val = lookupCC cfg.cc ps
      in case cfg.controlType of
        CfgToggle ->
          let isOn = val > 63
          in svgElement "g"
            [ svgAttr "style" "cursor:pointer"
            , svgOnClick \_ -> toAction (ToggleClick cfg.cc val)
            ]
            [ svgElement "circle"
                [ svgAttr "cx" (show x), svgAttr "cy" (show baseY)
                , svgAttr "r" "4"
                , svgAttr "fill" (if isOn then "#555" else "#ddd")
                ] []
            , svgElement "text"
                [ svgAttr "x" (show x), svgAttr "y" (show (baseY + 15.0))
                , svgAttr "text-anchor" "middle"
                , svgAttr "font-size" "6"
                , svgAttr "fill" "#999"
                , svgAttr "pointer-events" "none"
                ] [ HH.text cfg.label ]
            ]
        CfgSlider ->
          let
            barW = 40.0
            barH = 6.0
            fillW = barW * toNumber val / 127.0
            bx = x - barW / 2.0
            by = baseY - barH / 2.0
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
                [ svgAttr "x" (show x), svgAttr "y" (show (baseY + 15.0))
                , svgAttr "text-anchor" "middle"
                , svgAttr "font-size" "6"
                , svgAttr "fill" "#999"
                , svgAttr "pointer-events" "none"
                ] [ HH.text cfg.label ]
            ]
  in
    svgElement "g" [] (Array.mapWithIndex renderOne configs)

-- | Render DIP switch grid: banks with rotated labels
renderDipGrid :: forall w i. Number -> PedalLayout -> PedalState -> (DonutEvent -> i) -> HH.HTML w i
renderDipGrid baseY layout ps toAction =
  let
    banks = layout.dipBanks
    numBanks = Array.length banks
    dotY = baseY + 12.0
    spacing = 17.0
    renderBank bankIdx bank =
      let
        bankCenterX = layout.viewBox.width * (2.0 * toNumber bankIdx + 1.0) / (2.0 * toNumber numBanks)
        numSwitches = Array.length bank.switches
        bankStartX = bankCenterX - toNumber (numSwitches - 1) * spacing / 2.0
        header = svgElement "text"
          [ svgAttr "x" (show bankCenterX), svgAttr "y" (show baseY)
          , svgAttr "text-anchor" "middle"
          , svgAttr "font-size" "7"
          , svgAttr "font-weight" "600"
          , svgAttr "fill" "#999"
          ] [ HH.text bank.label ]
        renderSwitch sw =
          let
            bx = bankStartX + toNumber sw.index * spacing
            val = lookupCC sw.cc ps
            isOn = val > 63
            labelX = bx + 2.0
            labelBaseY = dotY + 8.0
          in svgElement "g"
            [ svgAttr "style" "cursor:pointer"
            , svgOnClick \_ -> toAction (ToggleClick sw.cc val)
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
                ] [ HH.text sw.label ]
            ]
      in [ header ] <> map renderSwitch bank.switches
  in
    svgElement "g" [] (Array.concat (Array.mapWithIndex renderBank banks))

-- | Thin horizontal separator line
renderSectionLine :: forall w i. Number -> HH.HTML w i
renderSectionLine y =
  svgElement "line"
    [ svgAttr "x1" "40", svgAttr "y1" (show y)
    , svgAttr "x2" "280", svgAttr "y2" (show y)
    , svgAttr "stroke" "#e0e0e0"
    , svgAttr "stroke-width" "0.5"
    ] []
