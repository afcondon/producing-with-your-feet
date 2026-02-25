module Component.Detail.Control
  ( renderControl
  , ControlOutput(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Int as Int
import Halogen.HTML.Events as HE
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, unMidiValue, unsafeMidiValue)
import Data.Pedal (Annotation, Control(..), LabelSource(..), PedalId)
import Data.Pedal.Modes (DualChannelModes, ChannelDef, ModeChannel(..), ModeRole(..))
import Data.Tuple (Tuple(..))
import Engine (PedalState)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen as H

data ControlOutput
  = SetCC PedalId CC MidiValue
  | SetMultipleCC PedalId (Array (Tuple CC MidiValue))
  | FireMomentary PedalId CC MidiValue
  | SetInfo PedalId String Int

-- | Render a control widget based on its type
renderControl
  :: forall w
   . PedalId
  -> PedalState
  -> Maybe DualChannelModes
  -> Control
  -> HH.HTML w ControlOutput
renderControl pid ps mModes = case _ of
  Slider r ->
    HH.div [ HP.class_ (H.ClassName "control slider-control") ]
      [ HH.div [ HP.class_ (H.ClassName "slider-header") ]
          [ HH.label [ HP.class_ (H.ClassName "control-label") ]
              [ HH.text (resolveLabel ps mModes r.label) ]
          , HH.span [ HP.class_ (H.ClassName "slider-value") ]
              [ HH.text (show currentVal) ]
          ]
      , HH.input
          [ HP.type_ HP.InputRange
          , HP.class_ (H.ClassName "slider-input")
          , HP.value (show currentVal)
          , HP.attr (HH.AttrName "min") "0"
          , HP.attr (HH.AttrName "max") "127"
          , HP.attr (HH.AttrName "style") ("--fill: " <> pctStr currentVal <> ";")
          , HE.onValueInput \s -> SetCC pid r.cc (unsafeMidiValue (fromMaybe 64 (Int.fromString s)))
          ]
      , renderSliderAnnotations r.annotations
      , controlDesc r.description
      ]
    where
    currentVal = fromMaybe 64 (map unMidiValue (Map.lookup r.cc ps.values))

  Toggle r ->
    HH.div [ HP.class_ (H.ClassName "control toggle-control") ]
      ( [ HH.button
            ( [ HP.class_ (H.ClassName (if isOn then "toggle-btn on" else "toggle-btn"))
              , HE.onClick \_ -> SetCC pid r.cc (if isOn then unsafeMidiValue 0 else unsafeMidiValue 127)
              ]
              <> case r.description of
                   Just d -> [ HP.attr (HH.AttrName "title") d ]
                   Nothing -> []
            )
            [ HH.span [ HP.class_ (H.ClassName "toggle-label") ] [ HH.text label ]
            , HH.span [ HP.class_ (H.ClassName "toggle-state") ]
                [ HH.text (if isOn then r.onLabel else r.offLabel) ]
            ]
        ]
        <> case r.description of
             Just d -> [ HH.div [ HP.class_ (H.ClassName "control-desc") ] [ HH.text d ] ]
             Nothing -> []
      )
    where
    isOn = fromMaybe 0 (map unMidiValue (Map.lookup r.cc ps.values)) > 0
    label = case r.labelSource of
      Just ls -> resolveLabel ps mModes ls
      Nothing -> r.label

  Segmented r ->
    HH.div [ HP.class_ (H.ClassName "control segmented-control") ]
      ( [ HH.label [ HP.class_ (H.ClassName "control-label") ] [ HH.text r.label ]
        , HH.div [ HP.class_ (H.ClassName "segment-group") ]
            (map segmentButton r.options)
        ]
        <> case activeDesc of
             Just d -> [ HH.div [ HP.class_ (H.ClassName "control-desc") ] [ HH.text d ] ]
             Nothing -> []
      )
    where
    currentVal = fromMaybe (unsafeMidiValue 0) (Map.lookup r.cc ps.values)
    activeDesc = Array.find (\opt -> opt.value == currentVal) r.options >>= _.description

    segmentButton opt =
      HH.button
        ( [ HP.class_ (H.ClassName (if opt.value == currentVal then "segment-btn active" else "segment-btn"))
          , HE.onClick \_ -> SetCC pid r.cc opt.value
          ]
          <> case opt.description of
               Just d -> [ HP.attr (HH.AttrName "title") d ]
               Nothing -> []
        )
        [ HH.text opt.label ]

  Dropdown r ->
    HH.div [ HP.class_ (H.ClassName "control dropdown-control") ]
      [ HH.label [ HP.class_ (H.ClassName "control-label") ] [ HH.text r.label ]
      , HH.select
          [ HP.class_ (H.ClassName "dropdown-select")
          , HE.onValueChange \s -> SetCC pid r.cc (unsafeMidiValue (fromMaybe 0 (Int.fromString s)))
          ]
          (map dropdownOption r.options)
      , controlDesc r.description
      ]
    where
    currentVal = fromMaybe (unsafeMidiValue 0) (Map.lookup r.cc ps.values)

    dropdownOption opt =
      HH.option
        [ HP.value (show (unMidiValue opt.value))
        , HP.selected (opt.value == currentVal)
        ]
        [ HH.text opt.label ]

  Momentary r ->
    HH.div [ HP.class_ (H.ClassName "control momentary-control") ]
      [ HH.button
          [ HP.class_ (H.ClassName "momentary-btn")
          , HE.onClick \_ -> FireMomentary pid r.cc r.value
          ]
          [ HH.text r.label ]
      , controlDesc r.description
      ]

  PianoKey r ->
    HH.div [ HP.class_ (H.ClassName "control piano-key-control") ]
      [ HH.label_ [ HH.text r.label ]
      , HH.div [ HP.class_ (H.ClassName "piano-keys") ]
          ( map pianoKeyButton r.options
            <> [ HH.button
                  [ HP.class_ (H.ClassName (if currentVal == r.chromaticValue then "active chromatic" else "chromatic"))
                  , HE.onClick \_ -> SetCC pid r.cc r.chromaticValue
                  ]
                  [ HH.text "\x266F" ]
               ]
          )
      ]
    where
    currentVal = fromMaybe (unsafeMidiValue 0) (Map.lookup r.cc ps.values)

    pianoKeyButton opt =
      HH.button
        [ HP.class_ (H.ClassName (if opt.value == currentVal then "active" else ""))
        , HE.onClick \_ -> SetCC pid r.cc opt.value
        ]
        [ HH.text opt.label ]

  RadioGroup r ->
    HH.div [ HP.class_ (H.ClassName "control radio-group-control") ]
      [ HH.label_ [ HH.text r.label ]
      , HH.div [ HP.class_ (H.ClassName "radio-group") ]
          (map radioOption r.mapping)
      ]
    where
    radioOption entry =
      let isSelected = Array.all
            (\(Tuple cc' val) -> Map.lookup cc' ps.values == Just val)
            (Map.toUnfoldable entry.values :: Array _)
      in HH.button
        [ HP.class_ (H.ClassName (if isSelected then "radio-btn active" else "radio-btn"))
        , HE.onClick \_ -> SetMultipleCC pid (Map.toUnfoldable entry.values)
        ]
        [ HH.text entry.label ]

  ModeRadio r -> case mModes of
    Nothing -> HH.text ""
    Just modes ->
      let
        chDef = channelDef r.modeChannel modes
        currentMode = fromMaybe 0 (map unMidiValue (Map.lookup chDef.modeCC ps.values))
      in
      HH.div [ HP.class_ (H.ClassName "control") ]
        [ HH.label [ HP.class_ (H.ClassName "control-label") ] [ HH.text r.label ]
        , HH.div [ HP.class_ (H.ClassName "radio-group") ]
            (Array.mapWithIndex (\i eff ->
              HH.button
                [ HP.class_ (H.ClassName (if i == currentMode then "radio-btn active" else "radio-btn"))
                , HE.onClick \_ -> SetCC pid chDef.modeCC (unsafeMidiValue i)
                ]
                [ HH.text eff.label ]
            ) modes.effects)
        ]

  InfoToggle r ->
    HH.div [ HP.class_ (H.ClassName "control info-toggle-control") ]
      ( [ HH.button
            ( [ HP.class_ (H.ClassName (if isOn then "toggle-btn on" else "toggle-btn"))
              , HE.onClick \_ -> SetInfo pid r.key (if isOn then 0 else 1)
              ]
              <> case r.description of
                   Just d -> [ HP.attr (HH.AttrName "title") d ]
                   Nothing -> []
            )
            [ HH.span [ HP.class_ (H.ClassName "toggle-label") ] [ HH.text r.label ]
            , HH.span [ HP.class_ (H.ClassName "toggle-state") ]
                [ HH.text (if isOn then "On" else "Off") ]
            ]
        ]
        <> case r.description of
             Just d -> [ HH.div [ HP.class_ (H.ClassName "control-desc") ] [ HH.text d ] ]
             Nothing -> []
      )
    where
    isOn = fromMaybe 0 (Map.lookup r.key ps.info) > 0

-- Helpers

channelDef :: ModeChannel -> DualChannelModes -> ChannelDef
channelDef LeftChannel modes = modes.left
channelDef RightChannel modes = modes.right

resolveLabel :: PedalState -> Maybe DualChannelModes -> LabelSource -> String
resolveLabel ps mModes = case _ of
  Static s -> s
  ModeMap { cc: modeCC, labels } ->
    let currentMode = fromMaybe (unsafeMidiValue 0) (Map.lookup modeCC ps.values)
    in fromMaybe "?" (Map.lookup currentMode labels)
  ChannelMode { channel, role } -> case mModes of
    Nothing -> "?"
    Just modes ->
      let
        chDef = channelDef channel modes
        modeIdx = fromMaybe 0 (map unMidiValue (Map.lookup chDef.modeCC ps.values))
      in case Array.index modes.effects modeIdx of
        Nothing -> "?"
        Just eff
          | role == HoldRole -> eff.hold
          | otherwise ->
            let
              getRoleLabel v = case role of
                TimeRole -> v.time
                ModifyRole -> v.modify
                AltRole -> v.alt
                HoldRole -> "" -- unreachable, guarded above
              labelA = getRoleLabel eff.a
              labelB = getRoleLabel eff.b
            in if labelA == labelB then labelA else labelA <> " / " <> labelB

pctStr :: Int -> String
pctStr val = show (toNumber val * 100.0 / 127.0) <> "%"

renderSliderAnnotations :: forall w i. Array Annotation -> HH.HTML w i
renderSliderAnnotations anns =
  if Array.null anns
    then HH.text ""
    else HH.div [ HP.class_ (H.ClassName "slider-annotations") ]
      (map renderAnn anns)
  where
  renderAnn a = HH.span
    [ HP.class_ (H.ClassName "annotation")
    , HP.attr (HH.AttrName "style") ("left: " <> pctStr (unMidiValue a.position))
    ]
    [ HH.text a.label ]

controlDesc :: forall w i. Maybe String -> HH.HTML w i
controlDesc = case _ of
  Nothing -> HH.text ""
  Just d -> HH.div [ HP.class_ (H.ClassName "control-desc") ] [ HH.text d ]
