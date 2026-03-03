module Component.Pedal.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Component.Pedal.Donut as Donut
import Component.Pedal.MoodLayout as Layout
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Data.Const (Const)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, unMidiValue)
import Data.Pedal (PedalId)
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, PedalState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input =
  { engine :: EngineState
  , pedalId :: PedalId
  , registry :: PedalRegistry
  }

data Output
  = BackToGrid

type State = { input :: Input }

data Action
  = Receive Input
  | ClickBack

type Slot = H.Slot (Const Void) Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i -> { input: i }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    pid = state.input.pedalId
    mDef = CRegistry.findPedal state.input.registry pid
    mPs = Map.lookup pid state.input.engine
  in
    HH.div [ HP.class_ (H.ClassName "pedal-view-container") ]
      [ case mDef, mPs of
          Just def, Just ps ->
            HH.div [ HP.class_ (H.ClassName "pedal-view") ]
              [ renderHeader def.meta.name def.meta.brand ps
              , renderSvg ps
              , renderBackButton
              ]
          _, _ ->
            HH.div [ HP.class_ (H.ClassName "pedal-view") ]
              [ HH.text "Pedal not found"
              , renderBackButton
              ]
      ]

renderHeader :: forall w i. String -> String -> PedalState -> HH.HTML w i
renderHeader name brand ps =
  HH.div [ HP.class_ (H.ClassName "pedal-view-header") ]
    [ HH.h2_ [ HH.text name ]
    , HH.span [ HP.class_ (H.ClassName "brand") ] [ HH.text brand ]
    , HH.span [ HP.class_ (H.ClassName "channel") ] [ HH.text ("Ch " <> show ps.channel) ]
    ]

renderBackButton :: forall m. H.ComponentHTML Action () m
renderBackButton =
  HH.button
    [ HP.class_ (H.ClassName "pedal-view-back")
    , HE.onClick \_ -> ClickBack
    ]
    [ HH.text "Back to Grid" ]

svgElement :: forall w i. String -> Array (HH.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
svgElement name = HH.elementNS (HH.Namespace "http://www.w3.org/2000/svg") (HH.ElemName name)

svgAttr :: forall r i. String -> String -> HH.IProp r i
svgAttr name val = HP.attr (HH.AttrName name) val

renderSvg :: forall w i. PedalState -> HH.HTML w i
renderSvg ps =
  svgElement "svg"
    [ svgAttr "viewBox" "0 0 320 370"
    , svgAttr "class" "pedal-svg"
    ]
    (  [ renderColumnHeaders ]
    <> map (renderKnobDonut ps) Layout.moodKnobs
    <> map (renderFsDonut ps) Layout.moodFootswitches
    )

renderColumnHeaders :: forall w i. HH.HTML w i
renderColumnHeaders =
  svgElement "g" []
    [ colHead 60.0 "Wet"
    , colHead 160.0 "Shared"
    , colHead 260.0 "ML"
    ]
  where
  colHead x label =
    svgElement "text"
      [ svgAttr "x" (show x), svgAttr "y" "12"
      , svgAttr "text-anchor" "middle"
      , svgAttr "font-size" "10"
      , svgAttr "font-weight" "700"
      , svgAttr "fill" "#999"
      , svgAttr "letter-spacing" "0.05em"
      ] [ HH.text label ]

renderKnobDonut :: forall w i. PedalState -> Layout.KnobPair -> HH.HTML w i
renderKnobDonut ps knob =
  let
    primaryVal = lookupCC knob.primaryCC ps
    hiddenVal = lookupCC knob.hiddenCC ps
  in
    Donut.renderDonut knob { primaryVal, hiddenVal }

renderFsDonut :: forall w i. PedalState -> Layout.Footswitch -> HH.HTML w i
renderFsDonut ps fs =
  Donut.renderFootswitch fs (lookupCC fs.cc ps)

lookupCC :: CC -> PedalState -> Int
lookupCC ccNum ps = fromMaybe 0 (map unMidiValue (Map.lookup ccNum ps.values))

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  ClickBack -> H.raise BackToGrid
