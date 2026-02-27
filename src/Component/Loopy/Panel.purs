module Component.Loopy.Panel
  ( component
  , Output(..)
  , Input
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Const (Const)
import Data.Loopy as Loopy
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff.Class (class MonadAff)
import Engine (MidiConnections)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot = H.Slot (Const Void) Output

type Input =
  { connections :: MidiConnections
  , loopyTwisterActive :: Boolean
  , selectedLoop :: Int
  , loopStates :: Array Loopy.LoopState
  , heldEncoder :: Maybe Int
  }

data Output
  = LoopSelected Int
  | ActionFired Loopy.LoopyAction
  | TwisterModeToggled
  | GenerateProject

type State =
  { input :: Input
  }

data Action
  = Receive Input
  | ClickLoop Int
  | ClickAction Loopy.LoopyAction
  | ClickTitle
  | ClickGenerate

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
  HH.div [ HP.class_ (H.ClassName "loopy-panel") ]
    [ HH.div
        [ HP.class_ (H.ClassName titleClass)
        , HE.onClick \_ -> ClickTitle
        ]
        [ HH.span_ [ HH.text titleText ] ]
    , HH.div [ HP.class_ (H.ClassName "loopy-grid") ]
        (map renderGroup Loopy.groups)
    , HH.div [ HP.class_ (H.ClassName "loopy-actions") ]
        (map renderAction Loopy.actions)
    , HH.div [ HP.class_ (H.ClassName "loopy-generate") ]
        [ HH.button
            [ HP.class_ (H.ClassName "loopy-action-btn loopy-generate-btn")
            , HE.onClick \_ -> ClickGenerate
            ]
            [ HH.text "Generate .lpproj" ]
        ]
    ]
  where
  isShifted = isJust state.input.heldEncoder
  titleClass = "loopy-header"
    <> (if state.input.loopyTwisterActive then " twister-active" else "")
    <> (if isShifted then " shift-active" else "")
  titleText
    | isShifted = "LoopyPro SHIFT"
    | state.input.loopyTwisterActive = "LoopyPro \x25C9"
    | otherwise = "LoopyPro"

  renderGroup group =
    let (Loopy.LoopIndex aIdx) = group.loopA
        (Loopy.LoopIndex bIdx) = group.loopB
        aState = Array.index state.input.loopStates aIdx
        bState = Array.index state.input.loopStates bIdx
    in HH.div [ HP.class_ (H.ClassName "loopy-group") ]
      [ loopButton group.loopA group.color "A"
      , loopButton group.loopB group.color "B"
      , HH.span [ HP.class_ (H.ClassName "loopy-group-label") ]
          [ HH.text group.color.label ]
      , HH.div [ HP.class_ (H.ClassName "loopy-info-pair") ]
          [ renderLoopInfo aState
          , renderLoopInfo bState
          ]
      ]

  renderLoopInfo Nothing = HH.span_ []
  renderLoopInfo (Just ls) =
    HH.span [ HP.class_ (H.ClassName "loopy-info") ]
      ( [ HH.text (show ls.volume) ]
        <> (if ls.speed /= 64 then [ HH.text (" s" <> show ls.speed) ] else [])
        <> (if ls.muted then [ HH.span [ HP.class_ (H.ClassName "loopy-flag loopy-flag-muted") ] [ HH.text "M" ] ] else [])
        <> (if ls.soloed then [ HH.span [ HP.class_ (H.ClassName "loopy-flag loopy-flag-soloed") ] [ HH.text "S" ] ] else [])
        <> (if ls.cleared then [ HH.span [ HP.class_ (H.ClassName "loopy-flag loopy-flag-cleared") ] [ HH.text "CLR" ] ] else [])
      )

  loopButton (Loopy.LoopIndex idx) color label =
    let isSelected = state.input.selectedLoop == idx
        style = "border-color: " <> color.color
                <> if isSelected
                     then "; background: " <> color.color <> "; color: #fff"
                     else "; color: " <> color.color
    in HH.button
      [ HP.class_ (H.ClassName (if isSelected then "loopy-loop selected" else "loopy-loop"))
      , HP.attr (HH.AttrName "style") style
      , HE.onClick \_ -> ClickLoop idx
      ]
      [ HH.text label ]

  renderAction def =
    HH.button
      [ HP.class_ (H.ClassName "loopy-action-btn")
      , HE.onClick \_ -> ClickAction def.action
      ]
      [ HH.text def.label ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  ClickLoop idx -> H.raise (LoopSelected idx)
  ClickAction action -> H.raise (ActionFired action)
  ClickTitle -> H.raise TwisterModeToggled
  ClickGenerate -> H.raise GenerateProject
