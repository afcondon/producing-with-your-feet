module Component.Loopy.ClipSettings
  ( component
  , Input
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Const (Const)
import Data.Int (round, toNumber, fromString)
import Data.Loopy (ClipSettings, CountInMode(..), CountOutMode(..), RecordEndAction(..), BeatQuantPreset(..))
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Foreign.ClipDiagram as Diagram
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement as HTMLElement

type Slot = H.Slot (Const Void) Output

type Input =
  { settings :: ClipSettings
  , loopColor :: String
  }

data Output = ClipSettingChanged ClipSettings

type State =
  { settings :: ClipSettings
  , loopColor :: String
  }

data Action
  = Initialize
  | Receive Input
  | SetCountIn CountInMode
  | SetCountOut CountOutMode
  | ToggleAutoCountOut
  | SetRecordEnd RecordEndAction
  | SetOverdubFeedback Number
  | SetBeatQuant BeatQuantPreset
  | TogglePhaseLock
  | ToggleLoopMode
  | ToggleThreshold
  | ToggleIntro
  | ToggleTail
  | ToggleRetrospective

diagramRef :: H.RefLabel
diagramRef = H.RefLabel "clip-diagram"

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \i -> { settings: i.settings, loopColor: i.loopColor }
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        , initialize = Just Initialize
        }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "clip-settings") ]
    [ HH.div [ HP.class_ (H.ClassName "clip-diagram"), HP.ref diagramRef ] []
    , renderControls state.settings
    ]

renderControls :: forall m. ClipSettings -> H.ComponentHTML Action () m
renderControls s =
  HH.div [ HP.class_ (H.ClassName "clip-controls") ]
    [ -- Row 1: Recording Behavior
      HH.div [ HP.class_ (H.ClassName "clip-control-row") ]
        [ controlGroup "Count-In"
            [ selectControl s.countIn
                [ { val: CountInNone, label: "None" }
                , { val: CountInMaster, label: "Master" }
                , { val: CountInLoop, label: "Loop" }
                ]
                SetCountIn
            ]
        , controlGroup "Count-Out"
            [ selectControl s.countOut
                [ { val: CountOutNone, label: "None" }
                , { val: CountOutMaster, label: "Master" }
                , { val: CountOutLoop, label: "Loop" }
                ]
                SetCountOut
            ]
        , toggleButton "Auto" s.autoCountOut ToggleAutoCountOut true
        , controlGroup "End"
            [ selectControl s.recordEndAction
                [ { val: EndPlay, label: "Play" }
                , { val: EndStop, label: "Stop" }
                , { val: EndOverdub, label: "Overdub" }
                ]
                SetRecordEnd
            ]
        ]
    , -- Row 2: Features & Sound
      HH.div [ HP.class_ (H.ClassName "clip-control-row") ]
        [ renderOverdubSlider s.overdubFeedback
        , controlGroup "Quant"
            [ selectControl s.beatQuant
                [ { val: BeatQuantOff, label: "Off" }
                , { val: BeatQuant16Tight, label: "16th Tight" }
                , { val: BeatQuant16Med, label: "16th Med" }
                , { val: BeatQuant16Loose, label: "16th Loose" }
                , { val: BeatQuant32Tight, label: "32nd Tight" }
                , { val: BeatQuant32Med, label: "32nd Med" }
                , { val: BeatQuant32Loose, label: "32nd Loose" }
                ]
                SetBeatQuant
            ]
        , toggleButton "Phase" s.phaseLocked TogglePhaseLock true
        , toggleButton (if s.loop then "Loop" else "1-Shot") s.loop ToggleLoopMode true
        ]
    , -- Row 3: Special Modes
      HH.div [ HP.class_ (H.ClassName "clip-control-row") ]
        [ toggleButton "Thresh" s.threshold ToggleThreshold true
        , toggleButton "Intro" s.intro ToggleIntro (s.countIn /= CountInNone)
        , toggleButton "Tail" s.tail ToggleTail true
        , toggleButton "Retro" s.retrospective ToggleRetrospective true
        ]
    ]

controlGroup :: forall a s m. String -> Array (H.ComponentHTML a s m) -> H.ComponentHTML a s m
controlGroup label children =
  HH.div [ HP.class_ (H.ClassName "clip-control-group") ]
    ( [ HH.span [ HP.class_ (H.ClassName "clip-label") ] [ HH.text label ] ]
      <> children
    )

-- | Generic select control using index-based dispatch
selectControl :: forall a s m. Eq a => a -> Array { val :: a, label :: String } -> (a -> Action) -> H.ComponentHTML Action s m
selectControl current options toAction =
  HH.select
    [ HP.class_ (H.ClassName "clip-select")
    , HE.onValueChange \str -> dispatchSelect str options toAction
    ]
    (map renderOpt options)
  where
  renderOpt opt =
    HH.option
      [ HP.value opt.label
      , HP.selected (current == opt.val)
      ]
      [ HH.text opt.label ]

  dispatchSelect :: String -> Array { val :: a, label :: String } -> (a -> Action) -> Action
  dispatchSelect str opts toAct =
    case Array.find (\opt -> opt.label == str) opts of
      Just opt -> toAct opt.val
      Nothing -> toAct current

toggleButton :: forall s m. String -> Boolean -> Action -> Boolean -> H.ComponentHTML Action s m
toggleButton label active action enabled =
  HH.button
    [ HP.class_ (H.ClassName ("clip-toggle" <> if active then " active" else ""))
    , HP.disabled (not enabled)
    , HE.onClick \_ -> action
    ]
    [ HH.text label ]

renderOverdubSlider :: forall s m. Number -> H.ComponentHTML Action s m
renderOverdubSlider feedback =
  let pct = round (feedback * 100.0)
      label
        | pct <= 0  = "Replace"
        | pct == 100 = "Standard"
        | otherwise  = show pct <> "%"
  in HH.div [ HP.class_ (H.ClassName "clip-slider-group") ]
    [ HH.span [ HP.class_ (H.ClassName "clip-label") ] [ HH.text "Overdub" ]
    , HH.input
        [ HP.class_ (H.ClassName "clip-slider")
        , HP.type_ HP.InputRange
        , HP.attr (HH.AttrName "min") "0"
        , HP.attr (HH.AttrName "max") "200"
        , HP.attr (HH.AttrName "step") "5"
        , HP.value (show pct)
        , HE.onValueInput \str ->
            SetOverdubFeedback (toNumber (parseIntOrZero str) / 100.0)
        ]
    , HH.span [ HP.class_ (H.ClassName "clip-slider-value") ] [ HH.text label ]
    ]

parseIntOrZero :: String -> Int
parseIntOrZero str = case fromString str of
  Just n -> n
  Nothing -> 0

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> refreshDiagram
  Receive input -> do
    H.modify_ _ { settings = input.settings, loopColor = input.loopColor }
    refreshDiagram
  SetCountIn mode -> updateAndEmit _ { countIn = mode }
  SetCountOut mode -> updateAndEmit _ { countOut = mode }
  ToggleAutoCountOut -> do
    s <- H.gets _.settings
    updateAndEmit _ { autoCountOut = not s.autoCountOut }
  SetRecordEnd action -> updateAndEmit _ { recordEndAction = action }
  SetOverdubFeedback val -> updateAndEmit _ { overdubFeedback = val }
  SetBeatQuant preset -> updateAndEmit _ { beatQuant = preset }
  TogglePhaseLock -> do
    s <- H.gets _.settings
    updateAndEmit _ { phaseLocked = not s.phaseLocked }
  ToggleLoopMode -> do
    s <- H.gets _.settings
    updateAndEmit _ { loop = not s.loop }
  ToggleThreshold -> do
    s <- H.gets _.settings
    updateAndEmit _ { threshold = not s.threshold }
  ToggleIntro -> do
    s <- H.gets _.settings
    updateAndEmit _ { intro = not s.intro }
  ToggleTail -> do
    s <- H.gets _.settings
    updateAndEmit _ { tail = not s.tail }
  ToggleRetrospective -> do
    s <- H.gets _.settings
    updateAndEmit _ { retrospective = not s.retrospective }

updateAndEmit :: forall m. MonadAff m => (ClipSettings -> ClipSettings) -> H.HalogenM State Action () Output m Unit
updateAndEmit f = do
  st <- H.get
  let newSettings = f st.settings
  H.modify_ _ { settings = newSettings }
  H.raise (ClipSettingChanged newSettings)
  refreshDiagram

refreshDiagram :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
refreshDiagram = do
  st <- H.get
  mEl <- H.getHTMLElementRef diagramRef
  case mEl of
    Nothing -> pure unit
    Just htmlEl ->
      liftEffect $ Diagram.renderClipDiagram st.settings st.loopColor (HTMLElement.toElement htmlEl)
