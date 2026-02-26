module Component.Boards.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Color (toHexString)
import Data.Array as Array
import Data.Const (Const)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, ProgramNumber, unProgramNumber)
import Data.Pedal (PedalDef, PedalId)
import Data.Pedal.Engage (EngageConfig(..), EngageState(..))
import Data.Preset (BoardPreset, BoardPresetEntry, PedalPreset, PresetId)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Engine (EngineState, MidiConnections)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pedals.Registry as Registry

type Input =
  { engine :: EngineState
  , connections :: MidiConnections
  , presets :: Array PedalPreset
  , boardPresets :: Array BoardPreset
  }

data Output
  = RecallBoard BoardPreset
  | SendEngageAudition PedalId EngageState
  | SendPCAudition PedalId ProgramNumber
  | ValueChanged PedalId CC MidiValue

type GridEntry =
  { engage :: EngageState
  , selectedPresetId :: Maybe PresetId
  }

type State =
  { input :: Input
  , grid :: Map.Map PedalId GridEntry
  , recalling :: Maybe PresetId
  }

data Action
  = Receive Input
  | ChangeEngage PedalId String
  | ChangePreset PedalId String
  | AllOn
  | AllOff
  | AllNoChange
  | ClickRecallBoard PresetId
  | ClickLoadBoard PresetId

type Slot = H.Slot (Const Void) Output

component :: forall q m. MonadAff m => H.Component q Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState i =
  { input: i
  , grid: Map.fromFoldable $ map (\def -> Tuple def.meta.id { engage: EngageNoChange, selectedPresetId: Nothing }) Registry.pedals
  , recalling: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "boards-view") ]
    [ renderBuilderGrid state
    , renderBoardList state
    ]

renderBuilderGrid :: forall m. State -> H.ComponentHTML Action () m
renderBuilderGrid state =
  HH.div_
    [ HH.div [ HP.class_ (H.ClassName "boards-header") ]
        [ HH.span [ HP.class_ (H.ClassName "boards-heading") ] [ HH.text "Boards" ]
        , HH.div [ HP.class_ (H.ClassName "boards-actions") ]
            [ HH.button [ HE.onClick \_ -> AllOn ] [ HH.text "All On" ]
            , HH.button [ HE.onClick \_ -> AllOff ] [ HH.text "All Off" ]
            , HH.button [ HE.onClick \_ -> AllNoChange ] [ HH.text "All \x2014\x2014" ]
            ]
        ]
    , HH.div [ HP.class_ (H.ClassName "boards-pedal-grid") ]
        (map (renderPedalRow state) Registry.pedals)
    ]

renderPedalRow :: forall m. State -> PedalDef -> H.ComponentHTML Action () m
renderPedalRow state def =
  HH.div [ HP.class_ (H.ClassName "boards-pedal-row") ]
    [ HH.span
        [ HP.class_ (H.ClassName "boards-pedal-name")
        , HP.attr (HH.AttrName "style") (case def.meta.color of
            Just c -> "color: " <> toHexString c
            Nothing -> "")
        ]
        [ HH.text def.meta.name ]
    , renderEngageSelect def.meta.id def.engage currentEntry.engage
    , renderPresetSelect def.meta.id currentEntry.selectedPresetId pedalPresets
    ]
  where
  currentEntry = fromMaybe { engage: EngageNoChange, selectedPresetId: Nothing }
    (Map.lookup def.meta.id state.grid)
  pedalPresets = Array.filter (\p -> p.pedalId == def.meta.id) state.input.presets

renderEngageSelect :: forall m. PedalId -> EngageConfig -> EngageState -> H.ComponentHTML Action () m
renderEngageSelect pid engageCfg currentState =
  HH.select
    [ HP.class_ (H.ClassName "boards-engage-select")
    , HP.value (engageToValue currentState)
    , HE.onValueChange (ChangeEngage pid)
    ]
    (engageOptions engageCfg)

engageOptions :: forall m. EngageConfig -> Array (H.ComponentHTML Action () m)
engageOptions = case _ of
  SingleEngage _ ->
    [ HH.option [ HP.value "no-change" ] [ HH.text "\x2014\x2014" ]
    , HH.option [ HP.value "on" ] [ HH.text "On" ]
    , HH.option [ HP.value "off" ] [ HH.text "Off" ]
    ]
  DualEngage { a, b } ->
    [ HH.option [ HP.value "no-change" ] [ HH.text "\x2014\x2014" ]
    , HH.option [ HP.value "on" ] [ HH.text "Both" ]
    , HH.option [ HP.value "a" ] [ HH.text a.label ]
    , HH.option [ HP.value "b" ] [ HH.text b.label ]
    , HH.option [ HP.value "off" ] [ HH.text "Off" ]
    ]

engageToValue :: EngageState -> String
engageToValue = case _ of
  EngageOn -> "on"
  EngageOff -> "off"
  EngageA -> "a"
  EngageB -> "b"
  EngageNoChange -> "no-change"

valueToEngage :: String -> EngageState
valueToEngage = case _ of
  "on" -> EngageOn
  "off" -> EngageOff
  "a" -> EngageA
  "b" -> EngageB
  _ -> EngageNoChange

renderPresetSelect :: forall m. PedalId -> Maybe PresetId -> Array PedalPreset -> H.ComponentHTML Action () m
renderPresetSelect pid selectedId presets =
  HH.select
    [ HP.class_ (H.ClassName "boards-preset-select")
    , HP.value (fromMaybe "" selectedId)
    , HE.onValueChange (ChangePreset pid)
    ]
    ( [ HH.option [ HP.value "" ] [ HH.text "No change" ] ]
      <> map presetOption presets
    )
  where
  presetOption p =
    let slotLabel = case p.savedSlot of
          Just slot -> " [" <> show (unProgramNumber slot) <> "]"
          Nothing -> " [LIB]"
    in HH.option [ HP.value p.id ] [ HH.text (p.name <> slotLabel) ]

renderBoardList :: forall m. State -> H.ComponentHTML Action () m
renderBoardList state =
  HH.div [ HP.class_ (H.ClassName "boards-list") ]
    [ HH.div [ HP.class_ (H.ClassName "boards-header") ]
        [ HH.span [ HP.class_ (H.ClassName "boards-heading") ] [ HH.text "Saved Boards" ] ]
    , if Array.null state.input.boardPresets
        then HH.div [ HP.class_ (H.ClassName "boards-empty") ] [ HH.text "No boards saved" ]
        else HH.div_ (map (renderBoardItem state) state.input.boardPresets)
    ]

renderBoardItem :: forall m. State -> BoardPreset -> H.ComponentHTML Action () m
renderBoardItem state bp =
  HH.div [ HP.class_ (H.ClassName "boards-item") ]
    [ HH.div [ HP.class_ (H.ClassName "boards-item-header") ]
        [ HH.span [ HP.class_ (H.ClassName "boards-item-name") ] [ HH.text bp.name ]
        , HH.span [ HP.class_ (H.ClassName "boards-item-date") ] [ HH.text (SCU.take 10 bp.modified) ]
        ]
    , if bp.notes /= ""
        then HH.div [ HP.class_ (H.ClassName "preset-description") ] [ HH.text bp.notes ]
        else HH.text ""
    , HH.div [ HP.class_ (H.ClassName "preset-description") ]
        [ HH.text (boardSummary bp state.input.presets) ]
    , HH.div [ HP.class_ (H.ClassName "boards-item-actions") ]
        [ HH.button
            [ HP.class_ (H.ClassName "recall-btn")
            , HE.onClick \_ -> ClickRecallBoard bp.id
            ]
            [ HH.text (if state.recalling == Just bp.id then "Sending..." else "Recall") ]
        , HH.button
            [ HE.onClick \_ -> ClickLoadBoard bp.id ]
            [ HH.text "Load" ]
        ]
    ]

boardSummary :: BoardPreset -> Array PedalPreset -> String
boardSummary bp allPresets =
  let entries = Map.toUnfoldable bp.pedals :: Array (Tuple PedalId BoardPresetEntry)
      parts = Array.mapMaybe summarizeEntry entries
  in if Array.null parts then "Empty" else Array.intercalate "  " parts
  where
  summarizeEntry (Tuple pid entry) = case entry.engage of
    EngageNoChange -> Nothing
    EngageOff -> do
      def <- Registry.findPedal pid
      Just (def.meta.name <> " off")
    _ -> do
      def <- Registry.findPedal pid
      let presetLabel = case entry.presetId of
            Just presetId -> case Array.find (\p -> p.id == presetId) allPresets of
              Just preset -> case preset.savedSlot of
                Just slot -> "[" <> show (unProgramNumber slot) <> "]"
                Nothing -> "[LIB]"
              Nothing -> ""
            Nothing -> ""
      Just (def.meta.name <> presetLabel)

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }

  ChangeEngage pid valStr -> do
    let eng = valueToEngage valStr
    H.modify_ \st -> st { grid = Map.update (\e -> Just e { engage = eng }) pid st.grid }
    H.raise (SendEngageAudition pid eng)

  ChangePreset pid valStr -> do
    let mPresetId = if valStr == "" then Nothing else Just valStr
    H.modify_ \st -> st { grid = Map.update (\e -> Just e { selectedPresetId = mPresetId }) pid st.grid }
    -- Live audition: send PC if preset has a saved slot
    case mPresetId of
      Nothing -> pure unit
      Just presetId -> do
        st <- H.get
        case Array.find (\p -> p.id == presetId) st.input.presets of
          Just preset -> case preset.savedSlot of
            Just slot -> H.raise (SendPCAudition pid slot)
            Nothing -> pure unit
          Nothing -> pure unit

  AllOn ->
    H.modify_ \st -> st { grid = map (\e -> e { engage = EngageOn }) st.grid }

  AllOff ->
    H.modify_ \st -> st { grid = map (\e -> e { engage = EngageOff }) st.grid }

  AllNoChange ->
    H.modify_ \st -> st { grid = map (\e -> e { engage = EngageNoChange, selectedPresetId = Nothing }) st.grid }

  ClickRecallBoard presetId -> do
    st <- H.get
    case Array.find (\bp -> bp.id == presetId) st.input.boardPresets of
      Nothing -> pure unit
      Just bp -> do
        let entries = Map.toUnfoldable bp.pedals :: Array (Tuple PedalId BoardPresetEntry)
            newGrid = Map.fromFoldable $ map (\(Tuple pid entry) ->
              Tuple pid { engage: entry.engage, selectedPresetId: entry.presetId }
            ) entries
        H.modify_ \s -> s { grid = Map.union newGrid s.grid, recalling = Just presetId }
        H.raise (RecallBoard bp)
        H.modify_ _ { recalling = Nothing }

  ClickLoadBoard presetId -> do
    st <- H.get
    case Array.find (\bp -> bp.id == presetId) st.input.boardPresets of
      Nothing -> pure unit
      Just bp -> do
        let entries = Map.toUnfoldable bp.pedals :: Array (Tuple PedalId BoardPresetEntry)
            newGrid = Map.fromFoldable $ map (\(Tuple pid entry) ->
              Tuple pid { engage: entry.engage, selectedPresetId: entry.presetId }
            ) entries
        H.modify_ \s -> s { grid = Map.union newGrid s.grid }
