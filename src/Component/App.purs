module Component.App (component) where

import Prelude

import Component.Boards.View as BoardsView
import Component.Controls.View as ControlsView
import Component.Detail.View as DetailView
import Component.Grid.View as GridView
import Component.Header as Header
import Component.Loopy.Panel as LoopyPanel
import Data.Loopy as Loopy
import Data.Array as Array
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.MC6.Backup as Backup
import Data.MC6.ControlBank as ControlBank
import Data.MC6.Message as MC6Msg
import Data.MC6.SysEx as SysEx
import Data.MC6.Types (MC6Message, MC6NativeBank, MC6Preset, MC6MsgType(..), MC6Action(..))
import Data.Foldable (any, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Midi (CC, MidiValue, ProgramNumber, makeChannel, unCC, unChannel, unMidiValue, unProgramNumber, unsafeCC, unsafeMidiValue)
import Data.Pedal (PedalId)
import Data.Pedal.Engage (EngageConfig(..), EngageState(..), engageCCs)
import Data.Preset (PedalPreset, BoardPreset, PresetId)
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Twister (SideBtn(..), TwisterEncoder(..), TwisterMsg(..), parseTwisterMsg)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Config.Decode as Decode
import Data.Either (Either(..))
import Data.String.CodeUnits as SCU
import Engine (AppState, MC6Assignment, View(..), initAppState, initEngineFromPedals)
import Config.Preset as CPreset
import Engine.Storage as Storage
import Engine.Twister as Twister
import Foreign.FileIO as FileIO
import Foreign.LoopyProject as LoopyProject
import Foreign.WebMIDI as MIDI
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Config.Registry as CRegistry
import Type.Proxy (Proxy(..))
import Web.DOM.Element as Element
import Web.HTML (window)
import Web.HTML.HTMLDocument (body) as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

data Action
  = Initialize
  | SetView View
  | SetValue PedalId CC MidiValue
  | SendMomentary PedalId CC MidiValue
  | SetInfo PedalId String Int
  | SelectPedalOutput String
  | SelectTwisterInput String
  | TwisterMidiReceived (Array Int)
  | HandleHeader Header.Output
  | HandleDetail DetailView.Output
  | HandleGrid GridView.Output
  | HandleBoards BoardsView.Output
  | HandleControls ControlsView.Output
  | HandleSideGrid GridView.Output
  | HandleLoopy LoopyPanel.Output
  | SelectLoopyOutput String
  | SelectMC6Input String
  | MC6MidiReceived (Array Int)
  | ExportAllPresetsAction
  | ExportAllBoardsAction
  | ImportPresetsFromFileAction
  | ImportBoardsFromFileAction
  | ExportMC6BackupAction
  | SelectBoardBank Int
  | ClickMC6Switch Int
  | UnassignMC6Switch Int
  | ClearMC6Bank

type Slots =
  ( header :: Header.Slot Unit
  , grid :: GridView.Slot Unit
  , detail :: DetailView.Slot Unit
  , boards :: BoardsView.Slot Unit
  , controls :: ControlsView.Slot Unit
  , sideGrid :: GridView.Slot Unit
  , loopy :: LoopyPanel.Slot Unit
  )

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const initAppState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

render :: forall m. MonadAff m => AppState -> H.ComponentHTML Action Slots m
render state = case state.configError of
  Just err ->
    HH.div [ HP.class_ (H.ClassName "config-error") ]
      [ HH.h2_ [ HH.text "Configuration Error" ]
      , HH.p_ [ HH.text err ]
      , HH.p_ [ HH.text "Check that config/rig.json and config/pedals/*.json are accessible." ]
      ]
  Nothing ->
   HH.div
    [ HP.class_ (H.ClassName "app") ]
    [ HH.slot (Proxy :: _ "header") unit Header.component
        { view: state.view
        , cardOrder: state.cardOrder
        , hiddenPedals: state.hiddenPedals
        , boardsActivePedal: state.boardsActivePedal
        , registry: state.registry
        }
        HandleHeader
    , case state.view of
        GridView ->
          HH.slot (Proxy :: _ "grid") unit GridView.component
            { engine: state.engine
            , cardOrder: state.cardOrder
            , hiddenPedals: state.hiddenPedals
            , presets: state.presets
            , connections: state.connections
            , registry: state.registry
            }
            HandleGrid
        DetailView pid ->
          HH.slot (Proxy :: _ "detail") unit DetailView.component
            { engine: state.engine
            , pedalId: pid
            , cardOrder: state.cardOrder
            , registry: state.registry
            }
            HandleDetail
        ControlsView ->
          HH.slot (Proxy :: _ "controls") unit ControlsView.component
            { controlBanks: state.controlBanks
            , activeControlBankIdx: state.activeControlBankIdx
            , registry: state.registry
            , mc6BoardBankNum: state.mc6BoardBankNum
            }
            HandleControls
        FilesView -> renderFilesView
        DocsView -> renderDocsView state
        ConnectView -> renderConnectView state
        BoardsView -> HH.text ""
    -- Boards always rendered for state persistence
    , HH.div
        [ HP.class_ (H.ClassName (if state.view == BoardsView then "boards-wrapper" else "boards-persist-hidden")) ]
        ( case state.view, state.boardsActivePedal of
            BoardsView, Just pid ->
              [ HH.slot (Proxy :: _ "sideGrid") unit GridView.component
                  { engine: state.engine
                  , cardOrder: [pid]
                  , hiddenPedals: []
                  , presets: state.presets
                  , connections: state.connections
                  , registry: state.registry
                  }
                  HandleSideGrid
              ]
            _, _ -> []
        <>
          [ HH.slot (Proxy :: _ "boards") unit BoardsView.component
              { engine: state.engine
              , connections: state.connections
              , presets: state.presets
              , boardPresets: state.boardPresets
              , registry: state.registry
              , mc6ActiveBank: Array.find (\b -> b.bankNumber == state.mc6BoardBankNum) state.mc6Banks
              , mc6Assignments: state.mc6Assignments
              }
              HandleBoards
          , HH.div [ HP.class_ (H.ClassName "right-panels") ]
              [ HH.slot (Proxy :: _ "loopy") unit LoopyPanel.component
                  { connections: state.connections
                  , loopyTwisterActive: state.loopyTwisterActive
                  , selectedLoop: state.loopySelectedLoop
                  , loopStates: state.loopyLoopStates
                  , heldEncoder: Nothing
                  , clipSettings: state.loopyClipSettings
                  }
                  HandleLoopy
              , renderMC6Panel state
              ]
          ]
        )
    ]

renderFilesView :: forall m. MonadAff m => H.ComponentHTML Action Slots m
renderFilesView =
  HH.div [ HP.class_ (H.ClassName "files-view") ]
    [ HH.p [ HP.class_ (H.ClassName "files-description") ]
        [ HH.text "Your presets are stored in this browser's local storage and will persist as long as the app is served from the same location. To back up your presets, share them, or edit them by hand, you can export to a JSON file and import it later." ]
    , HH.div [ HP.class_ (H.ClassName "files-actions") ]
        [ HH.div [ HP.class_ (H.ClassName "files-group") ]
            [ HH.h3_ [ HH.text "Pedal Presets" ]
            , HH.button
                [ HP.class_ (H.ClassName "files-btn")
                , HE.onClick \_ -> ExportAllPresetsAction
                ]
                [ HH.text "Export All Presets" ]
            , HH.button
                [ HP.class_ (H.ClassName "files-btn")
                , HE.onClick \_ -> ImportPresetsFromFileAction
                ]
                [ HH.text "Import Presets" ]
            ]
        , HH.div [ HP.class_ (H.ClassName "files-group") ]
            [ HH.h3_ [ HH.text "Board Presets" ]
            , HH.button
                [ HP.class_ (H.ClassName "files-btn")
                , HE.onClick \_ -> ExportAllBoardsAction
                ]
                [ HH.text "Export All Boards" ]
            , HH.button
                [ HP.class_ (H.ClassName "files-btn")
                , HE.onClick \_ -> ImportBoardsFromFileAction
                ]
                [ HH.text "Import Boards" ]
            ]
        ]
    ]

-- | MIDI connections view — port selection and signal flow
renderConnectView :: forall m. MonadAff m => AppState -> H.ComponentHTML Action Slots m
renderConnectView state =
  HH.div [ HP.class_ (H.ClassName "connect-view") ]
    [ HH.h2_ [ HH.text "MIDI Connections" ]
    , HH.div [ HP.class_ (H.ClassName "connect-grid") ]
        [ connectCard
            { label: "Pedal MIDI"
            , description: "Output to pedals via MC6 MIDI Thru. Sends CC changes when you adjust controls in the Grid or Detail views."
            , selectedId: state.connections.pedalOutputId
            , ports: state.connections.availableOutputs
            , onChange: SelectPedalOutput
            }
        , connectCard
            { label: "Twister"
            , description: "Input from Midifighter Twister. Receives encoder turns and button presses for loop selection and parameter control."
            , selectedId: state.connections.twisterInputId
            , ports: state.connections.availableInputs
            , onChange: SelectTwisterInput
            }
        , connectCard
            { label: "LoopyPro"
            , description: "Output to LoopyPro on iPad. Sends transport, loop actions, and parameter CCs on channel 16 via AUDIO4c."
            , selectedId: state.connections.loopyOutputId
            , ports: state.connections.availableOutputs
            , onChange: SelectLoopyOutput
            }
        , connectCard
            { label: "MC6"
            , description: "Input from Morningstar MC6 footswitch. Relays footswitch presses to LoopyPro and updates the phase display."
            , selectedId: state.connections.mc6InputId
            , ports: state.connections.availableInputs
            , onChange: SelectMC6Input
            }
        ]
    , HH.div [ HP.class_ (H.ClassName "connect-flow") ]
        [ HH.h3_ [ HH.text "Signal Flow" ]
        , HH.p_
            [ HH.text "Twister/MC6 \x2192 this app \x2192 LoopyPro (via AUDIO4c). "
            , HH.text "Pedal CCs go out a separate MIDI output to the pedalboard via MC6 MIDI Thru."
            ]
        ]
    ]
  where
  connectCard { label, description, selectedId, ports, onChange } =
    let connected = selectedId /= Nothing
        cls = "connect-card" <> if connected then " connected" else " disconnected"
    in HH.div [ HP.class_ (H.ClassName cls) ]
      [ HH.div [ HP.class_ (H.ClassName "connect-card-header") ]
          [ HH.span [ HP.class_ (H.ClassName "connect-card-status") ] []
          , HH.span [ HP.class_ (H.ClassName "connect-card-label") ] [ HH.text label ]
          ]
      , HH.p [ HP.class_ (H.ClassName "connect-card-desc") ] [ HH.text description ]
      , HH.select
          [ HP.class_ (H.ClassName "connect-select")
          , HE.onValueChange onChange
          ]
          ( [ HH.option [ HP.value "" ] [ HH.text "\x2014 not connected \x2014" ] ]
            <> map (\port ->
                HH.option
                  [ HP.value port.id
                  , HP.selected (selectedId == Just port.id)
                  ]
                  [ HH.text port.name ]
              ) ports
          )
      ]

-- | Documentation view — MIDI reference derived from config data
renderDocsView :: forall m. MonadAff m => AppState -> H.ComponentHTML Action Slots m
renderDocsView state =
  HH.div [ HP.class_ (H.ClassName "docs-view") ]
    [ HH.h2_ [ HH.text "MIDI Reference" ]
    , renderMC6Section
    , renderTwisterSection
    , renderCCSection
    ]
  where
  -- Helper: CC number → Loopy action label
  ccLabel :: Int -> String
  ccLabel cc = case Array.find (\a -> unCC a.cc == cc) Loopy.actions of
    Just a -> a.label
    Nothing -> "CC " <> show cc

  -- Extract press action from MC6 preset messages
  pressAction p =
    case Array.find (\m -> m.action == ActionPress && m.msgType == MsgCC) p.messages of
      Just m -> { label: ccLabel m.data1, cc: show m.data1 }
      Nothing -> case Array.find (\m -> m.action == ActionPress && m.msgType == MsgBankJump) p.messages of
        Just _ -> { label: "Boards", cc: "" }
        Nothing -> { label: "", cc: "" }

  -- Extract long press action from MC6 preset messages
  longPressAction p =
    case Array.find (\m -> m.action == ActionLongPress && m.msgType == MsgCC) p.messages of
      Just m -> { label: ccLabel m.data1, cc: show m.data1 }
      Nothing -> case Array.find (\m -> m.action == ActionLongPress && m.msgType == MsgBankJump) p.messages of
        Just _ -> { label: "Boards", cc: "" }
        Nothing -> { label: "", cc: "" }

  -- Find the LoopyPro bank
  loopyBank = Array.find (\b -> b.bankName == "LoopyPro") state.mc6Banks

  preset idx = loopyBank >>= \b -> Array.index b.presets idx

  -- MC6 Footswitch section
  renderMC6Section =
    HH.div [ HP.class_ (H.ClassName "docs-section") ]
      [ HH.h3_ [ HH.text "MC6 Footswitches — LoopyPro" ]
      , HH.p [ HP.class_ (H.ClassName "docs-note") ]
          [ HH.text "Channel 16, momentary (127 on press, 0 on release)." ]
      , HH.table [ HP.class_ (H.ClassName "docs-table") ]
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "" ]
                  , HH.th_ [ HH.text "Press" ]
                  , HH.th_ [ HH.text "CC" ]
                  , HH.th_ [ HH.text "Long Press" ]
                  , HH.th_ [ HH.text "CC" ]
                  ]
              ]
          , HH.tbody_
              [ switchRow "A" 0, switchRow "B" 1, switchRow "C" 2
              , switchRow "D" 3, switchRow "E" 4, switchRow "F" 5
              , switchRow "G" 6, switchRow "H" 7, switchRow "I" 8
              ]
          ]
      ]

  switchRow letter idx =
    case preset idx of
      Just p | p.shortName /= "" ->
        let press = pressAction p
            lp = longPressAction p
            zone = if idx >= 6 then " docs-fs3x" else ""
        in HH.tr [ HP.class_ (H.ClassName ("docs-switch" <> zone)) ]
          [ HH.td [ HP.class_ (H.ClassName "docs-switch-letter") ] [ HH.text letter ]
          , HH.td_ [ HH.text press.label ]
          , HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text press.cc ]
          , HH.td_ [ HH.text lp.label ]
          , HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text lp.cc ]
          ]
      _ ->
        HH.tr [ HP.class_ (H.ClassName "docs-switch docs-empty") ]
          [ HH.td_ [ HH.text letter ]
          , HH.td_ [], HH.td_ [], HH.td_ [], HH.td_ []
          ]

  -- Twister section
  renderTwisterSection =
    HH.div [ HP.class_ (H.ClassName "docs-section") ]
      [ HH.h3_ [ HH.text "Midifighter Twister" ]
      , HH.h4_ [ HH.text "Top Row - Loop Selection (enc 0-7)" ]
      , HH.table [ HP.class_ (H.ClassName "docs-table") ]
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "Enc" ]
                  , HH.th_ [ HH.text "Group" ]
                  , HH.th_ [ HH.text "Select" ]
                  , HH.th_ [ HH.text "Volume" ]
                  ]
              ]
          , HH.tbody_ (map renderLoopEnc (Array.range 0 7))
          ]
      , HH.h4_ [ HH.text "Bottom Row - Loop Config (enc 8-15)" ]
      , HH.table [ HP.class_ (H.ClassName "docs-table") ]
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "Enc" ]
                  , HH.th_ [ HH.text "Rotation" ]
                  , HH.th_ [ HH.text "CC" ]
                  , HH.th_ [ HH.text "Shift" ]
                  , HH.th_ [ HH.text "CC" ]
                  ]
              ]
          , HH.tbody_ (Array.mapWithIndex renderParamEnc Loopy.loopConfigBank.params)
          ]
      ]

  renderLoopEnc enc =
    let loopIdx = Loopy.encoderToLoop enc
        groupIdx = loopIdx / 2
        isB = loopIdx `mod` 2 == 1
        mGroup = Array.index Loopy.groups groupIdx
        label = case mGroup of
          Just g -> g.color.label <> (if isB then " B" else " A")
          Nothing -> ""
        hex = case mGroup of
          Just g -> g.color.color
          Nothing -> ""
    in HH.tr_
      [ HH.td_ [ HH.text (show enc) ]
      , HH.td [ HP.attr (HH.AttrName "style") ("color: " <> hex) ] [ HH.text label ]
      , HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text ("CC " <> show (30 + loopIdx)) ]
      , HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text ("CC " <> show (40 + loopIdx)) ]
      ]

  renderParamEnc idx param =
    let shiftLabel = case Loopy.paramShift param of
          Just s -> s.label
          Nothing -> ""
        shiftCC = case Loopy.paramShift param of
          Just s -> "CC " <> show (unCC s.cc)
          Nothing -> ""
    in HH.tr_
      [ HH.td_ [ HH.text (show (8 + idx)) ]
      , HH.td_ [ HH.text (Loopy.paramLabel param) ]
      , HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text ("CC " <> show (unCC (Loopy.paramCC param))) ]
      , HH.td_ [ HH.text shiftLabel ]
      , HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text shiftCC ]
      ]

  -- CC reference table
  renderCCSection =
    HH.div [ HP.class_ (H.ClassName "docs-section") ]
      [ HH.h3_ [ HH.text "LoopyPro CC Map" ]
      , HH.p [ HP.class_ (H.ClassName "docs-note") ]
          [ HH.text "All on channel 16 via AUDIO4c." ]
      , HH.table [ HP.class_ (H.ClassName "docs-table") ]
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "CC" ]
                  , HH.th_ [ HH.text "Action" ]
                  , HH.th_ [ HH.text "Type" ]
                  ]
              ]
          , HH.tbody_
              ( map (\a -> HH.tr_
                  [ HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text (show (unCC a.cc)) ]
                  , HH.td_ [ HH.text a.label ]
                  , HH.td_ [ HH.text "Momentary" ]
                  ]) Loopy.actions
              <> map (\r -> HH.tr_
                  [ HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text (show r.cc) ]
                  , HH.td_ [ HH.text r.label ]
                  , HH.td_ [ HH.text r.typ ]
                  ])
                  [ { cc: 30, label: "Select Loop 0", typ: "Momentary" }
                  , { cc: 37, label: "Select Loop 7", typ: "Momentary" }
                  , { cc: 40, label: "Volume Loop 0", typ: "Continuous" }
                  , { cc: 47, label: "Volume Loop 7", typ: "Continuous" }
                  , { cc: 53, label: "Speed", typ: "Continuous" }
                  , { cc: 54, label: "Fade In", typ: "Continuous" }
                  , { cc: 55, label: "Fade Out", typ: "Continuous" }
                  , { cc: 56, label: "Reverse", typ: "Toggle" }
                  , { cc: 70, label: "Pan Loop 0", typ: "Continuous" }
                  , { cc: 77, label: "Pan Loop 7", typ: "Continuous" }
                  , { cc: 80, label: "Overdub Feedback", typ: "Continuous" }
                  , { cc: 81, label: "Phase Lock", typ: "Toggle" }
                  , { cc: 82, label: "Threshold Recording", typ: "Toggle" }
                  ]
              )
          ]
      ]

-- | MC6 footswitch panel — 3 rows of 3 in hardware layout: D E F / A B C / G H I
renderMC6Panel :: forall m. MonadAff m => AppState -> H.ComponentHTML Action Slots m
renderMC6Panel state =
  HH.div [ HP.class_ (H.ClassName "mc6-panel") ]
    [ HH.div [ HP.class_ (H.ClassName "mc6-header") ]
        [ HH.span [ HP.class_ (H.ClassName "mc6-title") ] [ HH.text "MC6" ]
        , HH.button
            [ HP.class_ (H.ClassName "mc6-clear-btn")
            , HE.onClick \_ -> ClearMC6Bank
            ]
            [ HH.text "Clear" ]
        , HH.button
            [ HP.class_ (H.ClassName "mc6-export-btn")
            , HE.onClick \_ -> ExportMC6BackupAction
            ]
            [ HH.text "Export" ]
        ]
    , HH.div [ HP.class_ (H.ClassName "mc6-bank-tabs") ]
        (map (\n ->
          HH.button
            [ HP.class_ (H.ClassName ("mc6-bank-tab" <> if n == state.mc6BoardBankNum then " active" else ""))
            , HE.onClick \_ -> SelectBoardBank n
            ]
            [ HH.text (show n) ]
        ) [1, 2, 3, 4, 5])
    , HH.div [ HP.class_ (H.ClassName "mc6-switches") ]
        -- Row 1: D E F (preset indices 3, 4, 5)
        [ renderSwitchRow "D" 3, renderSwitchRow "E" 4, renderSwitchRow "F" 5
        -- Row 2: A B C (preset indices 0, 1, 2)
        , renderSwitchRow "A" 0, renderSwitchRow "B" 1, renderSwitchRow "C" 2
        -- Row 3: G H I (preset indices 6, 7, 8)
        , renderSwitchRow "G" 6, renderSwitchRow "H" 7, renderSwitchRow "I" 8
        ]
    ]
  where
  -- Look up board assignment for a switch in the active board bank
  boardForSwitch :: Int -> Maybe { boardPresetId :: String, boardName :: String }
  boardForSwitch idx =
    case Array.find (\a -> a.bankNumber == state.mc6BoardBankNum && a.switchIndex == idx) state.mc6Assignments of
      Just a -> case Array.find (\bp -> bp.id == a.boardPresetId) state.boardPresets of
        Just bp -> Just { boardPresetId: a.boardPresetId, boardName: bp.name }
        Nothing -> Nothing
      Nothing -> Nothing

  renderSwitchRow :: String -> Int -> H.ComponentHTML Action Slots m
  renderSwitchRow letter idx =
    case boardForSwitch idx of
      Just board ->
        HH.div
          [ HP.class_ (H.ClassName "mc6-switch board-assigned")
          , HE.onClick \_ -> ClickMC6Switch idx
          ]
          [ HH.span [ HP.class_ (H.ClassName "mc6-switch-letter") ] [ HH.text letter ]
          , HH.span [ HP.class_ (H.ClassName "mc6-switch-label") ] [ HH.text (SCU.take 8 board.boardName) ]
          , HH.button
              [ HP.class_ (H.ClassName "mc6-switch-clear")
              , HE.onClick \_ -> UnassignMC6Switch idx
              ]
              [ HH.text "\x00d7" ]
          ]
      Nothing ->
        HH.div
          [ HP.class_ (H.ClassName "mc6-switch empty") ]
          [ HH.span [ HP.class_ (H.ClassName "mc6-switch-letter") ] [ HH.text letter ]
          , HH.span [ HP.class_ (H.ClassName "mc6-switch-label") ] [ HH.text letter ]
          ]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM AppState Action Slots o m Unit
handleAction = case _ of
  Initialize -> do
    -- Load configuration from JSON files
    eConfig <- H.liftAff $ Decode.loadRig "./"
    case eConfig of
      Left err -> do
        liftEffect $ Console.log $ "Config load failed: " <> show err
        H.modify_ _ { configError = Just (show err) }
      Right { rig, pedals } -> do
        let registry = CRegistry.mkRegistry pedals rig.slotRanges rig.midiRouting
            defaultEngine = initEngineFromPedals pedals
            defaultCardOrder = map _.meta.id pedals
        H.modify_ _ { registry = registry, engine = defaultEngine, cardOrder = defaultCardOrder }
        -- Load controller config (MC6 banks)
        when (rig.controller /= "") do
          let controllerUrl = "./" <> "config/" <> rig.controller
          eController <- H.liftAff $ Decode.loadController controllerUrl
          case eController of
            Left err -> liftEffect $ Console.log $ "Controller config error: " <> show err
            Right ctrlConfig -> do
              liftEffect $ Console.log $ "MC6 banks loaded: " <> show (Array.length ctrlConfig.banks) <> " banks"
              H.modify_ _ { mc6Banks = ctrlConfig.banks }
        -- Load saved state from localStorage (overrides defaults)
        mEngine <- liftEffect Storage.loadEngineState
        case mEngine of
          Just eng -> H.modify_ _ { engine = eng }
          Nothing -> pure unit
        cardOrder <- liftEffect $ Storage.loadCardOrderParsed defaultCardOrder
        H.modify_ _ { cardOrder = cardOrder }
        presets <- liftEffect Storage.loadPresetsParsed
        boardPresets <- liftEffect Storage.loadBoardPresetsParsed
        mc6Assignments <- liftEffect Storage.loadMC6AssignmentsParsed
        currentSt <- H.get
        controlBanks <- liftEffect $ Storage.loadControlBanksParsed currentSt.controlBanks
        H.modify_ _ { presets = presets, boardPresets = boardPresets, mc6Assignments = mc6Assignments, controlBanks = controlBanks }
        st <- H.get
        liftEffect do
          w <- window
          d <- document w
          mb <- HTMLDocument.body d
          for_ mb \b -> case st.view of
            GridView -> Element.setClassName "grid-mode" (HTMLElement.toElement b)
            _ -> pure unit
        -- MIDI initialization
        mAccess <- H.liftAff MIDI.requestMIDIAccess
        outputs <- liftEffect $ MIDI.getOutputs mAccess
        inputs <- liftEffect $ MIDI.getInputs mAccess
        H.modify_ _ { connections { access = Just mAccess
                                   , availableOutputs = outputs
                                   , availableInputs = inputs } }
        -- Auto-select MIDI ports using routing patterns from registry
        let routing = rig.midiRouting
        -- Auto-select Twister input
        let mTwisterIn = Array.find (\p -> contains (Pattern routing.twisterInput.match) p.name) inputs
        for_ mTwisterIn \port ->
          handleAction (SelectTwisterInput port.id)
        -- Auto-select Twister output
        let mTwisterOut = Array.find (\p -> contains (Pattern routing.twisterOutput.match) p.name) outputs
        for_ mTwisterOut \port -> do
          mOut <- liftEffect $ MIDI.openOutput mAccess port.id
          H.modify_ _ { connections { twisterOutput = mOut, twisterOutputId = Just port.id } }
        -- Auto-select pedal output (MC6 via USB)
        let mPedalOut = Array.find (\p -> contains (Pattern routing.pedalOutput.match) p.name) outputs
        for_ mPedalOut \port ->
          handleAction (SelectPedalOutput port.id)
        -- Auto-select LoopyPro output
        let mLoopyOut = Array.find (\p -> contains (Pattern routing.loopyOutput.match) p.name) outputs
        for_ mLoopyOut \port ->
          handleAction (SelectLoopyOutput port.id)
        -- Auto-select MC6 input (relay to LoopyPro)
        let mMC6In = Array.find (\p -> contains (Pattern routing.mc6Input.match) p.name) inputs
        for_ mMC6In \port ->
          handleAction (SelectMC6Input port.id)
        -- Auto-select MC6 output (SysEx programming) — same device name as input
        when (routing.mc6Input.match /= "") do
          let mMC6Out = Array.find (\p -> contains (Pattern routing.mc6Input.match) p.name) outputs
          for_ mMC6Out \port -> do
            mOut <- liftEffect $ MIDI.openOutput mAccess port.id
            H.modify_ _ { connections { mc6Output = mOut, mc6OutputId = Just port.id } }

  SetView view -> do
    H.modify_ _ { view = view }
    case view of
      DetailView pid -> do
        H.modify_ _ { focusPedalId = Just pid }
        sendAllLEDs pid
      _ -> pure unit
    liftEffect do
      w <- window
      d <- document w
      mb <- HTMLDocument.body d
      for_ mb \b -> case view of
        GridView -> Element.setClassName "grid-mode" (HTMLElement.toElement b)
        _ -> Element.setClassName "" (HTMLElement.toElement b)

  SetValue pid ccNum val -> do
    liftEffect $ Console.log $ "MIDI CC: pedal=" <> show pid <> " cc=" <> show (unCC ccNum) <> " val=" <> show (unMidiValue val)
    H.modify_ \st -> st
      { engine = Map.update
          (\ps -> Just ps { values = Map.insert ccNum val ps.values })
          pid
          st.engine
      }
    st <- H.get
    case st.connections.pedalOutput of
      Nothing -> pure unit
      Just output -> do
        let mCh = do
              ps <- Map.lookup pid st.engine
              makeChannel ps.channel
        case mCh of
          Nothing -> pure unit
          Just ch -> liftEffect $ MIDI.sendCC output ch ccNum val
    -- LED feedback for UI-originated changes
    unless st.suppressTwister do
      for_ st.focusPedalId \focusPid ->
        when (focusPid == pid) do
          case CRegistry.findPedal st.registry pid of
            Nothing -> pure unit
            Just def -> case def.twister of
              Nothing -> pure unit
              Just tw -> do
                let mIdx = Array.findIndex (case _ of
                      Just (TwisterCC { cc: ecc }) -> ecc == ccNum
                      _ -> false) tw.encoders
                for_ mIdx \idx ->
                  sendRingPosition idx
                    (Twister.ringValueForEncoder (Array.index tw.encoders idx >>= identity) (unMidiValue val))

  SendMomentary pid ccNum val ->
    handleAction (SetValue pid ccNum val)

  SetInfo pid key val ->
    H.modify_ \st -> st
      { engine = Map.update
          (\ps -> Just ps { info = Map.insert key val ps.info })
          pid
          st.engine
      }

  SelectPedalOutput portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mOut <- liftEffect $ MIDI.openOutput access portId
        H.modify_ _ { connections { pedalOutput = mOut, pedalOutputId = Just portId } }

  SelectTwisterInput portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mInput <- liftEffect $ MIDI.openInput access portId
        case mInput of
          Nothing -> pure unit
          Just input -> do
            H.modify_ _ { connections { twisterInput = Just input, twisterInputId = Just portId } }
            void $ H.subscribe $ HS.makeEmitter \emit ->
              MIDI.onMessage input \bytes ->
                emit (TwisterMidiReceived bytes)

  TwisterMidiReceived bytes -> do
    st <- H.get
    when st.loopyTwisterActive $
      liftEffect $ Console.log $ "Twister (loopy mode): " <> show bytes
    case parseTwisterMsg bytes of
      Nothing -> pure unit
      Just msg -> do
        if st.loopyTwisterActive
          then handleLoopyTwisterMsg msg
          else case msg of
            EncoderTurn idx val -> handleEncoderTurn idx val
            EncoderPress idx -> handleEncoderPress idx
            EncoderRelease _ -> pure unit
            SideButton btn -> handleTwisterSideButton btn

  HandleHeader output -> case output of
    Header.ViewChanged view -> handleAction (SetView view)
    Header.PedalPillClicked pid -> do
      st <- H.get
      case st.view of
        GridView ->
          H.modify_ \s -> s { hiddenPedals =
            if Array.elem pid s.hiddenPedals
              then Array.filter (_ /= pid) s.hiddenPedals
              else Array.snoc s.hiddenPedals pid
          }
        BoardsView ->
          H.modify_ \s -> s { boardsActivePedal =
            if s.boardsActivePedal == Just pid
              then Nothing
              else Just pid
          }
        _ -> pure unit

  HandleDetail output -> case output of
    DetailView.ValueChanged pid ccNum val -> handleAction (SetValue pid ccNum val)
    DetailView.MomentarySent pid ccNum val -> handleAction (SendMomentary pid ccNum val)
    DetailView.PedalSelected pid -> handleAction (SetView (DetailView pid))
    DetailView.InfoChanged pid key val -> handleAction (SetInfo pid key val)

  HandleGrid output -> handleGridOutput output

  HandleSideGrid output -> case output of
    GridView.PedalClicked _ -> pure unit
    GridView.PedalFocused pid -> do
      H.modify_ _ { focusPedalId = Just pid }
      sendAllLEDs pid
    GridView.OrderChanged _ -> pure unit
    GridView.ValueChanged pid cc val -> handleAction (SetValue pid cc val)
    GridView.MomentarySent pid cc val -> handleAction (SendMomentary pid cc val)
    GridView.InfoChanged pid key val -> handleAction (SetInfo pid key val)
    GridView.RecallPreset preset -> do
      recallPreset preset
      autoEngageIfNeeded preset
    GridView.SendPC pid pn -> sendPC_ pid pn
    GridView.SavePreset r -> handleSavePreset r
    GridView.OverwritePreset presetId pedalId -> handleOverwritePreset presetId pedalId
    GridView.DeletePreset presetId -> handleDeletePreset presetId
    GridView.AssignSlot presetId pn -> handleAssignSlot presetId pn
    GridView.ExportPreset preset -> handleExportPreset preset
    GridView.ImportPresets presets -> handleImportPresets presets

  HandleBoards output -> case output of
    BoardsView.RecallBoard bp -> recallBoard bp
    BoardsView.SendEngageAudition pid engState -> sendEngage pid engState
    BoardsView.SendPCAudition pid pn -> sendPC_ pid pn
    BoardsView.RecallPresetAudition preset -> do
      recallPreset preset
      autoEngageIfNeeded preset
    BoardsView.FocusPedal pid -> H.modify_ _ { boardsActivePedal = Just pid }
    BoardsView.SendEngageAll engState -> do
      st <- H.get
      for_ st.cardOrder \pid ->
        sendEngage pid engState
    BoardsView.ValueChanged pid cc val -> handleAction (SetValue pid cc val)
    BoardsView.SaveBoard r -> handleSaveBoard r
    BoardsView.UpdateBoard presetId r -> handleUpdateBoard presetId r
    BoardsView.OverwriteBoard presetId pedals -> handleOverwriteBoard presetId pedals
    BoardsView.DeleteBoard presetId -> handleDeleteBoard presetId
    BoardsView.ExportBoard bp -> handleExportBoard bp
    BoardsView.ImportBoards boards -> handleImportBoards boards
    BoardsView.AssignBoardToSwitch boardId switchIdx -> handleAssignBoardToSwitch boardId switchIdx
    BoardsView.UnassignBoard boardId -> handleUnassignBoard boardId

  HandleControls output -> case output of
    ControlsView.SaveControlBanks banks mActiveIdx -> do
      H.modify_ _ { controlBanks = banks, activeControlBankIdx = mActiveIdx }
      st <- H.get
      liftEffect $ Storage.saveControlBanks st.controlBanks
    ControlsView.SyncControlBankToMC6 ->
      syncControlBankToMC6

  HandleLoopy output -> case output of
    LoopyPanel.LoopSelected idx -> do
      st <- H.get
      if st.loopySelectedLoop == idx
        then
          -- Local-only deselect: LoopyPro has no deselect action,
          -- so just hide parameter UI without sending MIDI
          H.modify_ _ { loopySelectedLoop = -1 }
        else do
          sendMomentaryLoopy (Loopy.selectCC (Loopy.LoopIndex idx))
          H.modify_ _ { loopySelectedLoop = idx }
    LoopyPanel.ActionGridPressed cc -> do
      st <- H.get
      sendMomentaryLoopy cc
      trackLoopyAction st.loopySelectedLoop cc
    LoopyPanel.ClearAllLoops -> do
      liftEffect $ Console.log "Clearing all loops..."
      for_ (Array.range 0 7) \i -> do
        sendMomentaryLoopy (Loopy.selectCC (Loopy.LoopIndex i))
        sendMomentaryLoopy Loopy.clearCC
        updateLoopState i (_ { phase = Loopy.PhaseEmpty })
      -- Re-select the previously selected loop
      st <- H.get
      sendMomentaryLoopy (Loopy.selectCC (Loopy.LoopIndex st.loopySelectedLoop))
    LoopyPanel.GenerateProject -> do
      liftEffect $ Console.log "Generating LoopyPro project..."
      H.liftAff $ LoopyProject.generateAndDownload "Explorer Template"
      liftEffect $ Console.log "LoopyPro project generated."
    LoopyPanel.ClipSettingChanged loopIdx settings ->
      H.modify_ \st -> st
        { loopyClipSettings = fromMaybe st.loopyClipSettings
            (Array.updateAt loopIdx settings st.loopyClipSettings) }
    LoopyPanel.TwisterModeToggled -> do
      st <- H.get
      let newActive = not st.loopyTwisterActive
      liftEffect $ Console.log $ "Loopy Twister mode: " <> show newActive
      H.modify_ _ { loopyTwisterActive = newActive }
      if newActive
        then sendLoopyLEDs
        else case st.focusPedalId of
          Just pid -> sendAllLEDs pid
          Nothing -> dimAllLEDs

  ExportAllPresetsAction -> handleExportAllPresets
  ExportAllBoardsAction -> handleExportAllBoards
  ImportPresetsFromFileAction -> handleImportPresetsFromFile
  ImportBoardsFromFileAction -> handleImportBoardsFromFile
  ExportMC6BackupAction -> handleExportMC6Backup

  SelectBoardBank n ->
    H.modify_ _ { mc6BoardBankNum = n }

  ClickMC6Switch idx -> do
    st <- H.get
    case Array.find (\a -> a.bankNumber == st.mc6BoardBankNum && a.switchIndex == idx) st.mc6Assignments of
      Just a -> case Array.find (\bp -> bp.id == a.boardPresetId) st.boardPresets of
        Just bp -> recallBoard bp
        Nothing -> pure unit
      Nothing -> pure unit

  UnassignMC6Switch switchIdx -> do
    st <- H.get
    let updated = Array.filter (\a -> not (a.bankNumber == st.mc6BoardBankNum && a.switchIndex == switchIdx)) st.mc6Assignments
    H.modify_ _ { mc6Assignments = updated }
    liftEffect $ Storage.saveMC6Assignments updated
    syncSwitchToMC6 st.mc6BoardBankNum switchIdx Nothing

  ClearMC6Bank -> handleClearMC6Bank

  SelectLoopyOutput portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mOut <- liftEffect $ MIDI.openOutput access portId
        H.modify_ _ { connections { loopyOutput = mOut, loopyOutputId = Just portId } }

  SelectMC6Input portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mInput <- liftEffect $ MIDI.openInput access portId
        case mInput of
          Nothing -> pure unit
          Just input -> do
            H.modify_ _ { connections { mc6Input = Just input, mc6InputId = Just portId } }
            void $ H.subscribe $ HS.makeEmitter \emit ->
              MIDI.onMessage input \bytes ->
                emit (MC6MidiReceived bytes)

  MC6MidiReceived bytes -> do
    liftEffect $ Console.log $ "MC6 relay: " <> show bytes
    st <- H.get
    for_ st.connections.loopyOutput \output ->
      liftEffect $ MIDI.send output bytes
    case bytes of
      -- Channel 1 CC with value 127 = board recall trigger
      [status, ccNum, 127] | status == 0xB0 ->
        handleBoardRecallFromMC6 ccNum
      -- Channel 16 CC with value 127 = LoopyPro action
      [status, ccNum, 127] | status == 0xBF ->
        case Loopy.ccToAction (unsafeCC ccNum) of
          Just action -> trackLoopyAction' st.loopySelectedLoop action
          Nothing -> pure unit
      _ -> pure unit

-- Grid output handler (shared by HandleGrid)
handleGridOutput :: forall o m. MonadAff m => GridView.Output -> H.HalogenM AppState Action Slots o m Unit
handleGridOutput = case _ of
  GridView.PedalClicked pid -> handleAction (SetView (DetailView pid))
  GridView.PedalFocused pid -> do
    H.modify_ _ { focusPedalId = Just pid }
    sendAllLEDs pid
  GridView.OrderChanged order -> H.modify_ _ { cardOrder = order }
  GridView.ValueChanged pid cc val -> handleAction (SetValue pid cc val)
  GridView.MomentarySent pid cc val -> handleAction (SendMomentary pid cc val)
  GridView.InfoChanged pid key val -> handleAction (SetInfo pid key val)
  GridView.RecallPreset preset -> recallPreset preset
  GridView.SendPC pid pn -> sendPC_ pid pn
  GridView.SavePreset r -> handleSavePreset r
  GridView.OverwritePreset presetId pedalId -> handleOverwritePreset presetId pedalId
  GridView.DeletePreset presetId -> handleDeletePreset presetId
  GridView.AssignSlot presetId pn -> handleAssignSlot presetId pn
  GridView.ExportPreset preset -> handleExportPreset preset
  GridView.ImportPresets presets -> handleImportPresets presets

-- Preset CRUD handlers

handleSavePreset :: forall o m. MonadAff m => { pedalId :: PedalId, name :: String, description :: String, notes :: String } -> H.HalogenM AppState Action Slots o m Unit
handleSavePreset r = do
  uuid <- liftEffect MIDI.randomUUID
  now <- liftEffect Storage.nowISO
  st <- H.get
  let mPs = Map.lookup r.pedalId st.engine
      values = case mPs of
        Just ps -> ps.values
        Nothing -> Map.empty
      info = case mPs of
        Just ps -> ps.info
        Nothing -> Map.empty
      preset :: PedalPreset
      preset =
        { id: uuid
        , pedalId: r.pedalId
        , name: r.name
        , description: r.description
        , notes: r.notes
        , values
        , info
        , savedSlot: Nothing
        , created: now
        , modified: now
        }
  H.modify_ \s -> s { presets = Array.cons preset s.presets }
  persistPresets

handleOverwritePreset :: forall o m. MonadAff m => PresetId -> PedalId -> H.HalogenM AppState Action Slots o m Unit
handleOverwritePreset presetId pedalId = do
  now <- liftEffect Storage.nowISO
  st <- H.get
  let mPs = Map.lookup pedalId st.engine
      values = case mPs of
        Just ps -> ps.values
        Nothing -> Map.empty
      info = case mPs of
        Just ps -> ps.info
        Nothing -> Map.empty
  H.modify_ \s -> s { presets = map (\p ->
    if p.id == presetId then p { values = values, info = info, modified = now } else p
  ) s.presets }
  persistPresets

handleDeletePreset :: forall o m. MonadAff m => PresetId -> H.HalogenM AppState Action Slots o m Unit
handleDeletePreset presetId = do
  H.modify_ \s -> s { presets = Array.filter (\p -> p.id /= presetId) s.presets }
  persistPresets

handleAssignSlot :: forall o m. MonadAff m => PresetId -> ProgramNumber -> H.HalogenM AppState Action Slots o m Unit
handleAssignSlot presetId pn = do
  now <- liftEffect Storage.nowISO
  H.modify_ \s -> s { presets = map (\p ->
    if p.id == presetId then p { savedSlot = Just pn, modified = now } else p
  ) s.presets }
  persistPresets

handleExportPreset :: forall o m. MonadAff m => PedalPreset -> H.HalogenM AppState Action Slots o m Unit
handleExportPreset preset = do
  st <- H.get
  let json = CPreset.presetsToReadableJsonString st.registry [preset]
      filename = preset.name <> ".json"
  liftEffect $ FileIO.downloadJson filename json

handleImportPresets :: forall o m. MonadAff m => Array PedalPreset -> H.HalogenM AppState Action Slots o m Unit
handleImportPresets imported = do
  st <- H.get
  let existingIds = map _.id st.presets
      newPresets = Array.filter (\p -> not (Array.elem p.id existingIds)) imported
  H.modify_ \s -> s { presets = newPresets <> s.presets }
  persistPresets

-- Board CRUD handlers

handleSaveBoard :: forall o m. MonadAff m => { name :: String, notes :: String, pedals :: Map.Map PedalId { presetId :: Maybe String, engage :: EngageState } } -> H.HalogenM AppState Action Slots o m Unit
handleSaveBoard r = do
  uuid <- liftEffect MIDI.randomUUID
  now <- liftEffect Storage.nowISO
  let bp :: BoardPreset
      bp =
        { id: uuid
        , name: r.name
        , description: ""
        , notes: r.notes
        , pedals: r.pedals
        , created: now
        , modified: now
        }
  H.modify_ \s -> s { boardPresets = Array.cons bp s.boardPresets }
  persistBoardPresets

handleUpdateBoard :: forall o m. MonadAff m => PresetId -> { name :: String, notes :: String } -> H.HalogenM AppState Action Slots o m Unit
handleUpdateBoard presetId r = do
  now <- liftEffect Storage.nowISO
  H.modify_ \s -> s { boardPresets = map (\bp ->
    if bp.id == presetId then bp { name = r.name, notes = r.notes, modified = now } else bp
  ) s.boardPresets }
  persistBoardPresets

handleOverwriteBoard :: forall o m. MonadAff m => PresetId -> Map.Map PedalId { presetId :: Maybe String, engage :: EngageState } -> H.HalogenM AppState Action Slots o m Unit
handleOverwriteBoard presetId pedals = do
  now <- liftEffect Storage.nowISO
  H.modify_ \s -> s { boardPresets = map (\bp ->
    if bp.id == presetId then bp { pedals = pedals, modified = now } else bp
  ) s.boardPresets }
  persistBoardPresets

handleDeleteBoard :: forall o m. MonadAff m => PresetId -> H.HalogenM AppState Action Slots o m Unit
handleDeleteBoard presetId = do
  H.modify_ \s -> s { boardPresets = Array.filter (\bp -> bp.id /= presetId) s.boardPresets }
  persistBoardPresets

handleExportBoard :: forall o m. MonadAff m => BoardPreset -> H.HalogenM AppState Action Slots o m Unit
handleExportBoard bp = do
  st <- H.get
  let json = CPreset.boardPresetsToReadableJsonString st.presets [bp]
      filename = bp.name <> ".json"
  liftEffect $ FileIO.downloadJson filename json

handleImportBoards :: forall o m. MonadAff m => Array BoardPreset -> H.HalogenM AppState Action Slots o m Unit
handleImportBoards imported = do
  st <- H.get
  let existingIds = map _.id st.boardPresets
      newBoards = Array.filter (\bp -> not (Array.elem bp.id existingIds)) imported
  H.modify_ \s -> s { boardPresets = newBoards <> s.boardPresets }
  persistBoardPresets

-- MC6 assignment handlers

handleAssignBoardToSwitch :: forall o m. MonadAff m => PresetId -> Int -> H.HalogenM AppState Action Slots o m Unit
handleAssignBoardToSwitch boardId switchIdx = do
  st <- H.get
  let bankNum = st.mc6BoardBankNum
      newAssignment :: MC6Assignment
      newAssignment = { bankNumber: bankNum, switchIndex: switchIdx, boardPresetId: boardId }
      -- Remove any existing assignment for this switch AND any existing assignment for this board in this bank
      filtered = Array.filter (\a -> not
        ((a.bankNumber == bankNum && a.switchIndex == switchIdx) ||
         (a.bankNumber == bankNum && a.boardPresetId == boardId))) st.mc6Assignments
      updated = Array.snoc filtered newAssignment
  H.modify_ _ { mc6Assignments = updated }
  liftEffect $ Storage.saveMC6Assignments updated
  -- Auto-sync: program this switch to MC6
  let mBoard = Array.find (\bp -> bp.id == boardId) st.boardPresets
  syncSwitchToMC6 bankNum switchIdx mBoard

handleUnassignBoard :: forall o m. MonadAff m => PresetId -> H.HalogenM AppState Action Slots o m Unit
handleUnassignBoard boardId = do
  st <- H.get
  -- Find which switch(es) this board was assigned to, for SysEx clearing
  let boardAssignments = Array.filter (\a -> a.boardPresetId == boardId) st.mc6Assignments
      updated = Array.filter (\a -> a.boardPresetId /= boardId) st.mc6Assignments
  H.modify_ _ { mc6Assignments = updated }
  liftEffect $ Storage.saveMC6Assignments updated
  -- Auto-sync: clear each affected switch on MC6
  for_ boardAssignments \a ->
    syncSwitchToMC6 a.bankNumber a.switchIndex Nothing

handleBoardRecallFromMC6 :: forall o m. MonadAff m => Int -> H.HalogenM AppState Action Slots o m Unit
handleBoardRecallFromMC6 ccNum = do
  st <- H.get
  -- ccNum = switchIndex; find assignment across all bank numbers
  case Array.find (\a -> a.switchIndex == ccNum) st.mc6Assignments of
    Just a -> case Array.find (\bp -> bp.id == a.boardPresetId) st.boardPresets of
      Just bp -> do
        liftEffect $ Console.log $ "Board recall from MC6: switch " <> show ccNum <> " -> " <> bp.name
        recallBoard bp
      Nothing -> pure unit
    Nothing -> pure unit

-- | Send SysEx with hex logging
sendSysExLogged :: forall o m. MonadAff m => String -> MIDI.MIDIOutput -> Array Int -> H.HalogenM AppState Action Slots o m Unit
sendSysExLogged label output bytes = do
  liftEffect $ Console.log $ "MC6 SysEx SEND [" <> label <> "]: " <> SysEx.toHexString bytes
  liftEffect $ MIDI.send output bytes

-- | SysEx upload session: connect → start upload → action → complete → disconnect
-- | The MC6 requires the F1=7 upload handshake to accept preset data.
-- | Ideally we'd wait for the MC6's "ready" response; for now we use fixed delays.
withEditorSession :: forall o m. MonadAff m => MIDI.MIDIOutput -> H.HalogenM AppState Action Slots o m Unit -> H.HalogenM AppState Action Slots o m Unit
withEditorSession output action = do
  sendSysExLogged "connect" output SysEx.sysexConnect
  H.liftAff (delay (Milliseconds 500.0))
  sendSysExLogged "start-upload" output SysEx.sysexStartUpload
  -- MC6 responds with "ready for next" (F1=7,F2=0,F3=33); wait for it
  H.liftAff (delay (Milliseconds 500.0))
  action
  H.liftAff (delay (Milliseconds 300.0))
  sendSysExLogged "complete-upload" output SysEx.sysexCompleteUpload
  H.liftAff (delay (Milliseconds 500.0))
  sendSysExLogged "disconnect" output SysEx.sysexDisconnect
  liftEffect $ Console.log "MC6 SysEx: session complete"

-- | Clear all MC6 switches (A-I) in the active board bank:
-- | remove webapp assignments, then send SysEx clears to hardware
handleClearMC6Bank :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
handleClearMC6Bank = do
  st <- H.get
  let bankNum = st.mc6BoardBankNum
      updated = Array.filter (\a -> a.bankNumber /= bankNum) st.mc6Assignments
  H.modify_ _ { mc6Assignments = updated }
  liftEffect $ Storage.saveMC6Assignments updated
  -- SysEx: clear all 9 switches on MC6 hardware
  case st.connections.mc6Output of
    Nothing -> liftEffect $ Console.log "MC6 SysEx: no MC6 output connected (assignments cleared locally)"
    Just output -> do
      liftEffect $ Console.log $ "MC6 SysEx CLEAR: bank " <> show bankNum <> " switches A-I"
      withEditorSession output do
        for_ (Array.range 0 8) \presetNum -> do
          let sysexBytes = SysEx.sysexClearPreset bankNum presetNum
          sendSysExLogged ("clear-" <> show presetNum) output sysexBytes
          H.liftAff (delay (Milliseconds 100.0))

-- | Sync a single switch to MC6 hardware via SysEx.
-- | If Just board: programs the switch with board messages (+ long press bank jump if control bank active).
-- | If Nothing: clears the switch.
-- | Also syncs the active control bank to its MC6 bank.
-- | Silently skips if MC6 output not connected.
syncSwitchToMC6 :: forall o m. MonadAff m => Int -> Int -> Maybe BoardPreset -> H.HalogenM AppState Action Slots o m Unit
syncSwitchToMC6 bankNum switchIdx mBoard = do
  st <- H.get
  let mControlBankNum = do
        idx <- st.activeControlBankIdx
        cb <- Array.index st.controlBanks idx
        pure cb.mc6BankNumber
  case st.connections.mc6Output of
    Nothing -> liftEffect $ Console.log "MC6 SysEx: no MC6 output (skipping sync)"
    Just output -> do
      case mBoard of
        Nothing -> do
          liftEffect $ Console.log $ "MC6 SysEx: clearing switch " <> show switchIdx <> " in bank " <> show bankNum
          withEditorSession output do
            let sysexBytes = SysEx.sysexClearPreset bankNum switchIdx
            sendSysExLogged ("clear-" <> show switchIdx) output sysexBytes
            H.liftAff (delay (Milliseconds 100.0))
        Just bp -> do
          let messages = boardToMC6Messages st bp mControlBankNum
              sysexBytes = SysEx.sysexPresetData bankNum switchIdx (SCU.take 8 bp.name) bp.name false messages
          liftEffect $ Console.log $ "MC6 SysEx: " <> bp.name <> " → switch " <> show switchIdx <> " (" <> show (Array.length messages) <> " messages)"
          withEditorSession output do
            sendSysExLogged ("preset-" <> show switchIdx) output sysexBytes
            H.liftAff (delay (Milliseconds 200.0))
      -- Also sync the control bank to its dedicated MC6 bank
      syncControlBankToMC6

-- | Program all 9 switches of the active control bank to its MC6 bank.
-- | Skips if no control bank is active or MC6 output not connected.
syncControlBankToMC6 :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
syncControlBankToMC6 = do
  st <- H.get
  case st.activeControlBankIdx >>= Array.index st.controlBanks of
    Nothing -> pure unit
    Just cb -> case st.connections.mc6Output of
      Nothing -> pure unit
      Just output -> do
        let presets = ControlBank.controlBankToPresets st.mc6BoardBankNum cb
        liftEffect $ Console.log $ "MC6 SysEx: syncing control bank '" <> cb.name <> "' to MC6 bank " <> show cb.mc6BankNumber
        withEditorSession output do
          for_ presets \p -> do
            let sysexBytes = SysEx.sysexPresetData cb.mc6BankNumber p.switchIndex p.shortName p.longName p.toToggle p.messages
            sendSysExLogged ("ctrl-" <> show p.switchIndex) output sysexBytes
            H.liftAff (delay (Milliseconds 100.0))

-- | Convert a board preset to MC6 messages: PC per pedal + bypass CCs.
-- | Walks the board's pedal entries, resolves PC numbers from saved slots,
-- | and adds engage CCs for bypassed pedals.
-- | When a control bank number is provided, appends a BankJump on LongPressRelease.
boardToMC6Messages :: AppState -> BoardPreset -> Maybe Int -> Array MC6Message
boardToMC6Messages st bp mControlBankNum =
  let entries = Map.toUnfoldable bp.pedals :: Array (Tuple PedalId { presetId :: Maybe PresetId, engage :: EngageState })
      pedalMsgs = Array.concatMap (entryToMessages st) entries
      jumpMsg = case mControlBankNum of
        Nothing -> []
        Just bankNum -> [MC6Msg.bankJumpMessage bankNum ActionLongPressRelease]
      allMsgs = pedalMsgs <> jumpMsg
      indexed = Array.mapWithIndex Tuple allMsgs
  in map (\(Tuple idx msg) -> msg { msgIndex = idx }) indexed
  where
  entryToMessages :: AppState -> Tuple PedalId { presetId :: Maybe PresetId, engage :: EngageState } -> Array MC6Message
  entryToMessages state (Tuple pid entry) =
    case entry.engage of
      EngageNoChange -> [] -- untouched pedal, skip
      _ ->
        let ch = fromMaybe 1 (map _.meta.defaultChannel (CRegistry.findPedal state.registry pid))
            pcMsg = case entry.presetId of
              Nothing -> []
              Just presetId ->
                case Array.find (\p -> p.id == presetId) state.presets of
                  Nothing -> []
                  Just preset -> case preset.savedSlot of
                    Nothing -> []
                    Just slot -> [MC6Msg.pcMessage ch (unProgramNumber slot) ActionPress]
            bypassMsg = case entry.engage of
              EngageOff ->
                case CRegistry.findPedal state.registry pid of
                  Nothing -> []
                  Just def -> case def.engage of
                    SingleEngage cc -> [MC6Msg.ccMessage ch (unCC cc) 0 ActionPress]
                    DualEngage { a, b } ->
                      [ MC6Msg.ccMessage ch (unCC a.cc) 0 ActionPress
                      , MC6Msg.ccMessage ch (unCC b.cc) 0 ActionPress
                      ]
              _ -> []
        in pcMsg <> bypassMsg

-- | Inject board-recall trigger messages into mc6Banks for export
injectBoardTriggers :: Array MC6Assignment -> Array BoardPreset -> Array MC6NativeBank -> Array MC6NativeBank
injectBoardTriggers assignments boards banks = map injectBank banks
  where
  injectBank :: MC6NativeBank -> MC6NativeBank
  injectBank bank =
    let bankAssignments = Array.filter (\a -> a.bankNumber == bank.bankNumber) assignments
    in bank { presets = Array.mapWithIndex (injectPreset bank.bankNumber bankAssignments) bank.presets }

  injectPreset :: Int -> Array MC6Assignment -> Int -> MC6Preset -> MC6Preset
  injectPreset _bankNum bankAssignments idx preset =
    case Array.find (\a -> a.switchIndex == idx) bankAssignments of
      Nothing -> preset
      Just a -> case Array.find (\bp -> bp.id == a.boardPresetId) boards of
        Nothing -> preset
        Just bp -> preset
          { shortName = SCU.take 8 bp.name
          , longName = bp.name
          , messages =
              [ MC6Msg.ccMessage 1 idx 127 ActionPress
              , MC6Msg.ccMessage 1 idx 0 ActionRelease
              ]
          }

-- Export All / Import from File handlers

handleExportAllPresets :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
handleExportAllPresets = do
  st <- H.get
  let json = CPreset.presetsToReadableJsonString st.registry st.presets
  liftEffect $ FileIO.downloadJson "presets-export.json" json

handleExportAllBoards :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
handleExportAllBoards = do
  st <- H.get
  let json = CPreset.boardPresetsToReadableJsonString st.presets st.boardPresets
  liftEffect $ FileIO.downloadJson "boards-export.json" json

handleExportMC6Backup :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
handleExportMC6Backup = do
  text <- H.liftAff $ FileIO.readFileAsText ".json"
  case jsonParser text >>= Backup.decodeBackup of
    Left err -> liftEffect $ Console.log $ "Backup parse error: " <> err
    Right backup -> do
      st <- H.get
      let banksWithAssignments = injectBoardTriggers st.mc6Assignments st.boardPresets st.mc6Banks
          merged = Backup.mergeBanks banksWithAssignments backup
          json = stringify (Backup.encodeBackup merged)
      liftEffect $ FileIO.downloadJson "mc6-backup.json" json

handleImportPresetsFromFile :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
handleImportPresetsFromFile = do
  text <- H.liftAff $ FileIO.readFileAsText ".json"
  case Storage.parsePresets text of
    Nothing -> liftEffect $ Console.log "Import failed: could not parse presets JSON"
    Just imported -> handleImportPresets imported

handleImportBoardsFromFile :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
handleImportBoardsFromFile = do
  text <- H.liftAff $ FileIO.readFileAsText ".json"
  case Storage.parseBoardPresets text of
    Nothing -> liftEffect $ Console.log "Import failed: could not parse board presets JSON"
    Just imported -> handleImportBoards imported

-- Persistence helpers

persistPresets :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
persistPresets = do
  st <- H.get
  liftEffect $ Storage.savePresets (Storage.presetsToJsonString st.presets)

persistBoardPresets :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
persistBoardPresets = do
  st <- H.get
  liftEffect $ Storage.saveBoardPresets (Storage.boardPresetsToJsonString st.boardPresets)

-- Recall helpers

recallPreset :: forall o m. MonadAff m => PedalPreset -> H.HalogenM AppState Action Slots o m Unit
recallPreset preset = do
  st <- H.get
  for_ (Map.lookup preset.pedalId st.engine) \ps ->
    for_ (makeChannel ps.channel) \_ -> do
      let entries = Map.toUnfoldable preset.values :: Array (Tuple CC MidiValue)
      for_ entries \(Tuple cc val) -> do
        handleAction (SetValue preset.pedalId cc val)
        H.liftAff (delay (Milliseconds 5.0))
      -- Restore info values (e.g. dip switches)
      let infoEntries = Map.toUnfoldable preset.info :: Array (Tuple String Int)
      for_ infoEntries \(Tuple key val) ->
        handleAction (SetInfo preset.pedalId key val)

sendPC_ :: forall o m. MonadAff m => PedalId -> ProgramNumber -> H.HalogenM AppState Action Slots o m Unit
sendPC_ pid pn = do
  st <- H.get
  liftEffect $ Console.log $ "sendPC: pedal=" <> show pid <> " pc=" <> show (unProgramNumber pn)
      <> " output=" <> show (map (const "connected") st.connections.pedalOutput)
  for_ st.connections.pedalOutput \output ->
    for_ (Map.lookup pid st.engine >>= \ps -> makeChannel ps.channel) \ch -> do
      liftEffect $ Console.log $ "sendPC: sending ch=" <> show (unChannel ch) <> " pc=" <> show (unProgramNumber pn)
      liftEffect $ MIDI.sendPC output ch pn

recallBoard :: forall o m. MonadAff m => { id :: String, name :: String, description :: String, notes :: String, pedals :: Map.Map PedalId { presetId :: Maybe String, engage :: EngageState }, created :: String, modified :: String } -> H.HalogenM AppState Action Slots o m Unit
recallBoard board = do
  st <- H.get
  let entries = Map.toUnfoldable board.pedals :: Array (Tuple PedalId { presetId :: Maybe String, engage :: EngageState })
  for_ entries \(Tuple pid entry) -> do
    -- Recall preset: PC if saved slot, otherwise stream CCs
    for_ entry.presetId \presetId ->
      for_ (Array.find (\p -> p.id == presetId) st.presets) \preset -> do
        case preset.savedSlot of
          Just slot -> sendPC_ pid slot
          Nothing -> recallPreset preset
        autoEngageIfNeeded preset
    H.liftAff (delay (Milliseconds 50.0))
    -- Engage CCs (explicit board engage state overrides auto-engage)
    sendEngage pid entry.engage

sendEngage :: forall o m. MonadAff m => PedalId -> EngageState -> H.HalogenM AppState Action Slots o m Unit
sendEngage pid engState = case engState of
  EngageNoChange -> pure unit
  _ -> do
    st <- H.get
    for_ (CRegistry.findPedal st.registry pid) \def -> case def.engage of
      SingleEngage cc -> case engState of
        EngageOn  -> handleAction (SetValue pid cc (unsafeMidiValue 127))
        EngageOff -> handleAction (SetValue pid cc (unsafeMidiValue 0))
        _ -> pure unit
      DualEngage { a, b } -> case engState of
        EngageOn  -> do handleAction (SetValue pid a.cc (unsafeMidiValue 127))
                        handleAction (SetValue pid b.cc (unsafeMidiValue 127))
        EngageOff -> do handleAction (SetValue pid a.cc (unsafeMidiValue 0))
                        handleAction (SetValue pid b.cc (unsafeMidiValue 0))
        EngageA   -> do handleAction (SetValue pid a.cc (unsafeMidiValue 127))
                        handleAction (SetValue pid b.cc (unsafeMidiValue 0))
        EngageB   -> do handleAction (SetValue pid a.cc (unsafeMidiValue 0))
                        handleAction (SetValue pid b.cc (unsafeMidiValue 127))
        _ -> pure unit

-- Auto-engage: if a preset doesn't contain any engage CCs, send EngageOn
autoEngageIfNeeded :: forall o m. MonadAff m => PedalPreset -> H.HalogenM AppState Action Slots o m Unit
autoEngageIfNeeded preset = do
  st <- H.get
  for_ (CRegistry.findPedal st.registry preset.pedalId) \def ->
    let ccs = engageCCs def.engage
    in unless (any (\cc -> Map.member cc preset.values) ccs) do
         sendEngage preset.pedalId EngageOn

-- Twister message handlers

handleEncoderTurn :: forall o m. MonadAff m => Int -> Int -> H.HalogenM AppState Action Slots o m Unit
handleEncoderTurn idx val = do
  st <- H.get
  case st.focusPedalId of
    Nothing -> pure unit
    Just pid -> case CRegistry.findPedal st.registry pid of
      Nothing -> pure unit
      Just def -> case Map.lookup pid st.engine of
        Nothing -> pure unit
        Just ps -> case Twister.handleEncoder idx val def ps of
          Nothing -> pure unit
          Just result -> do
            H.modify_ _ { suppressTwister = true }
            handleAction (SetValue pid result.cc result.value)
            H.modify_ _ { suppressTwister = false }
            for_ result.ringSnap \snap ->
              sendRingPosition idx snap

handleEncoderPress :: forall o m. MonadAff m => Int -> H.HalogenM AppState Action Slots o m Unit
handleEncoderPress idx = do
  st <- H.get
  case st.focusPedalId of
    Nothing -> pure unit
    Just pid -> case CRegistry.findPedal st.registry pid of
      Nothing -> pure unit
      Just def -> case Map.lookup pid st.engine of
        Nothing -> pure unit
        Just ps -> case Twister.handleButton idx def ps of
          Nothing -> pure unit
          Just changes -> do
            H.modify_ _ { suppressTwister = true }
            for_ changes \change ->
              handleAction (SetValue pid change.cc change.value)
            H.modify_ _ { suppressTwister = false }
            sendAllLEDs pid

handleTwisterSideButton :: forall o m. MonadAff m => SideBtn -> H.HalogenM AppState Action Slots o m Unit
handleTwisterSideButton btn = do
  st <- H.get
  case btn of
    RefreshLEDs ->
      if st.loopyTwisterActive
        then sendLoopyLEDs
        else for_ st.focusPedalId sendAllLEDs
    PrevPedal -> do
      H.modify_ _ { loopyTwisterActive = false }
      let newFocus = Twister.handleSideButtonPrev st.focusPedalId st.cardOrder
      H.modify_ _ { focusPedalId = newFocus }
      case newFocus of
        Nothing -> dimAllLEDs
        Just pid -> sendAllLEDs pid
    NextPedal -> do
      H.modify_ _ { loopyTwisterActive = false }
      let newFocus = Twister.handleSideButton st.focusPedalId st.cardOrder
      H.modify_ _ { focusPedalId = newFocus }
      case newFocus of
        Nothing -> dimAllLEDs
        Just pid -> sendAllLEDs pid

-- LED feedback helpers

sendRingPosition :: forall o m. MonadAff m => Int -> Int -> H.HalogenM AppState Action Slots o m Unit
sendRingPosition idx val = do
  st <- H.get
  for_ st.connections.twisterOutput \out ->
    liftEffect $ MIDI.send out [ 0xB0, idx, val ]

sendRGBColor :: forall o m. MonadAff m => Int -> Int -> H.HalogenM AppState Action Slots o m Unit
sendRGBColor idx hue = do
  st <- H.get
  for_ st.connections.twisterOutput \out ->
    liftEffect $ MIDI.send out [ 0xB1, idx, hue ]

sendAllLEDs :: forall o m. MonadAff m => PedalId -> H.HalogenM AppState Action Slots o m Unit
sendAllLEDs pid = do
  st <- H.get
  case CRegistry.findPedal st.registry pid of
    Nothing -> pure unit
    Just def -> case Map.lookup pid st.engine of
      Nothing -> pure unit
      Just ps -> do
        let leds = Twister.computeAllLEDs def ps
        for_ leds \led -> do
          sendRGBColor led.index led.hue
          sendRingPosition led.index led.ring

dimAllLEDs :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
dimAllLEDs = for_ (Array.range 0 15) \i -> do
  sendRGBColor i 0
  sendRingPosition i 0

-- LoopyPro momentary CC: send 127, wait 50ms, send 0 (channel 16)
sendMomentaryLoopy :: forall o m. MonadAff m => CC -> H.HalogenM AppState Action Slots o m Unit
sendMomentaryLoopy ccNum = do
  st <- H.get
  liftEffect $ Console.log $ "sendMomentaryLoopy: CC " <> show (unCC ccNum) <> " output=" <> show (map (const "connected") st.connections.loopyOutput)
  for_ st.connections.loopyOutput \output ->
    for_ (makeChannel 16) \ch -> do
      liftEffect $ MIDI.sendCC output ch ccNum (unsafeMidiValue 127)
      H.liftAff (delay (Milliseconds 50.0))
      liftEffect $ MIDI.sendCC output ch ccNum (unsafeMidiValue 0)

-- | Send a sustained CC to LoopyPro (channel 16)
sendLoopyCC :: forall o m. MonadAff m => CC -> Int -> H.HalogenM AppState Action Slots o m Unit
sendLoopyCC ccNum val = do
  st <- H.get
  for_ st.connections.loopyOutput \output ->
    for_ (makeChannel 16) \ch -> do
      liftEffect $ Console.log $ "sendLoopyCC: CC " <> show (unCC ccNum) <> " val=" <> show val
      liftEffect $ MIDI.sendCC output ch ccNum (unsafeMidiValue val)

-- | Helper to update a single loop's state in the array
updateLoopState :: forall o m. MonadAff m => Int -> (Loopy.LoopState -> Loopy.LoopState) -> H.HalogenM AppState Action Slots o m Unit
updateLoopState loopIdx f =
  H.modify_ \st -> st
    { loopyLoopStates = fromMaybe st.loopyLoopStates (Array.modifyAt loopIdx f st.loopyLoopStates) }

-- | Track phase transitions from a LoopyAction on the selected loop
trackLoopyAction' :: forall o m. MonadAff m => Int -> Loopy.LoopyAction -> H.HalogenM AppState Action Slots o m Unit
trackLoopyAction' loopIdx action = do
  st <- H.get
  let clipSettings = fromMaybe Loopy.defaultClipSettings
        (Array.index st.loopyClipSettings loopIdx)
  case action of
    Loopy.LoopySolo -> updateLoopState loopIdx \ls -> ls { soloed = not ls.soloed }
    _ -> updateLoopState loopIdx \ls ->
      ls { phase = Loopy.transition clipSettings ls.phase action }

-- | Track phase transitions from a CC send on the selected loop
trackLoopyAction :: forall o m. MonadAff m => Int -> CC -> H.HalogenM AppState Action Slots o m Unit
trackLoopyAction loopIdx cc =
  case Loopy.ccToAction cc of
    Just action -> trackLoopyAction' loopIdx action
    Nothing -> pure unit

-- | Handle Twister messages in Loopy mode
handleLoopyTwisterMsg :: forall o m. MonadAff m => TwisterMsg -> H.HalogenM AppState Action Slots o m Unit
handleLoopyTwisterMsg = case _ of
  EncoderTurn idx val
    | idx < 8 -> do
        -- Top encoders: rotate = pan for the loop at this position
        let loopIdx = Loopy.encoderToLoop idx
        sendLoopyCC (Loopy.panCC (Loopy.LoopIndex loopIdx)) val
        sendRingPosition idx val
    | otherwise -> do
        -- Bottom encoders: parameter for selected loop
        st <- H.get
        when (st.loopySelectedLoop >= 0) do
          let paramIdx = idx - 8
              cfg = Loopy.loopConfigBank
              loopIdx = st.loopySelectedLoop
          case Array.index cfg.params paramIdx of
            Just (Loopy.LoopyParamContinuous { cc })
              -- Enc 8 (paramIdx 0) = Volume: use per-loop CC
              | paramIdx == 0 -> do
                  sendLoopyCC (Loopy.volumeCC (Loopy.LoopIndex loopIdx)) val
                  sendRingPosition idx val
                  updateLoopState loopIdx (_ { volume = val })
              | otherwise -> do
                  sendLoopyCC cc val
                  sendRingPosition idx val
            _ -> pure unit

  EncoderPress idx
    | idx < 8 -> do
        st <- H.get
        let loopIdx = Loopy.encoderToLoop idx
        if st.loopySelectedLoop == loopIdx
          then do
            -- Local-only deselect: no MIDI, just hide parameter UI
            H.modify_ _ { loopySelectedLoop = -1 }
            -- Dim bottom encoders
            for_ (Array.range 8 15) \i -> do
              sendRGBColor i 0
              sendRingPosition i 0
          else do
            -- Select loop
            H.modify_ _ { loopySelectedLoop = loopIdx }
            sendMomentaryLoopy (Loopy.selectCC (Loopy.LoopIndex loopIdx))
            -- Light up bottom encoders
            let cfg = Loopy.loopConfigBank
            for_ (Array.range 8 15) \i ->
              sendRGBColor i cfg.paramHue
    | otherwise -> do
        -- Bottom encoder press: toggle/momentary params (only when a loop is selected)
        st <- H.get
        when (st.loopySelectedLoop >= 0) do
          let paramIdx = idx - 8
              cfg = Loopy.loopConfigBank
          case Array.index cfg.params paramIdx of
            Just (Loopy.LoopyParamToggle { cc }) -> do
              sendMomentaryLoopy cc
              trackLoopyAction st.loopySelectedLoop cc
            Just (Loopy.LoopyParamMomentary { cc }) -> do
              sendMomentaryLoopy cc
              trackLoopyAction st.loopySelectedLoop cc
            -- Phase lock (shift on FadeIn) and Threshold (shift on FadeOut) are press actions
            Just (Loopy.LoopyParamContinuous { shift: Just shift }) -> do
              sendMomentaryLoopy shift.cc
            _ -> pure unit

  EncoderRelease _ -> pure unit

  SideButton btn -> handleTwisterSideButton btn

-- | Send Loopy-themed LED colors to all 16 Twister encoders
sendLoopyLEDs :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
sendLoopyLEDs = do
  st <- H.get
  liftEffect $ Console.log $ "sendLoopyLEDs: twisterOutput=" <> show (map (const "connected") st.connections.twisterOutput)
  -- Top 8: loop group colors mapped to Twister grid positions
  for_ Loopy.groups \group -> do
    let (Loopy.LoopIndex aLoop) = group.loopA
        (Loopy.LoopIndex bLoop) = group.loopB
        aEnc = Loopy.loopToEncoder aLoop
        bEnc = Loopy.loopToEncoder bLoop
    sendRGBColor aEnc group.color.twisterHue
    sendRGBColor bEnc group.color.twisterHue
    sendRingPosition aEnc 0
    sendRingPosition bEnc 0
  -- Bottom 8: lit when a loop is selected, dark otherwise
  let cfg = Loopy.loopConfigBank
      bottomHue = if st.loopySelectedLoop >= 0 then cfg.paramHue else 0
  for_ (Array.range 8 15) \idx -> do
    sendRGBColor idx bottomHue
    sendRingPosition idx 0
