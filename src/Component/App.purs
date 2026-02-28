module Component.App (component) where

import Prelude

import Component.Boards.View as BoardsView
import Component.Detail.View as DetailView
import Component.Grid.View as GridView
import Component.Header as Header
import Component.Loopy.Panel as LoopyPanel
import Data.Loopy as Loopy
import Data.Array as Array
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
import Engine (AppState, View(..), initAppState, initEngineFromPedals)
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
  | HandleSideGrid GridView.Output
  | HandleLoopy LoopyPanel.Output
  | SelectLoopyOutput String
  | ExportAllPresetsAction
  | ExportAllBoardsAction
  | ImportPresetsFromFileAction
  | ImportBoardsFromFileAction

type Slots =
  ( header :: Header.Slot Unit
  , grid :: GridView.Slot Unit
  , detail :: DetailView.Slot Unit
  , boards :: BoardsView.Slot Unit
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
        , connections: state.connections
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
        FilesView -> renderFilesView
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
              , HH.div [ HP.class_ (H.ClassName "mc6-panel") ]
                  [ HH.div [ HP.class_ (H.ClassName "mc6-header") ]
                      [ HH.span [ HP.class_ (H.ClassName "mc6-title") ] [ HH.text "MC6" ] ]
                  ]
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
            Left err -> liftEffect $ Console.log $ "Controller config: " <> show err
            Right ctrlConfig -> H.modify_ _ { mc6Banks = ctrlConfig.banks }
        -- Load saved state from localStorage (overrides defaults)
        mEngine <- liftEffect Storage.loadEngineState
        case mEngine of
          Just eng -> H.modify_ _ { engine = eng }
          Nothing -> pure unit
        cardOrder <- liftEffect $ Storage.loadCardOrderParsed defaultCardOrder
        H.modify_ _ { cardOrder = cardOrder }
        presets <- liftEffect Storage.loadPresetsParsed
        boardPresets <- liftEffect Storage.loadBoardPresetsParsed
        H.modify_ _ { presets = presets, boardPresets = boardPresets }
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
    Header.PedalOutputChanged portId -> handleAction (SelectPedalOutput portId)
    Header.TwisterInputChanged portId -> handleAction (SelectTwisterInput portId)
    Header.LoopyOutputChanged portId -> handleAction (SelectLoopyOutput portId)

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

  HandleLoopy output -> case output of
    LoopyPanel.LoopSelected idx -> do
      st <- H.get
      if st.loopySelectedLoop == idx
        then do
          -- Deselect: send CC 29 instead of re-sending select
          sendMomentaryLoopy Loopy.deselectCC
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
        updateLoopState i (_ { cleared = true })
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

  SelectLoopyOutput portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mOut <- liftEffect $ MIDI.openOutput access portId
        H.modify_ _ { connections { loopyOutput = mOut, loopyOutputId = Just portId } }

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

-- | Track mute/solo/record state changes from normal-mode CC sends
trackLoopyAction :: forall o m. MonadAff m => Int -> CC -> H.HalogenM AppState Action Slots o m Unit
trackLoopyAction loopIdx cc
  | cc == unsafeCC 23 = updateLoopState loopIdx \ls -> ls { muted = not ls.muted }
  | cc == unsafeCC 24 = updateLoopState loopIdx \ls -> ls { soloed = not ls.soloed }
  | cc == unsafeCC 20 = updateLoopState loopIdx (_ { cleared = false })
  | cc == unsafeCC 22 = updateLoopState loopIdx (_ { cleared = true })
  | otherwise = pure unit

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
            -- Second press on same loop: deselect via CC 29
            H.modify_ _ { loopySelectedLoop = -1 }
            sendMomentaryLoopy Loopy.deselectCC
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
