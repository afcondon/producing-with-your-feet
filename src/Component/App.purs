module Component.App (component) where

import Prelude

import Component.Boards.View as BoardsView
import Component.Controls.View as ControlsView
import Component.Detail.View as DetailView
import Component.Grid.View as GridView
import Component.Pedal.Overview as OverviewView
import Component.Pedal.View as PedalView
import Component.Header as Header
import Data.Array as Array
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Looper as Looper
import Data.Looper.Banks as LoopBanks
import Component.Looper.Slots as Slots
import Data.Looper.Gestures as Gestures
import Data.Looper.Machine as Machine
import Data.JSDate as JSDate
import Data.MC6.Backup as Backup
import Data.MC6.ControlBank (ControlBank)
import Data.MC6.ControlBank as ControlBank
import Data.MC6.Dump as Dump
import Data.MC6.Global as Global
import Data.MC6.Diagnostics as Diagnostics
import Data.MC6.Message as MC6Msg
import Data.MC6.SysEx as SysEx
import Data.MC6.Types (MC6Action(..), MC6NativeBank, MC6Preset)
import Data.MC6.Board as Board
import Data.MC6.Read as Read
import Data.MC6.Settings as Settings
import Data.MC6.Survey as Survey
import Data.Foldable (any, for_, traverse_)
import Data.Traversable (for)
import Data.Map as Map
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Midi (CC, MidiValue, ProgramNumber, makeCC, makeChannel, makeMidiValue, makeProgramNumber, unCC, unChannel, unMidiValue, unProgramNumber, unsafeCC, unsafeMidiValue)
import Data.Pedal (PedalDef, PedalId)
import Pedals.Registry as PsRegistry
import Data.Pedal.Engage (EngageConfig(..), EngageState(..), engageCCs)
import Data.Preset (PedalPreset, BoardPreset, PresetId)
import Data.Preset as Preset
import Data.String as String
import Data.String.CodeUnits (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Twister (SideBtn(..), TwisterEncoder(..), TwisterMsg(..), parseTwisterMsg)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception as Exception
import Config.Decode as Decode
import Data.Either (Either(..))
import Data.String.CodeUnits as SCU
import Engine (AppState, EngineState, MC6Assignment, PedalState, View(..), getValue, initAppState, initEngineFromPedals, pedalsOnChannel)
import Config.Preset as CPreset
import Engine.Storage as Storage
import Engine.Twister as Twister
import Foreign.FileIO as FileIO
import Foreign.FolderBackup as FolderBackup
import Foreign.LooperSocket as LooperSocket
import Foreign.Remote as Remote
import Data.MC6.Wire as Wire
import Foreign.Unload as Unload
import Foreign.WebMIDI as MIDI
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Config.Registry as CRegistry
import Config.Types (MidiRouting)
import Type.Proxy (Proxy(..))
import Web.DOM.Element as Element
import Web.HTML (window)
import Web.HTML.HTMLDocument (body) as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)

data Action
  = Initialize
  | InitializeMIDI MidiRouting
  | RescanMIDI
  | MidiPortChanged MIDI.PortChange
  | SelectMC6Output String
  | PingMC6
  | SetTestCh String
  | SetTestCC String
  | SendTestCC Int
  | ProgramBypassBanks
  | ReadMC6Banks
  | ProgramLooperBank
  | ProgramLoopBanks
  | SetLooperFace Boolean
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
  | HandlePedal PedalView.Output
  | HandleOverview OverviewView.Output
  | SelectMC6Input String
  | MC6MidiReceived (Array Int)
  | ExportAllPresetsAction
  | ExportAllBoardsAction
  | ImportPresetsFromFileAction
  | ImportBoardsFromFileAction
  | DeepReadMC6Banks
  | ExportMC6BackupAction
  | SelectBoardBank Int
  | ClickMC6Switch Int
  | UnassignMC6Switch Int
  | ClearMC6Bank
  | BackupPickFolderAction
  | BackupReconnectAction
  | BackupSaveNowAction
  | BackupDisconnectAction
  | LooperPoll
  | LooperTick
  | LooperCommand String

type Slots =
  ( header :: Header.Slot Unit
  , grid :: GridView.Slot Unit
  , detail :: DetailView.Slot Unit
  , boards :: BoardsView.Slot Unit
  , controls :: ControlsView.Slot Unit
  , sideGrid :: GridView.Slot Unit
  , pedal :: PedalView.Slot Unit
  , overview :: OverviewView.Slot Unit
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
        , overviewActivePedal: state.overviewActivePedal
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
            , baselineStatus: state.baselineStatus
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
        PedalView pid ->
          HH.slot (Proxy :: _ "pedal") unit PedalView.component
            { engine: state.engine
            , pedalId: pid
            , registry: state.registry
            }
            HandlePedal
        OverviewView ->
          HH.slot (Proxy :: _ "overview") unit OverviewView.component
            { engine: state.engine
            , registry: state.registry
            -- The Overview is the board: twelve pedals you set up and leave.
            -- Itajara is a live surface rather than a set of settings, so it
            -- keeps its pill but lives on its own page.
            , cardOrder: Array.filter (not <<< Looper.isItajara) state.cardOrder
            , activePedal: state.overviewActivePedal
            }
            HandleOverview
        ControlsView ->
          HH.slot (Proxy :: _ "controls") unit ControlsView.component
            { controlBanks: state.controlBanks
            , activeControlBankIdx: state.activeControlBankIdx
            , registry: state.registry
            , mc6BoardBankNum: state.mc6BoardBankNum
            , mc6NativeBanks: state.mc6Banks
            , mc6DumpedBanks: state.mc6DumpedBanks
            , mc6BankNames: state.mc6BankNames
            , mc6BankSwitches: state.mc6BankSwitches
            , mc6ReadStatus: state.mc6ReadStatus
            , mc6SessionHeld: isJust state.mc6Held
            , mc6Reading: state.mc6Reading
            , mc6ReadAt: state.mc6ReadAt
            , boardPresets: state.boardPresets
            , presets: state.presets
            , mc6Assignments: state.mc6Assignments
            , globalSwitches: state.globalSwitches
            }
            HandleControls
        FilesView -> renderFilesView state
        LooperView -> renderLooperView state
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
                  , baselineStatus: state.baselineStatus
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
              , controlBankNum: map _.mc6BankNumber
                  (state.activeControlBankIdx >>= Array.index state.controlBanks)
              }
              HandleBoards
          ]
        )
    ]

-- | What the MC6 said it contains.
-- |
-- | Banks are numbered from zero here, as they are on the wire and everywhere
-- | else in this app. Morningstar's editor numbers them from one, and carrying
-- | both numberings side by side — which this table used to do — turned out to be
-- | the thing that produced off-by-one mistakes rather than the thing that
-- | prevented them.
renderMC6Readout :: forall m. MonadAff m => AppState -> H.ComponentHTML Action Slots m
renderMC6Readout state =
  if Map.isEmpty state.mc6BankNames && Map.isEmpty state.mc6BankSwitches
    then case state.mc6ReadStatus of
      Nothing -> HH.text ""
      Just msg -> HH.p [ HP.class_ (H.ClassName "files-description") ] [ HH.text msg ]
    else HH.div [ HP.class_ (H.ClassName "mc6-readout") ]
      [ HH.h3_ [ HH.text "MC6 contents" ]
      , HH.p [ HP.class_ (H.ClassName "files-description") ]
          [ HH.text (fromMaybe "" state.mc6ReadStatus) ]
      , HH.table [ HP.class_ (H.ClassName "mc6-readout-table") ]
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "bank" ]
                  , HH.th_ [ HH.text "name" ]
                  , HH.th_ [ HH.text "switches" ]
                  ]
              ]
          , HH.tbody_ (Array.mapMaybe row (Array.range 0 (Survey.bankCount - 1)))
          ]
      ]
  where
  row n =
    let nm = fromMaybe "" (Map.lookup n state.mc6BankNames)
        sws = fromMaybe [] (Map.lookup n state.mc6BankSwitches)
        used = Array.filter (_ /= "") sws
    in if nm == "" && Array.null used then Nothing
       else Just $ HH.tr_
         [ HH.td_ [ HH.text (show n) ]
         , HH.td_ [ HH.text nm ]
         , HH.td_ [ HH.text (String.joinWith "  " used) ]
         ]

renderFilesView :: forall m. MonadAff m => AppState -> H.ComponentHTML Action Slots m
renderFilesView state =
  HH.div [ HP.class_ (H.ClassName "files-view") ]
    [ HH.p [ HP.class_ (H.ClassName "files-description") ]
        [ HH.text "Your presets are stored in this browser's local storage and will persist as long as the app is served from the same location. To back up your presets, share them, or edit them by hand, you can export to a JSON file and import it later. For more durable storage, connect a backup folder below — the app will write a single JSON envelope (latest.json plus a dated snapshot in history/) that sits inside the Infovore data larder and rides its backup posture." ]
    , renderFolderBackup state
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
            -- The one file here that describes the *device* rather than this
            -- app's own data. The button existed with nothing wired to it.
            [ HH.h3_ [ HH.text "MC6 Device" ]
            , HH.button
                [ HP.class_ (H.ClassName "files-btn")
                , HE.onClick \_ -> ExportMC6BackupAction
                ]
                [ HH.text "Export MC6 Backup" ]
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

-- | Folder backup card: connect / reconnect / save-now / disconnect.
-- | Shown above the existing per-category Export/Import buttons.
renderFolderBackup :: forall m. AppState -> H.ComponentHTML Action Slots m
renderFolderBackup state =
  HH.div [ HP.class_ (H.ClassName "folder-backup") ]
    [ HH.h3_ [ HH.text "Folder Backup" ]
    , case state.backupFolderName of
        Just name ->
          HH.div [ HP.class_ (H.ClassName "folder-backup-row") ]
            [ HH.span [ HP.class_ (H.ClassName "folder-backup-chip connected") ]
                [ HH.text ("connected: " <> name) ]
            , case state.backupLastSaveAt of
                Nothing -> HH.span [ HP.class_ (H.ClassName "folder-backup-muted") ]
                  [ HH.text "no save yet this session" ]
                Just t -> HH.span [ HP.class_ (H.ClassName "folder-backup-muted") ]
                  [ HH.text ("last save: " <> t) ]
            , HH.button
                [ HP.class_ (H.ClassName "files-btn")
                , HE.onClick \_ -> BackupSaveNowAction
                ]
                [ HH.text "Save now" ]
            , HH.button
                [ HP.class_ (H.ClassName "files-btn files-btn-muted")
                , HE.onClick \_ -> BackupDisconnectAction
                ]
                [ HH.text "Disconnect" ]
            ]
        Nothing ->
          HH.div [ HP.class_ (H.ClassName "folder-backup-row") ]
            [ HH.span [ HP.class_ (H.ClassName "folder-backup-chip disconnected") ]
                [ HH.text "no folder connected" ]
            , HH.button
                [ HP.class_ (H.ClassName "files-btn")
                , HE.onClick \_ -> BackupPickFolderAction
                ]
                [ HH.text "Set backup folder…" ]
            , HH.button
                [ HP.class_ (H.ClassName "files-btn files-btn-muted")
                , HE.onClick \_ -> BackupReconnectAction
                ]
                [ HH.text "Reconnect last folder" ]
            ]
    , case state.backupLastError of
        Nothing -> HH.text ""
        Just err -> HH.div [ HP.class_ (H.ClassName "folder-backup-error") ]
          [ HH.text ("error: " <> err) ]
    ]



-- | The looper tab.
-- |
-- | The engine is `itajara/` in this repo — a Rust daemon on cpal holding the
-- | Audio4c directly. This page holds nothing: it renders the snapshot the
-- | daemon pushes, and sends command strings back. Every decision about what a
-- | command means lives in one place, at the other end of the socket.
-- |
-- | Deliberately unfinished. The interface this wants is in DESIGN-LOOPER §12 —
-- | concentric rings sharing a phase pointer, and a column per layer — and the
-- | thesis is that the display should say what the *next press* will do, not
-- | merely what happened. What is here is proof the wire works, and one button.
renderLooperView :: forall m. MonadAff m => AppState -> H.ComponentHTML Action Slots m
renderLooperView state =
  HH.div [ HP.class_ (H.ClassName "looper-view") ]
    [ HH.h2_ [ HH.text "Looper" ]
    , connectionLine
    , audioLine
    , faceToggle
    , case state.looper of
        Just lp | state.looperShowsSlots -> Slots.render lp (LoopBanks.face state.looperBankShown)
        _ -> HH.text ""
    -- What the last press did, in words. Present for refusals as much as for
    -- commands: the machine names every gap it meets rather than swallowing
    -- the press, and a footswitch that silently does nothing is the failure
    -- this whole surface exists to design against.
    , case state.looperLastAction of
        Nothing -> HH.text ""
        Just msg -> HH.p [ HP.class_ (H.ClassName "looper-lastaction") ] [ HH.text msg ]
    , HH.div [ HP.class_ (H.ClassName "looper-columns") ]
        [ HH.div [ HP.class_ (H.ClassName "looper-left") ]
            [ case state.looper of
                Just lp | not state.looperShowsSlots -> HH.div_ [ transport lp, readout lp ]
                _ -> HH.text ""
            , footswitchCard
            ]
        -- The pedal face, on the page it belongs to rather than in the board
        -- grid. Same component the board uses, so the knobs, the drag handling
        -- and the value routing are the ones already in service elsewhere.
        , HH.div [ HP.class_ (H.ClassName "looper-right") ]
            [ HH.slot (Proxy :: _ "pedal") unit PedalView.component
                { engine: state.engine
                , pedalId: Looper.itajaraId
                , registry: state.registry
                }
                HandlePedal
            ]
        ]
    ]
  where
  st = state.looperStatus

  -- Two faces, not two pages. The old transport is the only thing that can
  -- drive the engine by hand, which is exactly what the six-slot display needs
  -- in order to have anything to show while the state machine does not exist
  -- yet — so it stays one click away rather than behind a nav item.
  faceToggle =
    HH.div [ HP.class_ (H.ClassName "looper-face-toggle") ]
      [ tab true "Loops", tab false "Transport" ]

  tab wants label =
    HH.button
      [ HP.class_ (H.ClassName (if state.looperShowsSlots == wants then "face-tab on" else "face-tab"))
      , HE.onClick \_ -> SetLooperFace wants
      ]
      [ HH.text label ]

  -- A connected socket says nothing about whether audio is running: the push
  -- thread reads shared atomics and will serve a confident snapshot from an
  -- engine whose device was unplugged. That failure cost an afternoon of
  -- hunting a MIDI fault, so it gets its own line and says what to do.
  audioLine = case state.looper of
    Just lp | not lp.audioAlive ->
      HH.p [ HP.class_ (H.ClassName "looper-conn down") ]
        [ HH.text $
            if lp.deviceLost
              then "The daemon lost the audio device — reconnecting. Commands will not take effect until it is back."
              else "The daemon is connected but its audio has stopped. Commands will not take effect."
        ]
    Just lp | lp.reopens > 0 ->
      HH.p [ HP.class_ (H.ClassName "looper-muted") ]
        [ HH.text $ "Audio device recovered "
            <> show lp.reopens
            <> (if lp.reopens == 1 then " time" else " times")
            <> " this session."
        ]
    _ -> HH.text ""

  connectionLine =
    HH.p [ HP.class_ (H.ClassName ("looper-conn" <> if st.connected then " ok" else " down")) ]
      [ HH.text $
          if st.connected then "Connected to the daemon."
          else if st.everConnected then "Lost the daemon — retrying."
          else "No daemon. Start it with:  itajara loop --device AUDIO4c --ws"
      ]

  -- Every button here goes through `SetValue` on the Itajara pedal — the same
  -- path a footswitch or a Twister encoder takes. There is deliberately no
  -- shortcut to the socket from the UI: one route in means one place to debug.
  transport lp =
    HH.div [ HP.class_ (H.ClassName "looper-transport") ]
      [ gestureBtn ("looper-btn" <> if lp.recording then " recording" else "")
          (not st.connected) 1 (nextPress lp)
      , gestureBtn "looper-btn small"
          (not st.connected || lp.loopFrames == 0) 2
          (if lp.state == "multiplying" then "End multiply" else "Multiply")
      , gestureBtn "looper-btn small" (not st.connected) 5 "Take"
      , gestureBtn "looper-btn small" (not st.connected || lp.layers == 0) 3 "Undo"
      -- Only offered when there is a length and nothing sitting in it, which is
      -- the only moment it is meaningful and also the only moment it is wanted.
      , gestureBtn "looper-btn small"
          (not st.connected || lp.layers /= 0 || lp.loopFrames == 0) 13 "Forget length"
      , HH.button
          [ HP.class_ (H.ClassName "looper-btn small")
          , HP.disabled (not st.connected)
          , HE.onClick \_ -> SetValue Looper.itajaraId (unsafeCC 81)
                               (unsafeMidiValue (if lp.click then 0 else 127))
          ]
          [ HH.text (if lp.click then "Click off" else "Click on") ]
      ]

  gestureBtn cls disabled ccNum label =
    HH.button
      [ HP.class_ (H.ClassName cls)
      , HP.disabled disabled
      , HE.onClick \_ -> SetValue Looper.itajaraId (unsafeCC ccNum) (unsafeMidiValue 127)
      ]
      [ HH.text label ]

  -- | What the next press does, which is the thing every looper hides.
  nextPress lp = case lp.state of
    "recordingFirst" -> "Close the loop"
    "overdubbing" -> "Finish overdub"
    "multiplying" -> "End multiply"
    "armed" -> "Starting…"
    -- An empty loop with a length is not an overdub, whatever the engine calls
    -- the state. Saying "Overdub" with nothing to overdub onto is what made a
    -- kept grid read as a stuck one.
    _ | lp.loopFrames == 0 -> "Record"
      | lp.layers == 0 -> "Record on the grid"
      | otherwise -> "Overdub"

  readout lp =
    HH.div [ HP.class_ (H.ClassName "looper-readout") ]
      [ phaseBar lp
      , HH.table [ HP.class_ (H.ClassName "docs-table") ]
          [ HH.tbody_
              [ row "State" lp.state
              , row "Layers" (show lp.layers <> " of " <> show lp.maxLayers)
              , row "Loop"
                  ( if lp.loopFrames == 0 then "not set"
                    else fmt2 lp.loopSecs <> " s  (" <> show lp.loopFrames <> " frames)"
                           <> (if lp.layers == 0
                                 then "  \x2014 empty, grid kept for the next take"
                                 else "")
                  )
              , row "Input" (fmt1 lp.inDb <> " dBFS")
              , row "Output" (fmt1 lp.outDb <> " dBFS")
              , row "Alignment"
                  ( if lp.calibrated then "locked, K " <> show lp.k
                    else "waiting for the first input buffer"
                  )
              ]
          ]
      ]

  -- Where we are in the cycle. Crude next to the concentric rings §12 wants,
  -- but it is the one thing a looper must never leave you guessing about.
  phaseBar lp =
    HH.div [ HP.class_ (H.ClassName "looper-phase") ]
      [ HH.div
          [ HP.class_ (H.ClassName "looper-phase-fill")
          , HP.style ("width:" <> show (max 0.0 (min 100.0 (lp.phase * 100.0))) <> "%")
          ]
          []
      ]

  -- Itajara is a pedal now, so its full surface lives on its own Detail page
  -- and any switch can be assigned to any of it. What remains here is the one
  -- thing that page cannot do: put a usable bank on the hardware today.
  footswitchCard =
    HH.div [ HP.class_ (H.ClassName "looper-footswitch") ]
      [ HH.h3_ [ HH.text "Footswitch control" ]
      , HH.p [ HP.class_ (H.ClassName "looper-muted") ]
          [ HH.text $
              "Itajara is a pedal on channel " <> show Looper.itajaraChannel
              <> ", so the MC6 addresses it exactly as it addresses Habit or MOOD — "
              <> "and every control is on its own page, assignable to any switch. "
              <> "This writes a starter transport bank to MC6 bank "
              <> show state.mc6LooperBankNum <> "."
          ]
      , HH.table [ HP.class_ (H.ClassName "docs-table") ]
          [ HH.tbody_ (map bankRow (Looper.looperBank state.mc6LooperBankNum state.mc6BoardBankNum).switches) ]
      , HH.button
          [ HP.class_ (H.ClassName "files-btn")
          , HP.disabled (isNothing state.connections.mc6Output)
          , HE.onClick \_ -> ProgramLooperBank
          ]
          [ HH.text "Program MC6 looper bank" ]
      , loopFamilyCard
      , case state.looperProgramStatus of
          Nothing -> HH.text ""
          Just msg -> HH.p [ HP.class_ (H.ClassName "looper-muted") ] [ HH.text msg ]
      ]

  -- The six-loop machine's own banks, which are a different thing wearing a
  -- similar name to the transport bank above: that one drives one loop through
  -- Itajara's pedal CCs, this one gives the app twelve labelled places to stand
  -- on each of six pages and lets the app decide what standing there means.
  loopFamilyCard =
    HH.div [ HP.class_ (H.ClassName "looper-footswitch") ]
      [ HH.h3_ [ HH.text "Six-loop banks" ]
      , HH.p [ HP.class_ (H.ClassName "looper-muted") ]
          [ HH.text $
              "Six banks from MC6 bank " <> show state.mc6LoopBankBase
              <> ", uploaded once. Every switch sends its own CC on channel "
              <> show LoopBanks.switchChannel
              <> ", so a press says which bank it came from and the app never has "
              <> "to remember which page the board is showing."
          ]
      , HH.table [ HP.class_ (H.ClassName "docs-table") ]
          [ HH.tbody_ (map familyRow (LoopBanks.banks
              { base: state.mc6LoopBankBase, boardBank: state.mc6BoardBankNum })) ]
      , HH.button
          [ HP.class_ (H.ClassName "files-btn")
          , HP.disabled (isNothing state.connections.mc6Output)
          , HE.onClick \_ -> ProgramLoopBanks
          ]
          [ HH.text "Program MC6 loop banks" ]
      ]

  familyRow cb =
    HH.tr_
      [ HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text (show cb.mc6BankNumber) ]
      , HH.td_ [ HH.text cb.name ]
      , HH.td_
          [ HH.text $ Array.intercalate ", "
              (Array.filter (_ /= "") (map _.label cb.switches)) ]
      ]

  -- Switch letters run A–F on the MC6 itself, then G/H/I on the first FS3X.
  bankRow sw =
    if sw.label == "" then HH.text ""
    else HH.tr_
      [ HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text sw.label ]
      , HH.td_ [ HH.text sw.longName ]
      ]

  row label value =
    HH.tr_ [ HH.td [ HP.class_ (H.ClassName "docs-cc") ] [ HH.text label ], HH.td_ [ HH.text value ] ]

  fmt1 n = show (Int.toNumber (Int.round (n * 10.0)) / 10.0)
  fmt2 n = show (Int.toNumber (Int.round (n * 100.0)) / 100.0)


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
            { label: "MC6"
            , description: "Input from the Morningstar MC6 footswitch."
            , selectedId: state.connections.mc6InputId
            , ports: state.connections.availableInputs
            , onChange: SelectMC6Input
            }
        ]
    , HH.div [ HP.class_ (H.ClassName "connect-flow") ]
        [ HH.h3_ [ HH.text "Signal Flow" ]
        , HH.p_
            [ HH.text "Twister/MC6 \x2192 this app \x2192 the pedalboard. "
            , HH.text "Pedal CCs go out a MIDI output to the pedalboard via MC6 MIDI Thru."
            ]
        , connectCard
            { label: "MC6 SysEx Out"
            , description: "Output to the MC6 itself for programming switches over SysEx. Distinct from Pedal MIDI, which passes through the MC6 to the pedals."
            , selectedId: state.connections.mc6OutputId
            , ports: state.connections.availableOutputs
            , onChange: SelectMC6Output
            }
        ]
    , HH.div [ HP.class_ (H.ClassName "midi-test") ]
        [ HH.button
            [ HP.class_ (H.ClassName "files-btn")
            , HE.onClick \_ -> PingMC6
            ]
            [ HH.text "Ping MC6 (SysEx connect)" ]
        , HH.span [ HP.class_ (H.ClassName "midi-test-label") ] [ HH.text " ch " ]
        , HH.input
            [ HP.class_ (H.ClassName "midi-test-num")
            , HP.type_ HP.InputNumber
            , HP.value (show state.testCh)
            , HE.onValueInput SetTestCh
            ]
        , HH.span [ HP.class_ (H.ClassName "midi-test-label") ] [ HH.text " CC " ]
        , HH.input
            [ HP.class_ (H.ClassName "midi-test-num")
            , HP.type_ HP.InputNumber
            , HP.value (show state.testCC)
            , HE.onValueInput SetTestCC
            ]
        , HH.button
            [ HP.class_ (H.ClassName "files-btn")
            , HE.onClick \_ -> SendTestCC 127
            ]
            [ HH.text "Send 127 (down)" ]
        , HH.button
            [ HP.class_ (H.ClassName "files-btn files-btn-muted")
            , HE.onClick \_ -> SendTestCC 0
            ]
            [ HH.text "Send 0 (up)" ]
        , HH.button
            [ HP.class_ (H.ClassName "files-btn")
            , HE.onClick \_ -> ProgramBypassBanks
            ]
            [ HH.text "Program bypass-test banks" ]
        , HH.button
            [ HP.class_ (H.ClassName "files-btn")
            , HE.onClick \_ -> ReadMC6Banks
            ]
            [ HH.text "Read MC6" ]
        , HH.span [ HP.class_ (H.ClassName "midi-test-result") ]
            [ HH.text (case state.midiTest of
                Nothing -> ""
                Just msg -> msg) ]
        ]
    , renderMC6Readout state
    , renderMidiDiagnostics state
    ]
  where
  -- | Distinguish ports that share a name.
  -- |
  -- | iOS reports every Bluetooth MIDI port as, simply, "Bluetooth" - so two
  -- | WIDI transceivers doing entirely different jobs are indistinguishable in
  -- | a picker, and choosing between them is a coin toss you have to verify by
  -- | stomping on something. Where the name repeats, show a tail of the id:
  -- | ugly, but it is the only thing that differs, and it makes the choice
  -- | writable down.
  portLabel ports port =
    let dupes = Array.length (Array.filter (\p -> p.name == port.name) ports)
    in if dupes <= 1 then port.name
       else port.name <> "  ·  " <> suffix port.id
  suffix pid =
    let n = String.length pid
    in if n <= 6 then pid else String.drop (n - 6) pid

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
                  [ HH.text (portLabel ports port) ]
              ) ports
          )
      ]


-- | Raw MIDI state, shown on the connections page.
-- |
-- | Port selection failing is invisible from the outside: a control that will
-- | not stick looks the same whether the change event never fired, or the id
-- | stored fine but matches no port, or the port refuses to open. Guessing
-- | between those from a description costs more than printing them.
renderMidiDiagnostics :: forall m. MonadAff m => AppState -> H.ComponentHTML Action Slots m
renderMidiDiagnostics state =
  HH.details [ HP.class_ (H.ClassName "midi-diagnostics") ]
    [ HH.summary_ [ HH.text "Diagnostics" ]
    , line "MIDI access" (case state.connections.access of
        Nothing -> "NONE - requestMIDIAccess did not succeed"
        Just _ -> "granted")
    , HH.h4_ [ HH.text ("Outputs (" <> show (Array.length state.connections.availableOutputs) <> ")") ]
    , portList state.connections.availableOutputs
    , HH.h4_ [ HH.text ("Inputs (" <> show (Array.length state.connections.availableInputs) <> ")") ]
    , portList state.connections.availableInputs
    , HH.h4_ [ HH.text "Stored selections" ]
    , line "pedalOutputId" (showMb state.connections.pedalOutputId)
    , line "mc6InputId" (showMb state.connections.mc6InputId)
    , line "twisterInputId" (showMb state.connections.twisterInputId)
    , HH.h4_ [ HH.text "Ports actually opened" ]
    , line "pedalOutput" (case state.connections.pedalOutput of
        Nothing -> "not open"
        Just _ -> "open")
    , line "mc6Input" (case state.connections.mc6Input of
        Nothing -> "not open"
        Just _ -> "open")
    , line "mc6Output" (case state.connections.mc6Output of
        Nothing -> "not open"
        Just _ -> "open")
    , line "mc6OutputId" (showMb state.connections.mc6OutputId)
    , line "last MIDI test" (case state.midiTest of
        Nothing -> "(none run)"
        Just m -> m)
    ]
  where
  showMb = case _ of
    Nothing -> "(nothing stored)"
    Just v -> "\"" <> v <> "\""
  line label val =
    HH.div [ HP.class_ (H.ClassName "midi-diag-line") ]
      [ HH.span [ HP.class_ (H.ClassName "midi-diag-key") ] [ HH.text (label <> ": ") ]
      , HH.span_ [ HH.text val ]
      ]
  portList ports =
    if Array.null ports then HH.p_ [ HH.text "(none)" ]
    else HH.div_ (map (\p -> line ("name=\"" <> p.name <> "\"") ("id=\"" <> p.id <> "\"")) ports)

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
        let pedalsWithLayout = map mergeLayout pedals
            registry = CRegistry.mkRegistry pedalsWithLayout rig.slotRanges rig.midiRouting
            defaultEngine = initEngineFromPedals pedalsWithLayout
            defaultCardOrder = map _.meta.id pedalsWithLayout
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
        -- Pull from pwyf-store into the localStorage cache before reading it.
        -- The store is the system of record; the cache is what makes the app
        -- survive the store being unreachable, so a failure here is logged and
        -- shrugged off rather than fatal.
        storeBase <- liftEffect Remote.storeBaseUrl
        snapRes <- H.liftAff (Remote.getSnapshot storeBase)
        case snapRes of
          Right raw -> do
            okHydrate <- liftEffect (Storage.hydrateFromSnapshot raw)
            liftEffect $ Console.log $
              if okHydrate then "pwyf-store: loaded from " <> storeBase
              else "pwyf-store: unreadable snapshot from " <> storeBase <> " - using cache"
          Left errSnap -> liftEffect $ Console.log $
            "pwyf-store unreachable at " <> storeBase <> " (" <> Exception.message errSnap
              <> ") - using cache"
        -- Load saved state from the cache (overrides defaults)
        mEngine <- liftEffect Storage.loadEngineState
        case mEngine of
          Just eng -> H.modify_ _ { engine = reconcileEngine eng defaultEngine }
          Nothing -> pure unit
        cardOrder <- liftEffect $ Storage.loadCardOrderParsed defaultCardOrder
        H.modify_ _ { cardOrder = reconcileOrder cardOrder defaultCardOrder }
        presets <- liftEffect Storage.loadPresetsParsed
        boardPresets <- liftEffect Storage.loadBoardPresetsParsed
        mc6Assignments <- liftEffect Storage.loadMC6AssignmentsParsed
        currentSt <- H.get
        -- Padded on the way in, so pages authored when a bank had nine
        -- switches gain the other three without a migration step.
        controlBanks <- map (map ControlBank.padSwitches)
          (liftEffect $ Storage.loadControlBanksParsed currentSt.controlBanks)
        loadedGlobals <- liftEffect Storage.loadGlobalSwitchesParsed
        legacyOverrides <- liftEffect Storage.loadLegacyOverrides
        -- Two one-time conversions, and at most one of them can apply.
        --
        -- With no globals in the store, the return switch has never been
        -- converted: it used to be substituted in by the compiler, and becomes
        -- an ordinary switch here. With globals present, the store may still
        -- carry per-page overrides from when a global could be refused — those
        -- go now, by dissolving any global that was ever refused into copies.
        let reconciled =
              if Array.null loadedGlobals && not (Array.null controlBanks)
                then Global.migrateReturns currentSt.mc6BoardBankNum controlBanks
                else Global.retireOverrides legacyOverrides loadedGlobals controlBanks
            globalSwitches = reconciled.globals
            banksAfter = reconciled.banks
        when (globalSwitches /= loadedGlobals || banksAfter /= controlBanks
                || not (Array.null (Array.concat legacyOverrides))) do
          liftEffect $ Storage.saveGlobalSwitches globalSwitches
          liftEffect $ Storage.saveControlBanks banksAfter
          liftEffect $ Console.log
            ("Reconciled globals: " <> show (map _.slot globalSwitches))
        -- Whatever the device last said about itself. Reading every bank costs
        -- minutes at the hardware, so the result has to survive a reload or it
        -- is not a baseline, it is a chore.
        dumpedBanks <- liftEffect Storage.loadDumpedBanks
        H.modify_ _ { mc6DumpedBanks = dumpedBanks }
        mDeviceRead <- liftEffect Storage.loadDeviceRead
        for_ mDeviceRead \dr -> H.modify_ _
          { mc6BankNames = dr.names
          , mc6BankSwitches = dr.switches
          , mc6ReadAt = Just dr.readAt
          , mc6ReadStatus = Just
              ("Showing what the MC6 said on " <> String.take 10 dr.readAt
                <> " \x2014 " <> show (Map.size dr.switches) <> " of "
                <> show Survey.bankCount <> " banks with switches.")
          }
        H.modify_ _ { presets = presets, boardPresets = boardPresets, mc6Assignments = mc6Assignments, controlBanks = banksAfter, globalSwitches = globalSwitches }
        st <- H.get
        liftEffect do
          w <- window
          d <- document w
          mb <- HTMLDocument.body d
          for_ mb \b -> case st.view of
            GridView -> Element.setClassName "grid-mode" (HTMLElement.toElement b)
            _ -> pure unit
        -- MIDI initialisation is forked, not awaited.
        --
        -- requestMIDIAccess does not settle until the user answers the
        -- permission prompt, and Halogen serialises a component's actions — so
        -- awaiting it here means an unanswered prompt silently wedges the whole
        -- app. Every later click queues behind Initialize forever, with no
        -- error and no clue: nav dead, pills dead, and the Overview detail
        -- panel never appearing because it needs a pill action first.
        --
        -- It also matters on the iPad, where Safari has no Web MIDI at all and
        -- this can only fail. Forking lets startup finish and MIDI turn up late
        -- or never.
        void $ H.fork $ handleAction (InitializeMIDI rig.midiRouting)
        -- The looper daemon is a separate process that comes and goes; the
        -- socket reconnects itself, so this is fire-and-forget. The poll loop
        -- is forked for the same reason MIDI is: it never returns.
        liftEffect $ LooperSocket.connect LooperSocket.defaultUrl
        void $ H.fork looperPollLoop
        -- Try to silently reconnect the folder-backup handle (IndexedDB). If
        -- the browser needs a fresh gesture, this surfaces Nothing and the
        -- user will see a "Reconnect" affordance in the Files view.
        mBackupFolder <- H.liftAff FolderBackup.attemptReconnect
        H.modify_ _ { backupFolderName = mBackupFolder }

  InitializeMIDI routing -> do
    mAccess <- H.liftAff MIDI.requestMIDIAccess
    outputs <- liftEffect $ MIDI.getOutputs mAccess
    inputs <- liftEffect $ MIDI.getInputs mAccess
    H.modify_ _ { connections { access = Just mAccess
                               , availableOutputs = outputs
                               , availableInputs = inputs } }
    -- Devices arrive late. A WIDI transceiver pairs over Bluetooth well after
    -- the page has loaded, and a USB cable gets plugged in mid-session as a
    -- matter of course on a pedalboard. Enumerating only at startup meant any
    -- of that was invisible until a reload, with no hint that a reload was
    -- what was needed.
    { listener: midiListener, emitter: midiEmitter } <- H.liftEffect HS.create
    void $ H.subscribe midiEmitter
    void $ liftEffect $ MIDI.onStateChange mAccess \change ->
      HS.notify midiListener (MidiPortChanged change)
    -- Auto-select MIDI ports using routing patterns from registry
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
    -- Auto-select MC6 input
    let mMC6In = Array.find (\p -> contains (Pattern routing.mc6Input.match) p.name) inputs
    for_ mMC6In \port ->
      handleAction (SelectMC6Input port.id)
    -- Auto-select MC6 output (SysEx programming) — same device name as input
    when (routing.mc6Input.match /= "") do
      let mMC6Out = Array.find (\p -> contains (Pattern routing.mc6Input.match) p.name) outputs
      for_ mMC6Out \port -> do
        mOut <- liftEffect $ MIDI.openOutput mAccess port.id
        H.modify_ _ { connections { mc6Output = mOut, mc6OutputId = Just port.id } }

  RescanMIDI -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        outputs <- liftEffect $ MIDI.getOutputs access
        inputs <- liftEffect $ MIDI.getInputs access
        H.modify_ _ { connections { availableOutputs = outputs
                                  , availableInputs = inputs } }
        liftEffect $ Console.log $ "MIDI rescan: "
          <> show (Array.length outputs) <> " out, "
          <> show (Array.length inputs) <> " in"

  -- | A port came or went. Put back whatever we had chosen.
  -- |
  -- | Refreshing the dropdowns is not enough, and that was the bug: a port that
  -- | reappears is a *new* `MIDIPort`, so the handle opened before the
  -- | disconnection is dead and delivers nothing without ever saying so. Every
  -- | cycle of the USB bus meant reconnecting by hand, and the only symptom was
  -- | that stomping did nothing — indistinguishable from a wrong bank, a wrong
  -- | channel, or a dead daemon, all of which we chased today.
  MidiPortChanged change -> do
    st0 <- H.get
    -- `statechange` fires for *connection* changes as well as device ones, and
    -- sending on an output opens it — so a burst of thirty SysEx requests fires
    -- thirty of these, each triggering a full port enumeration. Only a port we
    -- have not seen before, or one going away, can actually change the lists.
    let knownPort =
          Array.any (\p -> p.id == change.id)
            (st0.connections.availableInputs <> st0.connections.availableOutputs)
    when (not knownPort || change.state == "disconnected") $ handleAction RescanMIDI
    st <- H.get
    let matches sel = sel == Just change.id
    if change.state == "disconnected"
      then do
        liftEffect $ Console.log $ "MIDI port gone: " <> change.name
        -- Drop the dead handles so the UI stops claiming a connection, and
        -- silence the subscriptions rather than leaving them attached to a
        -- port that no longer exists.
        when (matches st.connections.mc6InputId) do
          for_ st.connections.mc6InputSub H.unsubscribe
          H.modify_ _ { connections { mc6Input = Nothing, mc6InputSub = Nothing } }
        when (matches st.connections.twisterInputId) do
          for_ st.connections.twisterInputSub H.unsubscribe
          H.modify_ _ { connections { twisterInput = Nothing, twisterInputSub = Nothing } }
        when (matches st.connections.pedalOutputId) $
          H.modify_ _ { connections { pedalOutput = Nothing } }
        when (matches st.connections.mc6OutputId) $
          H.modify_ _ { connections { mc6Output = Nothing } }
        when (matches st.connections.twisterOutputId) $
          H.modify_ _ { connections { twisterOutput = Nothing } }
        H.modify_ _ { midiTest = Just (change.name <> " disconnected") }
      else when (change.state == "connected") do
        -- Only worth saying when it is news; see the rescan note above.
        when (not knownPort) $
          liftEffect $ Console.log $ "MIDI port back: " <> change.name
        -- Re-open only what was selected and is currently dead. Re-opening a
        -- working port would tear down its subscription and could drop a
        -- message in the gap.
        when (matches st.connections.mc6InputId && isNothing st.connections.mc6Input) $
          handleAction (SelectMC6Input change.id)
        when (matches st.connections.twisterInputId && isNothing st.connections.twisterInput) $
          handleAction (SelectTwisterInput change.id)
        for_ st.connections.access \access -> do
          when (matches st.connections.pedalOutputId && isNothing st.connections.pedalOutput) do
            mOut <- liftEffect $ MIDI.openOutput access change.id
            H.modify_ _ { connections { pedalOutput = mOut } }
          when (matches st.connections.mc6OutputId && isNothing st.connections.mc6Output) do
            mOut <- liftEffect $ MIDI.openOutput access change.id
            H.modify_ _ { connections { mc6Output = mOut } }
          when (matches st.connections.twisterOutputId && isNothing st.connections.twisterOutput) do
            mOut <- liftEffect $ MIDI.openOutput access change.id
            H.modify_ _ { connections { twisterOutput = mOut } }
        H.modify_ _ { midiTest = Just (change.name <> " reconnected") }

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
    -- Record the outcome of every CC send on the MIDI page. Without it, a pedal
    -- not responding is indistinguishable between "no output selected", "the
    -- pedal has no channel", and "sent correctly and something downstream ate
    -- it" - three very different problems that look identical from the stage.
    -- Itajara is a pedal like any other except in one respect: its transport is
    -- the socket rather than a MIDI port. Branching here rather than upstream is
    -- what lets the assignment UI, board presets, the Twister and the donut view
    -- stay ignorant of the difference.
    if Looper.isItajara pid
      then case Looper.command ccNum val of
        Looper.Ignore -> pure unit
        Looper.NotYetImplemented what -> do
          liftEffect $ Console.log $ "looper: " <> what <> " is not in the engine yet"
          H.modify_ _ { midiTest = Just ("looper: " <> what <> " not implemented") }
        Looper.Send cmd -> do
          ok <- liftEffect $ LooperSocket.send cmd
          H.modify_ _ { midiTest = Just
            ( if ok then "looper: sent " <> show cmd
              else "looper: daemon not connected, dropped " <> show cmd ) }
      else case st.connections.pedalOutput of
        Nothing ->
          H.modify_ _ { midiTest = Just "CC not sent: no Pedal MIDI output selected" }
        Just output -> do
          let mCh = do
                ps <- Map.lookup pid st.engine
                makeChannel ps.channel
          case mCh of
            Nothing ->
              H.modify_ _ { midiTest = Just ("CC not sent: no channel for " <> show pid) }
            Just ch -> do
              liftEffect $ MIDI.sendCC output ch ccNum val
              H.modify_ _ { midiTest = Just
                ("sent CC " <> show (unCC ccNum) <> " val " <> show (unMidiValue val)
                  <> " ch " <> show (unChannel ch) <> " to " <> show pid) }
    -- A gesture is not a state. Left at 127 the pedal's Rec button would stay
    -- lit for the rest of the session, and a board preset would recall a
    -- permanent record command.
    when (Looper.isItajara pid && Looper.isMomentary ccNum) $
      H.modify_ \s -> s
        { engine = Map.update
            (\ps -> Just ps { values = Map.insert ccNum (unsafeMidiValue 0) ps.values })
            pid s.engine
        }
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

  SelectMC6Output portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mOut <- liftEffect $ MIDI.openOutput access portId
        H.modify_ _ { connections { mc6Output = mOut, mc6OutputId = Just portId } }

  -- | Send the Morningstar editor handshake and nothing else.
  -- |
  -- | The cheapest possible proof that bytes leave the app and reach the MC6:
  -- | it is the same frame the official editor opens with, it writes nothing,
  -- | and the device acknowledges it visibly. Worth having as a deliberate
  -- | button because every other path to the MC6 is a side effect of editing
  -- | something, which is a poor thing to be doing while you are still
  -- | establishing whether the cable works.
  PingMC6 -> do
    st <- H.get
    case st.connections.mc6Output of
      Nothing ->
        H.modify_ _ { midiTest = Just "no MC6 SysEx output selected" }
      Just output -> do
        Wire.sendLoose output SysEx.sysexConnect
        H.modify_ _ { midiTest = Just
          "sent the SysEx connect frame - MC6 should show an editor session" }

  SetTestCh v -> H.modify_ _ { testCh = fromMaybe 1 (Int.fromString v) }
  SetTestCC v -> H.modify_ _ { testCC = fromMaybe 1 (Int.fromString v) }

  -- | Send one CC wherever Pedal MIDI points, with no pedal semantics attached.
  -- |
  -- | The MC6's response to incoming MIDI is whatever you configure it to be,
  -- | so a hardcoded 'bank up' would be a guess. This sends an arbitrary
  -- | channel/CC/value instead: set the MC6 to react to something, send it,
  -- | and you have a end-to-end test that does not depend on a pedal being
  -- | patched, powered or in the right mode.
  SendTestCC val -> do
    st <- H.get
    case st.connections.pedalOutput of
      Nothing ->
        H.modify_ _ { midiTest = Just "no Pedal MIDI output selected" }
      Just output -> case makeChannel st.testCh, makeCC st.testCC of
        Just ch, Just ccNum -> do
          liftEffect $ MIDI.sendCC output ch ccNum (unsafeMidiValue val)
          H.modify_ _ { midiTest = Just
            ("sent CC " <> show st.testCC <> " val " <> show val
              <> " ch " <> show st.testCh) }
        _, _ ->
          H.modify_ _ { midiTest = Just "channel must be 1-16, CC 0-127" }

  -- | Ask the MC6 what it contains.
  -- |
  -- | There is no read request. The device volunteers a full dump when an
  -- | editor session opens — controller settings, every bank name in one frame,
  -- | and the twelve switch names of whichever bank it is currently on. So this
  -- | is connect, wait, disconnect, with `MC6MidiReceived` decoding whatever
  -- | turns up in between. Established by sweeping the function-code space and
  -- | finding nothing: see `Data.MC6.Read`.
  -- |
  -- | `Wire.withSession`, not `Wire.withUpload`: asking a question should never
  -- | be able to leave the device anywhere a half-finished write could land,
  -- | which is why the two brackets are separate functions rather than a flag.
  ReadMC6Banks -> do
    st <- H.get
    case st.connections.mc6Output of
      Nothing ->
        H.modify_ _ { mc6ReadStatus = Just "Cannot read: no MC6 SysEx output selected." }
      Just output -> do
        H.modify_ _
          { mc6ReadStatus = Just "Opening a session\x2026"
          , mc6BankNames = Map.empty
          , mc6BankSwitches = Map.empty
          }
        -- The device volunteers everything unasked, so the session body has
        -- nothing to send: it only has to stay open. The dump arrives about a
        -- second after the acknowledgement and takes another moment to finish;
        -- disconnecting early truncates it.
        inFreshSession output \_ ->
          H.liftAff (delay (Milliseconds 2500.0))
        st' <- H.get
        readAt <- liftEffect Storage.nowISO
        when (not (Map.isEmpty st'.mc6BankNames)) do
          liftEffect $ Storage.saveDeviceRead st'.mc6BankNames st'.mc6BankSwitches readAt
          H.modify_ _ { mc6ReadAt = Just readAt }
        -- Says what it got *and* what it did not, because the numbers alone read
        -- as a complete answer. One connect only ever describes the switches of
        -- the bank the device is standing on.
        H.modify_ _ { mc6ReadStatus = Just
          ( if Map.isEmpty st'.mc6BankNames
              then "Session opened but nothing came back \x2014 is the MC6 input connected?"
              else "Read all " <> show (Map.size st'.mc6BankNames) <> " bank names, and the "
                     <> show (Map.size st'.mc6BankSwitches)
                     <> " switch set the MC6 volunteered for the bank it is on. "
                     <> "Use \x201cRead the whole device\x201d for the rest."
          ) }

  -- | Read the entire device: every bank's names, then every preset's messages.
  -- |
  -- | Two requests do the work that a thirty-bank walk used to attempt.
  -- | `sysexRequestPresetNames` returns any bank's switch names with the device
  -- | sitting still, and `sysexRequestFullDump` returns every preset with its
  -- | full message list. Both were read out of Morningstar's own editor bundle
  -- | after `Data.MC6.Read` spent months asserting no read request existed.
  -- |
  -- | Names first because they are cheap and give every bank a label even if the
  -- | dump is interrupted; then the dump, which is the only thing that says what
  -- | a switch actually *does* and so the only thing that makes an adopted page
  -- | reproducible rather than merely plausible.
  DeepReadMC6Banks -> do
    st <- H.get
    case st.connections.mc6Output of
      Nothing ->
        H.modify_ _ { mc6ReadStatus = Just "Cannot read: no MC6 SysEx output selected." }
      Just output -> do
        H.modify_ _
          { mc6Reading = true
          , mc6BankNames = Map.empty
          , mc6BankSwitches = Map.empty
          , mc6DumpedPresets = []
          , mc6DumpFrames = 0
          , mc6DumpDone = false
          , mc6FrameCounts = Map.empty
          , mc6ReadStatus = Just "Opening a session\x2026"
          }
        inFreshSession output \open -> do
          -- Wait for the device to say something before asking it anything: the
          -- session is live once the bank names it volunteers arrive, not when
          -- connect has been written to the port.
          _ <- awaitState 40 (\s -> not (Map.isEmpty s.mc6BankNames))
          Wire.send open SysEx.sysexRequestAllPresetNames
          _ <- awaitState 20 (\s -> Map.size s.mc6BankSwitches >= Survey.bankCount)
          exhaustBanks open 4
          H.modify_ _ { mc6ReadStatus = Just "Asking for every preset\x2026" }
          Wire.send open SysEx.sysexRequestFullDump
          awaitDumpSettled 0 0
          -- If the all-banks request produced nothing, try the single-bank one.
          -- They are neighbouring opcodes and the wrong one answers with silence
          -- rather than an error, so the cheap check is to ask the other way and
          -- see — and one bank of real messages beats none.
          stDump <- H.get
          when (stDump.mc6DumpFrames == 0) do
            H.modify_ _ { mc6ReadStatus = Just
              "No presets came back \x2014 trying the single-bank request\x2026" }
            Wire.send open SysEx.sysexRequestBankDump
            awaitDumpSettled 0 0
        st2 <- H.get
        let got = Map.size st2.mc6BankSwitches
            missing = Array.filter (\b -> not (Map.member b st2.mc6BankSwitches))
                        (Array.range 0 (Survey.bankCount - 1))
        readAt <- liftEffect Storage.nowISO
        let dumped = Dump.presetsToBanks st2.mc6DumpedPresets
        liftEffect $ Storage.saveDeviceRead st2.mc6BankNames st2.mc6BankSwitches readAt
        when (not (Array.null dumped)) $
          liftEffect $ Storage.saveDumpedBanks dumped
        H.modify_ _
          { mc6Reading = false
          , mc6ReadAt = Just readAt
          , mc6DumpedBanks = dumped
          , mc6ReadStatus = Just
              ("Read " <> show got <> " of " <> show Survey.bankCount <> " banks"
                <> (if Array.null missing then "" else "; " <> show missing <> " gave no names")
                -- Frames against frames, banks against banks. It used to
                -- compare decoded presets against the *total* frame count,
                -- which includes thirty bank records  14 so a complete dump
                -- reported 422 of 450 and read as a shortfall.
                <> ", and " <> show st2.mc6DumpFrames <> " of "
                <> show Dump.expectedFrames <> " dump frames giving "
                <> show (Array.length dumped) <> " banks with their messages."
                -- What arrived, by function code. The one thing worth printing
                -- when a request returns less than asked for.
                <> (if Map.isEmpty st2.mc6FrameCounts then "" else
                      "  Frames seen (F1/F2): "
                        <> String.joinWith ", "
                             (map (\(Tuple k n) -> k <> " \xd7 " <> show n)
                               (Map.toUnfoldable st2.mc6FrameCounts :: Array _)))) }

  -- | Write generated bypass-test banks to the MC6.
  -- |
  -- | One switch per pedal, toggling that pedal's bypass, across as many banks
  -- | as twelve pedals need. The point is to make "which pedals are actually
  -- | wired up" a two-minute sweep along a row of footswitches rather than an
  -- | afternoon in a point-and-click editor.
  ProgramBypassBanks -> do
    st <- H.get
    case st.connections.mc6Output of
      Nothing ->
        H.modify_ _ { midiTest = Just "no MC6 SysEx output selected" }
      Just output -> do
        let banks = map (Global.applyGlobals st.globalSwitches)
              (Diagnostics.bypassBanks st.mc6DiagBankNum st.mc6BoardBankNum st.registry)
        r <- uploadBanks "diag" output banks \n ->
          H.modify_ _ { midiTest = Just ("writing bank " <> show n <> "...") }
        invalidateObservation r.written
        H.modify_ _ { midiTest = Just $
          (if Array.null r.written then "wrote nothing"
           else "programmed bypass-test banks " <> commaList r.written)
          <> (if Array.null r.refused then ""
              else "; the MC6 never confirmed moving to " <> commaList r.refused) }

  -- | Write the looper transport to its own MC6 bank.
  -- |
  -- | The bank is generated from the same gesture table the relay reads, so
  -- | the hardware cannot drift away from the handler: if a stomp does the
  -- | wrong thing after this runs, the table is wrong, not the wiring.
  ProgramLooperBank -> do
    st <- H.get
    case st.connections.mc6Output of
      Nothing ->
        H.modify_ _ { looperProgramStatus = Just "No MC6 SysEx output selected — pick one on the Connect page." }
      Just output -> do
        let cb = Looper.looperBank st.mc6LooperBankNum st.mc6BoardBankNum
        H.modify_ _ { looperProgramStatus = Just "Programming…" }
        r <- uploadBanks "looper" output [ cb ] \_ -> pure unit
        -- This writes a bank like any other sync, so what we had read about that
        -- bank is now stale in the same way.
        invalidateObservation r.written
        H.modify_ _ { looperProgramStatus = Just $
          if Array.null r.written then
            "The MC6 never confirmed moving to bank " <> show st.mc6LooperBankNum
              <> ", so nothing was written."
          else "Written to MC6 bank " <> show st.mc6LooperBankNum <> ". Stomp to test." }

  SetLooperFace slots -> H.modify_ _ { looperShowsSlots = slots }

  -- | Write the six-loop machine's whole bank family in one pass.
  -- |
  -- | Six banks and seventy-two presets, which is the better part of a minute
  -- | of SysEx — and exactly why this runs once rather than as the board is
  -- | played (`itajara-in-atlantis` §"The MC6 is a keyboard"). What each loop is
  -- | doing shows on screen; the device only supplies labelled places to stand.
  -- |
  -- | Blank switches are written too, so a bank that used to be something else
  -- | is left with no stragglers from that life.
  ProgramLoopBanks -> do
    st <- H.get
    case st.connections.mc6Output of
      Nothing ->
        H.modify_ _ { looperProgramStatus = Just "No MC6 SysEx output selected — pick one on the Connect page." }
      Just output -> do
        let family = LoopBanks.banks
              { base: st.mc6LoopBankBase, boardBank: st.mc6BoardBankNum }
        r <- uploadBanks "loopbanks" output family \n ->
          H.modify_ _ { looperProgramStatus = Just ("Writing bank " <> show n <> "…") }
        invalidateObservation r.written
        H.modify_ _ { looperProgramStatus = Just $
          (if Array.null r.written then "Wrote nothing."
           else "Written to MC6 banks " <> commaList r.written <> ".")
          <> (if Array.null r.refused then ""
              else " The MC6 never confirmed moving to "
                     <> commaList r.refused <> ", so nothing was written there.")}

  SelectTwisterInput portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mInput <- liftEffect $ MIDI.openInput access portId
        -- Record the choice even when the port will not open, so a
        -- failed open shows as "selected but dead" rather than silently
        -- reverting to "not connected" and looking like the tap was missed.
        H.modify_ _ { connections { twisterInputId = Just portId } }
        -- Tear down any previous subscription first. Re-selecting, or
        -- re-opening after a reconnect, would otherwise stack listeners and
        -- every message would arrive once per open.
        for_ st.connections.twisterInputSub H.unsubscribe
        case mInput of
          Nothing ->
            H.modify_ _ { connections { twisterInput = Nothing, twisterInputSub = Nothing } }
          Just input -> do
            sid <- H.subscribe $ HS.makeEmitter \emit ->
              MIDI.onMessage input \bytes ->
                emit (TwisterMidiReceived bytes)
            H.modify_ _ { connections { twisterInput = Just input
                                      , twisterInputId = Just portId
                                      , twisterInputSub = Just sid } }

  TwisterMidiReceived bytes ->
    case parseTwisterMsg bytes of
      Nothing -> pure unit
      Just msg -> case msg of
        EncoderTurn idx val -> handleEncoderTurn idx val
        EncoderPress idx -> handleEncoderPress idx
        EncoderRelease _ -> pure unit
        SideButton btn -> handleTwisterSideButton btn

  HandleHeader output -> case output of
    Header.ViewChanged view -> handleAction (SetView view)
    Header.PedalPillClicked pid | Looper.isItajara pid ->
      -- The looper's pill is a door rather than a selection: it has no place
      -- in the twelve-cell grid, so the only sensible thing a click can mean
      -- is "take me to it".
      handleAction (SetView LooperView)

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
        OverviewView ->
          H.modify_ \s -> s { overviewActivePedal =
            if s.overviewActivePedal == Just pid
              then Nothing
              else Just pid
          }
        _ -> pure unit

  HandleDetail output -> case output of
    DetailView.ValueChanged pid ccNum val -> handleAction (SetValue pid ccNum val)
    DetailView.MomentarySent pid ccNum val -> handleAction (SendMomentary pid ccNum val)
    DetailView.PedalSelected pid -> handleAction (SetView (DetailView pid))
    DetailView.InfoChanged pid key val -> handleAction (SetInfo pid key val)

  HandlePedal output -> case output of
    PedalView.BackToGrid -> handleAction (SetView OverviewView)
    PedalView.ValueChanged pid cc val -> handleAction (SetValue pid cc val)

  HandleOverview output -> case output of
    OverviewView.BackToGrid -> handleAction (SetView OverviewView)
    OverviewView.ValueChanged pid cc val -> handleAction (SetValue pid cc val)
    -- Deliberately the pill's own action rather than the same field written
    -- twice: "clicking a card does what pressing its pill does" is the
    -- requirement, so it should be the same code and stay so.
    OverviewView.SelectPedal pid ->
      handleAction (HandleHeader (Header.PedalPillClicked pid))

  HandleGrid output -> handleGridOutput output

  HandleSideGrid output -> case output of
    GridView.PedalClicked _ -> pure unit
    GridView.PedalViewClicked _ -> pure unit
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
    GridView.SaveSlotRef r -> handleSaveSlotRef r
    GridView.BaselinePedal pid -> handleBaselinePedal pid

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

  HandleControls output -> case output of
    ControlsView.SaveControlBanks banks mActiveIdx -> do
      H.modify_ _ { controlBanks = banks, activeControlBankIdx = mActiveIdx }
      st <- H.get
      -- localStorage is the cache; the store is where authored pages live.
      liftEffect $ Storage.saveControlBanks st.controlBanks
      liftEffect FolderBackup.scheduleBackup
      pushSnapshot
    ControlsView.SyncControlBankToMC6 cb ->
      syncControlBankToMC6 cb
    ControlsView.SyncAllBanksToMC6 ->
      syncAllBanksToMC6
    ControlsView.ReadMC6 ->
      handleAction ReadMC6Banks
    ControlsView.DeepReadMC6 ->
      handleAction DeepReadMC6Banks
    ControlsView.AssignBoard bankNum switchIdx boardId ->
      handleAssignBoardToSwitch bankNum boardId switchIdx
    ControlsView.UnassignSwitch bankNum switchIdx ->
      handleUnassignSwitch bankNum switchIdx
    -- The device says which bank it is standing on, unasked, whenever it moves.
    -- So this can confirm itself rather than assume: if nothing comes back, the
    -- status line says the request went out and the MC6 did not answer, which is
    -- the difference between "it moved" and "we sent something".
    --
    -- A session is genuinely required, tested rather than assumed. The same
    -- request addressed to device 0x00 — the device number connect and
    -- disconnect use, and so the one a controller command would use — was sent
    -- bare and did nothing: the MC6 neither answered it nor moved, and said it
    -- was still on the bank it had started on when the session opened a moment
    -- later. So Morningstar's editor changing banks without the MC6 visibly
    -- entering edit mode means their web app holds one session open the whole
    -- time it is loaded, not that a session-free form exists.
    --
    -- The remembered bank is dropped before asking, so the confirmation has to
    -- be earned. Otherwise jumping to the bank the device already reported
    -- would satisfy the check without a byte arriving — the same
    -- remembered-value-mistaken-for-observation fault as everywhere else.
    ControlsView.JumpMC6ToBank n -> do
      st <- H.get
      case st.connections.mc6Output of
        Nothing ->
          H.modify_ _ { mc6ReadStatus = Just "Cannot jump: no MC6 SysEx output selected." }
        Just mc6 -> do
          H.modify_ _ { mc6ReadStatus = Just
            (if isJust st.mc6Held then "Asking the MC6\x2026" else "Opening a session\x2026") }
          { live, moved } <- inSession mc6 \open -> do
            -- A held session has already proven itself; a fresh one has not.
            live <- if isJust st.mc6Held then pure true
                    else awaitState 40 (\s -> not (Map.isEmpty s.mc6BankNames))
            H.modify_ _
              { mc6CurrentBank = Nothing
              , mc6ReadStatus = Just ("Asking the MC6 to show bank " <> show n <> "\x2026")
              }
            Wire.send open (SysEx.sysexEditorBankChange n)
            -- Waits on `CurrentBank`, which the device sends the moment it
            -- moves. This used to wait on the switch-names frame, which says
            -- the same thing but arrives after the entire controller-settings
            -- parade — so a jump that had already happened could time out.
            moved <- awaitState 20 (\s -> s.mc6CurrentBank == Just n)
            pure { live, moved }
          when (not moved) $ H.modify_ _ { mc6ReadStatus = Just
            ( if not live then
                "Asked for bank " <> show n <> ", but the MC6 never answered the "
                  <> "session request \x2014 is its input connected?"
              else
                "Asked for bank " <> show n <> " inside a session the MC6 answered, "
                  <> "and it did not report moving. Look at the pedalboard: it may "
                  <> "have moved without saying so."
            ) }
    -- | Hold a session open, and make it safe to hold.
    -- |
    -- | An open editor session is what the MC6 requires before it will change
    -- | bank for us, and opening one per jump costs a connect, a settle and a
    -- | disconnect — fine for a button, useless for anything that has to happen
    -- | while playing.
    -- |
    -- | But a session on its own is *not* safe to hold. With the controller's
    -- | "load preset data into editor using switch press" setting on, which is
    -- | the factory default, the device cannot tell a press meaning "edit this"
    -- | from one meaning "engage this", so while an editor is connected it
    -- | blocks the ambiguous functions — its own bank jump, and MIDI clock.
    -- | Losing clock mid-performance, silently, with nothing about it looking
    -- | like a session problem, is exactly the failure this project keeps
    -- | finding. So the setting goes off as the session opens and back on as it
    -- | closes: the unblocking is scoped to the session rather than left behind
    -- | us on the instrument.
    -- |
    -- | The restore is the weak point, and it is weak in a way worth naming: we
    -- | write the default back rather than what we found, because the `3/33`
    -- | reply that carries the controller settings has not been decoded far
    -- | enough to say which byte this is. It is the one MC6 value the app sets
    -- | without having read it. If you had turned it off yourself, releasing
    -- | the session turns it back on.
    ControlsView.ToggleMC6Session -> do
      st <- H.get
      case st.mc6Held, st.connections.mc6Output of
        Just open, _ -> do
          Wire.send open (SysEx.sysexSwitchPressLoad true)
          Wire.closeSession open
          for_ st.mc6UnloadGuard liftEffect
          H.modify_ _
            { mc6Held = Nothing
            , mc6UnloadGuard = Nothing
            , mc6ReadStatus = Just
                ("Session released. The MC6 is its own again \x2014 and "
                  <> "\x201cload preset data using switch press\x201d is back on.")
            }
        Nothing, Nothing ->
          H.modify_ _ { mc6ReadStatus = Just "Cannot hold a session: no MC6 SysEx output selected." }
        Nothing, Just mc6 -> do
          H.modify_ _
            { mc6ReadStatus = Just "Opening a session to hold\x2026"
            -- Forget what we knew about editor mode, so that what arrives next
            -- is the device answering rather than something we remembered.
            , mc6EditorMode = Nothing
            }
          -- `openSession` disconnects before it connects, so a session left by a
          -- previous page load is closed rather than inherited. The device
          -- answers a real disconnect with `EditorMode false`, so seeing that is
          -- how we learn there *was* one.
          open <- Wire.openSession mc6
          stAfter <- H.get
          let tookOver = stAfter.mc6EditorMode == Just false
          live <- awaitState 40 (\s -> not (Map.isEmpty s.mc6BankNames))
          if not live
            then do
              Wire.closeSession open
              H.modify_ _ { mc6ReadStatus = Just
                ("The MC6 did not answer the session request, so nothing is being "
                  <> "held \x2014 is its input connected?") }
            else do
              Wire.send open (SysEx.sysexSwitchPressLoad false)
              -- A session outlives the tab that opened it, so closing the tab
              -- without closing the session leaves the instrument in a mode
              -- nobody chose. Installed only while one is held.
              guard <- liftEffect $ Unload.onBeforeUnload do
                Wire.send open (SysEx.sysexSwitchPressLoad true)
                Wire.closeSession open
              H.modify_ _
                { mc6Held = Just open
                , mc6UnloadGuard = Just guard
                , mc6ReadStatus = Just
                    ("Session held. Bank jumps from here are instant, and the MC6 "
                      <> "keeps its own bank switching and MIDI clock."
                      <> (if tookOver
                            then "  Note: something already had a session open \x2014 "
                                   <> "Morningstar's editor, most likely \x2014 and this "
                                   <> "took it over."
                            else "")
                      <> "  Release it when you are done editing.")
                }
    ControlsView.SaveGlobalSwitches globals -> do
      H.modify_ _ { globalSwitches = globals }
      liftEffect $ Storage.saveGlobalSwitches globals
      liftEffect FolderBackup.scheduleBackup

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
    liftEffect FolderBackup.scheduleBackup
    pushSnapshot
    syncSwitchToMC6 st.mc6BoardBankNum switchIdx Nothing

  ClearMC6Bank -> handleClearMC6Bank

  BackupPickFolderAction -> do
    mName <- H.liftAff FolderBackup.pickAndSetFolder
    status <- liftEffect FolderBackup.getStatus
    H.modify_ _
      { backupFolderName = mName
      , backupLastError = if status.lastError == "" then Nothing else Just status.lastError
      }

  BackupReconnectAction -> do
    mName <- H.liftAff FolderBackup.reconnectWithPrompt
    status <- liftEffect FolderBackup.getStatus
    H.modify_ _
      { backupFolderName = mName
      , backupLastError = if status.lastError == "" then Nothing else Just status.lastError
      }

  BackupSaveNowAction -> do
    mName <- H.liftAff FolderBackup.saveBackupNow
    status <- liftEffect FolderBackup.getStatus
    H.modify_ _
      { backupFolderName = if status.connected then mName else Nothing
      , backupLastSaveAt = if status.lastSaveAt == "" then Nothing else Just status.lastSaveAt
      , backupLastError = if status.lastError == "" then Nothing else Just status.lastError
      }

  BackupDisconnectAction -> do
    _ <- H.liftAff FolderBackup.disconnectFolder
    H.modify_ _
      { backupFolderName = Nothing
      , backupLastSaveAt = Nothing
      , backupLastError = Nothing
      }

  SelectMC6Input portId -> do
    st <- H.get
    case st.connections.access of
      Nothing -> pure unit
      Just access -> do
        mInput <- liftEffect $ MIDI.openInput access portId
        -- Record the choice even when the port will not open, so a
        -- failed open shows as "selected but dead" rather than silently
        -- reverting to "not connected" and looking like the tap was missed.
        H.modify_ _ { connections { mc6InputId = Just portId } }
        -- See the note in `SelectTwisterInput`: a stacked subscription would
        -- relay every footswitch press twice, which for the looper means
        -- record-then-immediately-close.
        for_ st.connections.mc6InputSub H.unsubscribe
        case mInput of
          Nothing ->
            H.modify_ _ { connections { mc6Input = Nothing, mc6InputSub = Nothing } }
          Just input -> do
            sid <- H.subscribe $ HS.makeEmitter \emit ->
              MIDI.onMessage input \bytes ->
                emit (MC6MidiReceived bytes)
            H.modify_ _ { connections { mc6Input = Just input
                                      , mc6InputId = Just portId
                                      , mc6InputSub = Just sid } }

  MC6MidiReceived bytes -> do
    liftEffect $ Console.log $ "MC6 relay: " <> show bytes
    case bytes of
      -- Itajara's channel. Every other pedal is reached by the MC6 directly
      -- over MIDI, so the app never sees those; the looper has no MIDI
      -- hardware, so its CCs come here and go out over the socket. Relaying
      -- the whole channel rather than a table of gestures means the MC6 can be
      -- reprogrammed freely without touching this file.
      [status, ccNum, val] | status == 0xB0 + Looper.itajaraChannel - 1 ->
        case makeCC ccNum, makeMidiValue val of
          Just c, Just v -> handleAction (SetValue Looper.itajaraId c v)
          _, _ -> pure unit

      -- The app's own switch namespace (`Data.Looper.Banks`). A press here says
      -- which switch on which bank, so nothing has to be inferred from a memory
      -- of the last bank change — and both edges arrive, because the recogniser
      -- times the gap between them.
      [status, ccNum, val] | status == 0xB0 + LoopBanks.switchChannel - 1 ->
        case LoopBanks.decodeSwitch LoopBanks.switchChannel ccNum val of
          Just press -> do
            -- The board says which bank it is on with every press, so the
            -- display never has to guess — including after a bank change made
            -- with a foot, which nothing else would have told us about.
            H.modify_ _ { looperBankShown = Just press.slot }
            t <- liftEffect (JSDate.getTime <$> JSDate.now)
            feedGesture (if press.down then Gestures.Down press t else Gestures.Up press t)
          -- A CC on our channel that is not one of our switches. Worth saying
          -- out loud: it means the board is sending something this app wrote
          -- and no longer understands.
          Nothing -> liftEffect $ Console.log $
            "MC6: CC " <> show ccNum <> " on the switch channel is not a switch."

      -- Channel 1 CC with value 127 = a board-recall footswitch press.
      [status, ccNum, 127] | status == 0xB0 ->
        handleBoardRecallFromMC6 ccNum

      -- Everything else the MC6 sends is aimed at a pedal, and we are only
      -- overhearing it. See DESIGN-v2 §3: the MC6 mirrors its pedal-bound
      -- messages to USB as well as DIN, which is what lets the app stay in step
      -- without standing in the signal path.
      [status, d1, d2] | status >= 0xB0 && status <= 0xBF ->
        observePedalCC (status - 0xB0 + 1) d1 d2

      [status, pc] | status >= 0xC0 && status <= 0xCF ->
        observePedalPC (status - 0xC0 + 1) pc

      -- Any Morningstar frame gets acknowledged first, before anything looks at
      -- what it was. The device streams a dump hundreds of frames long and waits
      -- to be told each one landed, so this is flow control: without it the
      -- first request produced silence and looked like a wrong opcode.
      --
      -- Tallied by function code at the same time, because when a request
      -- returns nothing the useful question is "what *did* arrive", and the
      -- answer should not require a browser console.
      _ | Array.head bytes == Just 0xF0 -> do
        stAck <- H.get
        let f1 = fromMaybe (-1) (Array.index bytes 6)
            f2 = fromMaybe (-1) (Array.index bytes 7)
            cs = fromMaybe 0 (Array.index bytes (Array.length bytes - 2))
        for_ stAck.connections.mc6Output \out -> Wire.sendAck out cs
        H.modify_ \s -> s
          { mc6FrameCounts = Map.insertWith (+) (show f1 <> "/" <> show f2) 1 s.mc6FrameCounts }
        -- Every dump frame counts towards progress, decoded or not. Tying the
        -- progress counter to successful decoding is what let one wrong function
        -- code stop the read after 221 of 450 frames: nothing was landing, so
        -- nothing looked like progress, so it concluded the device had gone quiet
        -- while the device was still talking.
        case Dump.decodeDumpFrame bytes of
          Just (Dump.DumpPresetFrame preset) ->
            H.modify_ \s -> s
              { mc6DumpedPresets = Array.snoc s.mc6DumpedPresets preset
              , mc6DumpFrames = s.mc6DumpFrames + 1
              , mc6ReadStatus = Just
                  ("Reading presets\x2026 " <> show (s.mc6DumpFrames + 1)
                    <> " of " <> show Dump.expectedFrames)
              }
          Just (Dump.DumpBankFrame _) ->
            H.modify_ \s -> s { mc6DumpFrames = s.mc6DumpFrames + 1 }
          Just Dump.DumpStarted ->
            H.modify_ _ { mc6DumpDone = false }
          -- The device says when it has finished. Far better than inferring it
          -- from a gap in the stream, which is a guess that gets slower and less
          -- reliable the more careful you make it.
          Just Dump.DumpFinished ->
            H.modify_ _ { mc6DumpDone = true }
          Nothing -> handleReadReply bytes

      _ -> pure unit

  -- | Pull the newest snapshot the socket is holding.
  -- | Pull the newest snapshot the socket is holding.
  -- |
  -- | The daemon pushes thirty times a second; this reads at ten, because a
  -- | position readout does not need more and Halogen re-rendering at thirty
  -- | would make the whole app feel heavy for the sake of one number.
  -- | Writing state unconditionally at 10 Hz re-renders the whole app ten times
  -- | a second forever, which is how the Overview's footswitches died. Only
  -- | touch state when the daemon actually said something new; with no daemon
  -- | running this settles to zero work.
  -- | Time is an input like any other: a tap becomes a tap by the double-tap
  -- | window expiring, and a transducer only moves when it is fed.
  LooperTick -> do
    t <- liftEffect (JSDate.getTime <$> JSDate.now)
    feedGesture (Gestures.Tick t)

  LooperPoll -> do
    st' <- liftEffect LooperSocket.status
    snap <- liftEffect LooperSocket.latest
    cur <- H.get
    when (cur.looper /= snap || cur.looperStatus /= st') do
      H.modify_ _ { looper = snap, looperStatus = st' }
      -- The daemon's `k` and `m` flip rather than set, so the app's idea of
      -- them could drift from the engine's after one dropped command and never
      -- recover. It reports both in every snapshot, so take its word: for the
      -- things the engine owns, the snapshot is authoritative and pedal state
      -- follows it rather than the other way round.
      for_ snap \lp ->
        H.modify_ \s -> s
          { engine = Map.update
              (\ps -> Just ps { values =
                  Map.insert (unsafeCC 81) (unsafeMidiValue (if lp.click then 127 else 0))
                    (Map.insert (unsafeCC 83)
                       (unsafeMidiValue (if lp.monitor then 127 else 0)) ps.values) })
              Looper.itajaraId
              s.engine
          }

  -- | One command to the daemon, from a button or a footswitch alike.
  -- |
  -- | The daemon has no MIDI of its own by design, so this app is the only
  -- | process talking to the MC6 and the only one deciding what a press means.
  LooperCommand cmd -> do
    ok <- liftEffect $ LooperSocket.send cmd
    unless ok $
      liftEffect $ Console.log $ "looper: not connected, dropped " <> show cmd


-- | Read the socket ten times a second, forever.
-- |
-- | The daemon pushes thirty times a second and the FFI keeps only the newest,
-- | so this cannot fall behind — it just decides how often Halogen re-renders.
-- | Ten is enough for a position bar to look continuous and cheap enough that
-- | the rest of the app does not notice.
-- |
-- | Forked from Initialize, which is the same lesson as MIDI: anything that
-- | never returns must not sit in the action queue, or every later click waits
-- | behind it.
looperPollLoop :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
looperPollLoop = do
  H.liftAff (delay (Milliseconds 100.0))
  handleAction LooperPoll
  handleAction LooperTick
  looperPollLoop

-- Grid output handler (shared by HandleGrid)
handleGridOutput :: forall o m. MonadAff m => GridView.Output -> H.HalogenM AppState Action Slots o m Unit
handleGridOutput = case _ of
  GridView.PedalClicked pid -> handleAction (SetView (DetailView pid))
  GridView.PedalViewClicked pid -> handleAction (SetView (PedalView pid))
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
  GridView.SaveSlotRef r -> handleSaveSlotRef r
  GridView.BaselinePedal pid -> handleBaselinePedal pid

-- | Spacing between CCs in a baseline sweep.
-- |
-- | Deliberately much slower than the 5ms used for preset recall. A sweep is
-- | the one operation whose whole value is that it lands, and these pedals give
-- | nothing back — no CC readback on Strymon, Chase Bliss or Meris — so a
-- | message the pedal was too busy to take is lost silently and leaves exactly
-- | the unknown state the sweep was run to escape. Tens of milliseconds costs
-- | about a second on the largest baseline (MOOD, 45 CCs) and buys the pedal
-- | room to keep up.
baselineSendDelayMs :: Number
baselineSendDelayMs = 25.0

-- | Re-anchor one pedal to its baseline.
-- |
-- | This is the only operation that *establishes* pedal state rather than
-- | tracking it. Everything else in the app — observing the MC6, recalling a
-- | preset, moving a knob — updates a belief that was already there; a sweep
-- | replaces it outright by transmitting every value the definition declares.
-- |
-- | It is deliberately idempotent and safe to repeat. Since no pedal here can
-- | be read back, a second sweep is the only available defence against a
-- | dropped message, and it costs the same as the first.
-- | Note this sends the CCs itself rather than looping over `SetValue`, and
-- | must keep doing so. `SetValue` writes to state, the `delay` between CCs
-- | yields, and Halogen therefore renders the whole tree between every message
-- | — measured at ~270ms per CC on the Boards page, against the 25ms this is
-- | trying to space by. That does not merely make the sweep slow, it makes the
-- | spacing wildly uneven, which is the one property the sweep needs.
-- |
-- | So: transmit in a tight loop with only the delay in it, and write the
-- | belief once at the end. `values = def.baseline` wholesale is exactly right
-- | — being at the baseline is the state we just asserted.
handleBaselinePedal :: forall o m. MonadAff m => PedalId -> H.HalogenM AppState Action Slots o m Unit
handleBaselinePedal pid = do
  st <- H.get
  -- Itajara's "CCs" are looper gestures, not parameters: sweeping them would
  -- fire record, multiply, undo and clear in sequence. Its state lives in the
  -- daemon and is reset there.
  if Looper.isItajara pid
    then H.modify_ _ { baselineStatus = Just "The looper is reset in the daemon, not by a CC sweep." }
    else for_ (CRegistry.findPedal st.registry pid) \def -> do
      let entries = Map.toUnfoldable def.baseline :: Array (Tuple CC MidiValue)
          n = Array.length entries
          mCh = Map.lookup pid st.engine >>= \ps -> makeChannel ps.channel
      case st.connections.pedalOutput, mCh of
        Nothing, _ ->
          H.modify_ _ { baselineStatus = Just "Not sent \x2014 no Pedal MIDI output selected on the MIDI page." }
        _, Nothing ->
          H.modify_ _ { baselineStatus = Just ("Not sent \x2014 no MIDI channel for " <> show pid <> ".") }
        Just output, Just ch -> do
          H.modify_ _ { baselineStatus = Just ("Sweeping — " <> show n <> " CCs…") }
          for_ entries \(Tuple ccNum val) -> do
            liftEffect $ MIDI.sendCC output ch ccNum val
            H.liftAff (delay (Milliseconds baselineSendDelayMs))
          H.modify_ \s -> s
            { engine = Map.update (\ps -> Just ps { values = def.baseline }) pid s.engine
            , baselineStatus = Just
                ("Swept — " <> show n <> " CCs sent on channel " <> show (unChannel ch)
                  <> ". Nothing here reads back, so run it again if you want to be sure.")
            }

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

-- | Keep a numbered slot without capturing what is in it.
-- |
-- | The empty value map is the whole point rather than a gap to fill in later:
-- | the app genuinely does not know what slot 14 sounds like and should not
-- | pretend to. Everything downstream that matters — board entries, the MC6
-- | Program Change path — reads `savedSlot` and never touches `values`.
handleSaveSlotRef :: forall o m. MonadAff m => { pedalId :: PedalId, slot :: ProgramNumber, name :: String } -> H.HalogenM AppState Action Slots o m Unit
handleSaveSlotRef r = do
  uuid <- liftEffect MIDI.randomUUID
  now <- liftEffect Storage.nowISO
  let preset :: PedalPreset
      preset =
        { id: uuid
        , pedalId: r.pedalId
        , name: r.name
        , description: "Lives in the pedal at slot " <> show (unProgramNumber r.slot) <> "; values not captured."
        , notes: ""
        , values: Map.empty
        , info: Map.empty
        , savedSlot: Just r.slot
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

-- | Delete a board, and take it off any footswitch it was on.
-- |
-- | The unassign is not tidiness. An assignment names a board by id, so a
-- | deleted board leaves a switch pointing at nothing — the app would show an
-- | empty switch while the hardware still carried the compiled messages, which
-- | is the worst of both: it looks free, and it does something. Clearing first
-- | also sends the SysEx that empties it on the device.
handleDeleteBoard :: forall o m. MonadAff m => PresetId -> H.HalogenM AppState Action Slots o m Unit
handleDeleteBoard presetId = do
  handleUnassignBoard presetId
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

-- | Put a board on one switch of one bank.
-- |
-- | The bank is a parameter rather than `mc6BoardBankNum` because assignment is
-- | now made from the Controls page, where you are already looking at a
-- | particular bank. A single global "the boards bank" could only ever describe
-- | one page of an instrument that has thirty.
handleAssignBoardToSwitch :: forall o m. MonadAff m => Int -> PresetId -> Int -> H.HalogenM AppState Action Slots o m Unit
handleAssignBoardToSwitch bankNum boardId switchIdx = do
  st <- H.get
  let newAssignment :: MC6Assignment
      newAssignment = { bankNumber: bankNum, switchIndex: switchIdx, boardPresetId: boardId }
      -- Remove any existing assignment for this switch AND any existing assignment for this board in this bank
      filtered = Array.filter (\a -> not
        ((a.bankNumber == bankNum && a.switchIndex == switchIdx) ||
         (a.bankNumber == bankNum && a.boardPresetId == boardId))) st.mc6Assignments
      updated = Array.snoc filtered newAssignment
  H.modify_ _ { mc6Assignments = updated }
  liftEffect $ Storage.saveMC6Assignments updated
  liftEffect FolderBackup.scheduleBackup
  pushSnapshot
  -- Auto-sync: program this switch to MC6
  let mBoard = Array.find (\bp -> bp.id == boardId) st.boardPresets
  syncSwitchToMC6 bankNum switchIdx mBoard

-- | Clear one switch, whatever was on it.
handleUnassignSwitch :: forall o m. MonadAff m => Int -> Int -> H.HalogenM AppState Action Slots o m Unit
handleUnassignSwitch bankNum switchIdx = do
  st <- H.get
  let updated = Array.filter
        (\a -> not (a.bankNumber == bankNum && a.switchIndex == switchIdx))
        st.mc6Assignments
  H.modify_ _ { mc6Assignments = updated }
  liftEffect $ Storage.saveMC6Assignments updated
  liftEffect FolderBackup.scheduleBackup
  pushSnapshot
  syncSwitchToMC6 bankNum switchIdx Nothing

handleUnassignBoard :: forall o m. MonadAff m => PresetId -> H.HalogenM AppState Action Slots o m Unit
handleUnassignBoard boardId = do
  st <- H.get
  -- Find which switch(es) this board was assigned to, for SysEx clearing
  let boardAssignments = Array.filter (\a -> a.boardPresetId == boardId) st.mc6Assignments
      updated = Array.filter (\a -> a.boardPresetId /= boardId) st.mc6Assignments
  H.modify_ _ { mc6Assignments = updated }
  liftEffect $ Storage.saveMC6Assignments updated
  liftEffect FolderBackup.scheduleBackup
  pushSnapshot
  -- Auto-sync: clear each affected switch on MC6
  for_ boardAssignments \a ->
    syncSwitchToMC6 a.bankNumber a.switchIndex Nothing

-- | Overhearing the MC6 talk to a pedal.
-- |
-- | This is the whole of DESIGN-v2 §3 in two functions. Pedal state is a belief
-- | the app cannot verify by asking, so the next best thing is to watch every
-- | change go past. The MC6 sends its pedal messages to USB as well as DIN, so
-- | a footswitch that toggles MOOD's freeze is visible here — and without this,
-- | the app's picture silently diverges the moment a foot touches the board.
-- |
-- | Note what these deliberately do *not* do: they never transmit. The pedal
-- | already got the message directly from the MC6; re-sending it would be at
-- | best redundant and at worst a feedback loop. So they write state and stop,
-- | which is also why they cannot go through `SetValue`.
observePedalCC :: forall o m. MonadAff m => Int -> Int -> Int -> H.HalogenM AppState Action Slots o m Unit
observePedalCC channel ccNum val = do
  st <- H.get
  case makeCC ccNum, makeMidiValue val of
    Just c, Just v -> do
      -- Only write when the value actually changed. Every `modify_` renders the
      -- whole tree, and a board recall arrives as a burst of a dozen messages;
      -- re-rendering for a value we already hold is the same waste that once
      -- made the Overview's footswitches unclickable.
      let targets = Array.filter
            (\pid -> getValue pid c st.engine /= Just v)
            (pedalsOnChannel channel st.engine)
      for_ targets \pid ->
        H.modify_ \s -> s
          { engine = Map.update (\ps -> Just ps { values = Map.insert c v ps.values }) pid s.engine }
      unless (Array.null targets) $
        liftEffect $ Console.log $
          "MC6 observed: ch " <> show channel <> " cc " <> show ccNum <> " = " <> show val
            <> " -> " <> show targets
    _, _ -> pure unit

-- | A Program Change on a pedal's channel means a preset was recalled.
-- |
-- | If we hold a captured preset flashed to that slot we can adopt its values
-- | wholesale, which is the single largest belief update available anywhere in
-- | the app — twelve knobs at once from one message. A slot reference tells us
-- | only the number, so the slot is recorded and the values left alone rather
-- | than pretending to knowledge we never had.
observePedalPC :: forall o m. MonadAff m => Int -> Int -> H.HalogenM AppState Action Slots o m Unit
observePedalPC channel pc = do
  st <- H.get
  for_ (pedalsOnChannel channel st.engine) \pid ->
    for_ (makeProgramNumber pc) \pn -> do
      let match = Array.find
            (\p -> p.pedalId == pid && p.savedSlot == Just pn && not (Preset.isSlotRef p))
            st.presets
      case match of
        Just preset -> do
          H.modify_ \s -> s
            { engine = Map.update
                (\ps -> Just ps { values = preset.values, info = preset.info }) pid s.engine }
          liftEffect $ Console.log $
            "MC6 observed: " <> show pid <> " recalled slot " <> show pc
              <> " (\"" <> preset.name <> "\") - adopted its values"
        Nothing ->
          liftEffect $ Console.log $
            "MC6 observed: " <> show pid <> " recalled slot " <> show pc
              <> " - no captured preset for that slot, values now unknown"

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

-- | Do something in an editor session: the held one if there is one, a fresh
-- | bracket otherwise.
-- |
-- | Every path to the MC6 goes through this rather than `Wire.withSession`
-- | directly, because the two ways of having a session must not overlap. A
-- | bracket opened while one is held would end with a disconnect, silently
-- | closing the held session and leaving `mc6Held` claiming a session that is
-- | gone — the same shape as every other fault in this file, and the reason the
-- | choice is made in one place rather than at each call site.
inSession
  :: forall o m a. MonadAff m
  => MIDI.MIDIOutput
  -> (Wire.Open -> H.HalogenM AppState Action Slots o m a)
  -> H.HalogenM AppState Action Slots o m a
inSession output act = do
  st <- H.get
  case st.mc6Held of
    Just open -> act open
    Nothing -> Wire.withSession output act

-- | Do something in a session that has *just* been opened, even if one is held.
-- |
-- | For the reads, which do not ask the device anything — they rely on what it
-- | volunteers the moment an editor connects. Handed a session that opened
-- | minutes ago they would sit waiting for a dump that already happened, and
-- | report an unresponsive device. So this closes and reopens a held session
-- | rather than borrowing it, and goes on holding afterwards.
-- |
-- | The unblocking survives the cycle because it is a controller *setting*,
-- | stored on the device, not a property of the session.
inFreshSession
  :: forall o m a. MonadAff m
  => MIDI.MIDIOutput
  -> (Wire.Open -> H.HalogenM AppState Action Slots o m a)
  -> H.HalogenM AppState Action Slots o m a
inFreshSession output act = do
  st <- H.get
  case st.mc6Held of
    Nothing -> Wire.withSession output act
    Just held -> do
      Wire.closeSession held
      H.modify_ _ { mc6Held = Nothing }
      open <- Wire.openSession output
      a <- act open
      H.modify_ _ { mc6Held = Just open }
      pure a

-- | The same, for a write. An upload needs a session around it either way; this
-- | only decides whether that session is borrowed or opened.
inUpload
  :: forall o m a. MonadAff m
  => MIDI.MIDIOutput
  -> (Wire.Uploading -> H.HalogenM AppState Action Slots o m a)
  -> H.HalogenM AppState Action Slots o m a
inUpload output act = inSession output \open -> Wire.withUpload open act

-- | Write whole banks, having first made the device show each one.
-- |
-- | **The MC6 ignores the bank number in the preset frame.** An upload lands on
-- | the bank the editor is *currently on*, whatever `sysexPresetData`'s header
-- | says — which was found the only way it could be found, by writing six banks
-- | to 22-27, reading the device back, and finding all six had gone to bank 19
-- | on top of each other, leaving the last one standing. Nothing complained:
-- | seventy-two frames went out, the device took every one, and the app said
-- | "written to banks 22-27".
-- |
-- | It is also why the looper transport bank has always appeared to work. It
-- | was written while the device sat on the bank being looked at, so the labels
-- | duly appeared on the LCD, and "the bank I asked for" and "the bank I was
-- | looking at" were never once distinguished by the evidence.
-- |
-- | So: jump, **wait for the device to say it moved**, then write. And where it
-- | does not say so, write nothing and report the bank as refused — a silent
-- | skip here means overwriting a bank nobody named, which is the exact failure
-- | this whole function exists to have found.
-- |
-- | **A session per bank, not a session per run.** With one session around all
-- | six, the first jump was answered and the other five never were: committing
-- | an upload leaves the editor somewhere that no longer replies to a bank
-- | change. The guard caught it — one bank written, five reported refused, and
-- | a read-back confirming those five were untouched rather than merely
-- | unconfirmed — which is the difference between a slow afternoon and a
-- | silently wrong pedalboard.
-- | Step the recogniser, and act on whatever it decided.
-- |
-- | `Mealy` returns the *next machine* alongside the output, so stepping it is
-- | a state update — which is why the machine lives in `AppState` rather than
-- | in a closure somewhere. The library's shape and Halogen's agree here
-- | without either being bent.
feedGesture
  :: forall o m. MonadAff m
  => Gestures.Event -> H.HalogenM AppState Action Slots o m Unit
feedGesture ev = do
  st <- H.get
  let stepped = Gestures.feed st.looperGestures ev
  H.modify_ _ { looperGestures = fst stepped }
  traverse_ runGesture (snd stepped)

-- | What a gesture means, and then doing it.
-- |
-- | The meaning is a pure function of the gesture and the *daemon's* report of
-- | the loops — this app models no loop state of its own, so there is nothing
-- | here that can fall out of step with the engine.
runGesture
  :: forall o m. MonadAff m
  => Gestures.Gesture -> H.HalogenM AppState Action Slots o m Unit
runGesture g = do
  st <- H.get
  let rig = { loops: maybe [] _.loops st.looper, focus: st.looperFocus }
  followBoard g
  -- How late this command will be, measured from the press rather than
  -- assumed. The daemon spends it where a frame matters (`@ms` in its
  -- dispatch) and strips it everywhere else, so everything can be stamped
  -- without the app having to know which commands care.
  now <- liftEffect (JSDate.getTime <$> JSDate.now)
  let late = max 0.0 (now - gestureAt g)
  traverse_ (runAction late) (Machine.act rig g)

-- | When the foot went down, which is not when the gesture was recognised.
gestureAt :: Gestures.Gesture -> Number
gestureAt = case _ of
  Gestures.Tap _ _ t -> t
  Gestures.DoubleTap _ _ t -> t
  Gestures.Hold _ _ t -> t

-- | Keep track of which bank the board is showing, including the jumps it makes
-- | on its own.
-- |
-- | A press tells us the bank it came *from*; this works out the bank it leaves
-- | the board on, by reading the same jump table the device was programmed with.
-- | Without it the app is permanently one press behind — the long press that
-- | opens the config bank is performed entirely by the MC6, so the app sees a
-- | *loop* switch and hears nothing more until something on the config bank is
-- | pressed. The legend then names the wrong six switches, which is how "J is
-- | Clear" came to be printed under a foot standing on End Stop.
-- |
-- | `Nothing` means the looper is not on screen at all: `< Board` leaves the
-- | family, and a legend that kept describing the loop bank there would be
-- | inventing a board.
followBoard
  :: forall o m. MonadAff m
  => Gestures.Gesture -> H.HalogenM AppState Action Slots o m Unit
followBoard g = do
  let
    Tuple from (Tuple i long) = case g of
      Gestures.Tap slot i' _ -> Tuple slot (Tuple i' false)
      Gestures.DoubleTap slot i' _ -> Tuple slot (Tuple i' false)
      Gestures.Hold slot i' _ -> Tuple slot (Tuple i' true)
  H.modify_ _ { looperBankShown = case LoopBanks.sendsTo from i long of
      Just (LoopBanks.ToSlot to) -> Just to
      Just LoopBanks.ToBoard -> Nothing
      -- Not a navigating switch, so the board stayed where the press came from.
      Nothing -> Just from
    }

runAction
  :: forall o m. MonadAff m
  => Number -> Machine.Action -> H.HalogenM AppState Action Slots o m Unit
runAction late a = do
  liftEffect $ Console.log $ "looper: " <> Machine.describe a
    <> (if late >= 1.0 then " (" <> show (Int.round late) <> " ms late)" else "")
  case a of
    Machine.Command c -> do
      ok <- liftEffect $ LooperSocket.send (c <> "@" <> show (Int.round late))
      note (if ok then Machine.describe a else "no daemon — " <> c <> " went nowhere")
    Machine.Focus i -> H.modify_ _ { looperFocus = i }
    -- **Forked on purpose.** The loop closes and plays on the engine's own
    -- schedule; the bank change is a courtesy that either lands or does not.
    -- Audio must never wait on the display, and opening an editor session takes
    -- the better part of a second.
    Machine.ShowBank slot -> do
      stB <- H.get
      case stB.connections.mc6Output of
        Nothing -> note "no MC6 output — cannot change bank"
        Just out -> do
          note (Machine.describe a)
          void $ H.fork $ inSession out \open ->
            Wire.send open (SysEx.sysexEditorBankChange
              (stB.mc6LoopBankBase + LoopBanks.slotIndex slot))
    Machine.Unavailable why -> note why
    Machine.Handled what -> note what
  where
  note msg = H.modify_ _ { looperLastAction = Just msg }

-- | Bank numbers as prose, for a status line that has to name several.
commaList :: Array Int -> String
commaList = Array.intercalate ", " <<< map show

uploadBanks
  :: forall o m. MonadAff m
  => String
  -> MIDI.MIDIOutput
  -> Array ControlBank
  -> (Int -> H.HalogenM AppState Action Slots o m Unit)
  -> H.HalogenM AppState Action Slots o m { written :: Array Int, refused :: Array Int }
uploadBanks label output cbs note = do
  results <- for cbs \cb -> inFreshSession output \open -> do
    note cb.mc6BankNumber
    -- Cleared first, or `awaitState` would be satisfied by the answer to the
    -- *previous* jump and we would write before the device had moved.
    H.modify_ _ { mc6CurrentBank = Nothing }
    Wire.send open (SysEx.sysexEditorBankChange cb.mc6BankNumber)
    moved <- awaitState 30 (\s -> s.mc6CurrentBank == Just cb.mc6BankNumber)
    when moved $
      Wire.withUpload open \up ->
        for_ (ControlBank.controlBankToPresets cb) \pr -> do
          Wire.sendUpload up $ SysEx.labelled label $
            SysEx.sysexPresetData cb.mc6BankNumber pr.switchIndex
              pr.shortName pr.longName pr.toToggle pr.messages
          H.liftAff (delay (Milliseconds 100.0))
    pure { bank: cb.mc6BankNumber, ok: moved }
  pure
    { written: map _.bank (Array.filter _.ok results)
    , refused: map _.bank (Array.filter (\r -> not r.ok) results)
    }

-- | Wait for the device to say something, rather than assuming it did.
-- |
-- | MIDI has no acknowledgement, so the alternative is a fixed delay long enough
-- | for the worst case — too short sometimes, too long always. Polling state
-- | costs nothing and turns "probably arrived" into "arrived, and here is when".
awaitState
  :: forall o m. MonadAff m
  => Int -> (AppState -> Boolean) -> H.HalogenM AppState Action Slots o m Boolean
awaitState tries done
  | tries <= 0 = pure false
  | otherwise = do
      st <- H.get
      if done st then pure true else do
        H.liftAff (delay (Milliseconds 150.0))
        awaitState (tries - 1) done

-- | Wait for a dump to finish, by waiting for it to stop.
-- |
-- | There is no end-of-dump marker, and the expected count is a per-model number
-- | read out of somebody else's minified code — so treating it as a requirement
-- | would hang on any device that sends 449. Instead: stop when the frame count
-- | has not moved for a while, or early if it reaches the expected total.
-- | Whichever happens first, what arrived is kept.
awaitDumpSettled
  :: forall o m. MonadAff m
  => Int -> Int -> H.HalogenM AppState Action Slots o m Unit
awaitDumpSettled lastCount quietFor = do
  st <- H.get
  let now = st.mc6DumpFrames
  if st.mc6DumpDone || now >= Dump.expectedFrames then pure unit
  else if quietFor >= 12 then
    liftEffect $ Console.log $
      "MC6 dump: went quiet after " <> show now <> " frames (expected "
        <> show Dump.expectedFrames <> ", no completion frame seen)"
  else do
    H.liftAff (delay (Milliseconds 250.0))
    awaitDumpSettled now (if now == lastCount then quietFor + 1 else 0)

-- | Sweep the banks until a whole pass adds nothing.
-- |
-- | A reply may go missing for a reason that clears next time, and stopping
-- | while the last sweep was still finding things would leave holes for no
-- | reason but an arbitrary limit. So the loop ends on *no progress*; the count
-- | is only a backstop against a device that never answers at all.
exhaustBanks
  :: forall o m. MonadAff m
  => Wire.Open -> Int -> H.HalogenM AppState Action Slots o m Unit
exhaustBanks open sweepsLeft
  | sweepsLeft <= 0 = pure unit
  | otherwise = do
      st0 <- H.get
      let allBanks = Array.range 0 (Survey.bankCount - 1)
          missing = Array.filter (\b -> not (Map.member b st0.mc6BankSwitches)) allBanks
          before = Map.size st0.mc6BankSwitches
      if Array.null missing then pure unit else do
        traverse_ (requestOneBank open) missing
        st1 <- H.get
        if Map.size st1.mc6BankSwitches == before
          then liftEffect $ Console.log $
            "MC6 read: a whole sweep added nothing; " <> show (Array.length missing)
              <> " bank(s) are not answering"
          else do
            H.liftAff (delay (Milliseconds 500.0))
            exhaustBanks open (sweepsLeft - 1)

-- | Ask one bank for its switch names, and wait for the answer rather than for
-- | a fixed delay, so the read runs at whatever speed the device manages.
requestOneBank
  :: forall o m. MonadAff m
  => Wire.Open -> Int -> H.HalogenM AppState Action Slots o m Unit
requestOneBank open b = do
  H.modify_ _ { mc6ReadStatus = Just
    ("Asking for bank " <> show b <> " of " <> show (Survey.bankCount - 1) <> "\x2026") }
  Wire.send open (SysEx.sysexRequestPresetNames b)
  _ <- awaitState 20 (\s -> Map.member b s.mc6BankSwitches)
  pure unit

-- | Write every authored page, in one editor session.
-- |
-- | The per-page sync is the wrong shape for anything instrument-wide. A global
-- | occupies its slot on all thirty pages, so changing one and syncing the page
-- | you happen to be looking at leaves twenty-nine holding the previous version
-- | — and nothing says so, because the app's own model is correct and only the
-- | device disagrees.
-- |
-- | Writes pages in full rather than only the slots globals occupy: after
-- | `discard` the changed slots are the ones a global *stopped* filling, so a
-- | globals-only write would miss precisely the pages that need it.
syncAllBanksToMC6 :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
syncAllBanksToMC6 = do
  st <- H.get
  case st.connections.mc6Output of
    Nothing ->
      H.modify_ _ { mc6ReadStatus = Just "Cannot write: no MC6 SysEx output selected." }
    Just output -> do
      let banks = st.controlBanks
          writes = Array.length banks * ControlBank.switchCount
      H.modify_ _ { mc6ReadStatus = Just
        ("Writing " <> show (Array.length banks) <> " pages ("
          <> show writes <> " presets)\x2026") }
      inUpload output \up ->
        for_ banks \cb -> do
          let presets = ControlBank.controlBankToPresets
                          (Global.applyGlobals st.globalSwitches cb)
          for_ presets \p -> do
            Wire.sendUpload up $ SysEx.labelled "all" $
              SysEx.sysexPresetData cb.mc6BankNumber p.switchIndex
                p.shortName p.longName p.toToggle p.messages
            H.liftAff (delay (Milliseconds 100.0))
      invalidateObservation (map _.mc6BankNumber banks)
      H.modify_ _ { mc6ReadStatus = Just
        ("Wrote " <> show (Array.length banks)
          <> " pages to the MC6. Read the device again to confirm what landed.") }

-- | Clear all MC6 switches (A-I) in the active board bank:
-- | remove webapp assignments, then send SysEx clears to hardware
handleClearMC6Bank :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
handleClearMC6Bank = do
  st <- H.get
  let bankNum = st.mc6BoardBankNum
      updated = Array.filter (\a -> a.bankNumber /= bankNum) st.mc6Assignments
  H.modify_ _ { mc6Assignments = updated }
  liftEffect $ Storage.saveMC6Assignments updated
  liftEffect FolderBackup.scheduleBackup
  pushSnapshot
  -- SysEx: clear every switch on MC6 hardware
  case st.connections.mc6Output of
    Nothing -> liftEffect $ Console.log "MC6 SysEx: no MC6 output connected (assignments cleared locally)"
    Just output -> do
      liftEffect $ Console.log $ "MC6 SysEx CLEAR: bank " <> show bankNum <> " all switches"
      inUpload output \up ->
        for_ (Array.range 0 (ControlBank.switchCount - 1)) \presetNum -> do
          Wire.sendUpload up $ SysEx.labelled "clear" $ SysEx.sysexClearPreset bankNum presetNum
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
          inUpload output \up -> do
            Wire.sendUpload up $ SysEx.labelled "clear" $ SysEx.sysexClearPreset bankNum switchIdx
            H.liftAff (delay (Milliseconds 100.0))
        Just bp -> do
          let messages = Board.boardToMC6Messages st.registry st.presets mControlBankNum bp
              n = Array.length messages
          -- Refuse rather than truncate. `sysexPresetData` pads with
          -- `Array.take 16`, so an over-budget board used to program cleanly
          -- and arrive on the hardware missing its last messages — a switch
          -- that silently does most of what you asked is worse than one that
          -- says it cannot.
          if n > Board.messageLimit
            then H.modify_ _ { midiTest = Just
              ("Not programmed: \"" <> bp.name <> "\" needs " <> show n
                <> " messages and an MC6 preset holds " <> show Board.messageLimit
                <> ". Set a pedal to \x2014\x2014 to get under.") }
            else do
              liftEffect $ Console.log $ "MC6 SysEx: " <> bp.name <> " → switch " <> show switchIdx <> " (" <> show n <> " messages)"
              inUpload output \up -> do
                Wire.sendUpload up $ SysEx.labelled "board" $
                  SysEx.sysexPresetData bankNum switchIdx (SCU.take 8 bp.name) bp.name false messages
                H.liftAff (delay (Milliseconds 200.0))
      -- The control page carries the jump back to this board bank, so a board
      -- landing on a switch can change what its return switch should say.
      for_ (st.activeControlBankIdx >>= Array.index st.controlBanks) syncControlBankToMC6

-- | Write one authored page to its MC6 bank.
-- |
-- | Takes the bank rather than resolving `activeControlBankIdx`, which is set by
-- | saving and so could name a different page than the one on screen. Writing a
-- | page to the wrong bank number is unrecoverable from the app's side: the
-- | bank it lands on is overwritten, and nothing on the device says what used
-- | to be there.
syncControlBankToMC6 :: forall o m. MonadAff m => ControlBank.ControlBank -> H.HalogenM AppState Action Slots o m Unit
syncControlBankToMC6 cb = do
  st <- H.get
  case st.connections.mc6Output of
    Nothing -> pure unit
    Just output -> do
      let presets = ControlBank.controlBankToPresets
                      (Global.applyGlobals st.globalSwitches cb)
      liftEffect $ Console.log $ "MC6 SysEx: syncing control bank '" <> cb.name <> "' to MC6 bank " <> show cb.mc6BankNumber
      inUpload output \up ->
        for_ presets \p -> do
          Wire.sendUpload up $ SysEx.labelled "ctrl" $
            SysEx.sysexPresetData cb.mc6BankNumber p.switchIndex p.shortName p.longName p.toToggle p.messages
          H.liftAff (delay (Milliseconds 100.0))
      invalidateObservation [ cb.mc6BankNumber ]

-- | Forget what we had read about the banks we just wrote.
-- |
-- | A write makes the read stale, and stale observation is worse than none: the
-- | survey compares what we authored against what the device last said, so a
-- | page that was synced *because* it differed goes on reporting "device
-- | disagrees" against a snapshot taken before the fix. Every adopted bank
-- | shows it, since globals are written into every page and no dump ever had
-- | them.
-- |
-- | The tempting repair is to copy what we sent into the observed side, and it
-- | is the same laundering that made a five-month-old backup file read as
-- | `Observed`. We did not look; we wrote. So the honest move is to drop the
-- | reading and let provenance fall back to `Authored` — the bank is ours,
-- | unverified, and a re-read is what makes it observed again.
invalidateObservation :: forall o m. MonadAff m => Array Int -> H.HalogenM AppState Action Slots o m Unit
invalidateObservation bankNums = do
  H.modify_ \s -> s
    { mc6BankNames = Map.filterKeys (\k -> not (Array.elem k bankNums)) s.mc6BankNames
    , mc6BankSwitches = Map.filterKeys (\k -> not (Array.elem k bankNums)) s.mc6BankSwitches
    , mc6DumpedBanks = Array.filter (\nb -> not (Array.elem nb.bankNumber bankNums)) s.mc6DumpedBanks
    }
  st <- H.get
  liftEffect $ Storage.saveDeviceRead st.mc6BankNames st.mc6BankSwitches
    (fromMaybe "" st.mc6ReadAt)
  liftEffect $ Storage.saveDumpedBanks st.mc6DumpedBanks

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
              [ MC6Msg.ccMessage Board.boardRecallChannel idx 127 ActionPress
              , MC6Msg.ccMessage Board.boardRecallChannel idx 0 ActionRelease
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

-- | Mirror presets, patches and assignments to pwyf-store.
-- |
-- | One round trip rather than one per record: the app persists whole
-- | collections and the store reconciles, deleting files whose records have
-- | gone. A failure is logged, not raised — the cache write has already
-- | happened, so the edit is not lost, and the next successful push carries it.
-- |
-- | A 409 here is not a network problem: it is the store refusing to empty
-- | itself because this client's state looked empty. Its body says so.
pushSnapshot :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
pushSnapshot = do
  st <- H.get
  base <- liftEffect Remote.storeBaseUrl
  let
    body = Storage.snapshotToJsonString st.presets st.boardPresets st.controlBanks st.mc6Assignments
  res <- H.liftAff (Remote.putSnapshot base body)
  case res of
    Right _ -> pure unit
    Left err -> liftEffect $ Console.log $
      "pwyf-store save failed: " <> Exception.message err

persistPresets :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
persistPresets = do
  st <- H.get
  liftEffect $ Storage.savePresets (Storage.presetsToJsonString st.presets)
  liftEffect FolderBackup.scheduleBackup
  pushSnapshot

persistBoardPresets :: forall o m. MonadAff m => H.HalogenM AppState Action Slots o m Unit
persistBoardPresets = do
  st <- H.get
  liftEffect $ Storage.saveBoardPresets (Storage.boardPresetsToJsonString st.boardPresets)
  liftEffect FolderBackup.scheduleBackup
  pushSnapshot

-- Recall helpers

recallPreset :: forall o m. MonadAff m => PedalPreset -> H.HalogenM AppState Action Slots o m Unit
recallPreset preset
  -- A slot reference has nothing to stream. Without this it would recall by
  -- sending an empty list of CCs, which looks exactly like a working recall
  -- and does nothing at all.
  | Preset.isSlotRef preset = for_ preset.savedSlot (sendPC_ preset.pedalId)
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
    RefreshLEDs -> for_ st.focusPedalId sendAllLEDs
    PrevPedal -> do
      let newFocus = Twister.handleSideButtonPrev st.focusPedalId st.cardOrder
      H.modify_ _ { focusPedalId = newFocus }
      case newFocus of
        Nothing -> dimAllLEDs
        Just pid -> sendAllLEDs pid
    NextPedal -> do
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

-- | Merge layout from PureScript pedal definitions into JSON-decoded pedals.
-- | JSON provides runtime config; layout is PureScript-only (contains ADTs).
-- | Fold what the browser remembered over what the config says, rather than
-- | letting either win outright.
-- |
-- | Storage is a snapshot of a rig that had twelve pedals; the config is the
-- | rig as it is now. Taking storage wholesale means a pedal added to
-- | `rig.json` never appears, and a CC added to an existing pedal reads as
-- | absent — both of which look like the new thing is broken rather than like
-- | the cache is old. Per-CC union costs nothing and removes the whole class
-- | of "clear your local storage and try again".
reconcileEngine :: EngineState -> EngineState -> EngineState
reconcileEngine stored defaults =
  Map.fromFoldable (map merge (Map.toUnfoldable defaults :: Array (Tuple PedalId PedalState)))
  where
  merge (Tuple pid def) = Tuple pid $ case Map.lookup pid stored of
    Nothing -> def
    -- Channel comes from the config, never from the cache: `rig.json` is where
    -- that is decided, and a stale channel is silent and baffling.
    Just s -> def
      { values = Map.union s.values def.values
      , info = Map.union s.info def.info
      }

-- | Keep the order you arranged, drop pedals that no longer exist, and append
-- | ones that have appeared since.
reconcileOrder :: Array PedalId -> Array PedalId -> Array PedalId
reconcileOrder stored defaults =
  Array.filter (\p -> Array.elem p defaults) stored
    <> Array.filter (\p -> not (Array.elem p stored)) defaults

mergeLayout :: PedalDef -> PedalDef
mergeLayout p = case PsRegistry.findPedal p.meta.id of
  Just psDef -> p { layout = psDef.layout }
  Nothing -> p

-- | What the device said, once it is not a dump frame.
-- |
-- | Split out of the receive branch when acknowledgement moved in front of it:
-- | the ack has to happen for every frame regardless of kind, so the kinds had
-- | to stop being alternatives of one another.
handleReadReply
  :: forall o m. MonadAff m => Array Int -> H.HalogenM AppState Action Slots o m Unit
handleReadReply bytes = case Read.decodeReply bytes of
  Just (Read.BankNames names) ->
    H.modify_ \s -> s
      { mc6BankNames = Map.fromFoldable names
      , mc6ReadStatus = Just ("Read " <> show (Array.length names) <> " bank names.")
      }
  -- Double duty: what a bank holds, and — when the device volunteers it rather
  -- than answering a request — which bank it is standing on.
  Just (Read.BankSwitches bank names) ->
    H.modify_ \s -> s
      { mc6BankSwitches = Map.insert bank names s.mc6BankSwitches
      , mc6CurrentBank = Just bank
      , mc6ReadStatus = Just ("Read bank " <> show bank <> ".")
      }
  -- Said the instant the device moves, and the fastest honest answer to "which
  -- bank is it on". The switch names for the same bank turn up much later.
  Just (Read.CurrentBank bank name) ->
    H.modify_ \s -> s
      { mc6CurrentBank = Just bank
      , mc6ReadStatus = Just
          ("The MC6 is showing bank " <> show bank
            <> (if name == "" then "" else " (" <> name <> ")") <> ".")
      }
  Just (Read.CurrentPreset bank _) ->
    H.modify_ _ { mc6CurrentBank = Just bank }
  Just (Read.EditorMode on) -> do
    liftEffect $ Console.log $ "MC6 editor mode " <> (if on then "on" else "off")
    H.modify_ _ { mc6EditorMode = Just on }
  -- Decoded now rather than logged as a hex blob. These ten frames carry
  -- everything about the device that is not a preset — the channel table, the
  -- omniports that make the FS3X switches exist, the engines and counters —
  -- and the app used to acknowledge each one and throw the payload away.
  --
  -- Still logged as well as stored, because the hex is what a future diff is
  -- taken against: change one setting in the editor, capture again, and the
  -- byte that moved is the field.
  Just (Read.ControllerSettings f2 payload) -> do
    let section = Settings.decodeSection f2 payload
    liftEffect $ Console.log $
      "MC6 " <> Settings.sectionName section
        <> " (" <> show (Array.length payload) <> " bytes): "
        <> SysEx.toHexString payload
    H.modify_ \s -> s
      { mc6Settings = Map.insert f2 section s.mc6Settings }
  Just (Read.OtherReply f1 f2) ->
    liftEffect $ Console.log $ "MC6 reply not decoded: F1=" <> show f1 <> " F2=" <> show f2
  Nothing -> pure unit
