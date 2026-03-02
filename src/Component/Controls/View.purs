module Component.Controls.View
  ( component
  , Output(..)
  , Slot
  , Input
  ) where

import Prelude

import Data.Array as Array
import Data.Const (Const)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MC6.ControlBank (ControlBank, ControlBankSwitch, ccToggleMessages, ccMomentaryMessages)
import Data.MC6.Types (MC6Action(..), MC6Message, MC6MsgType(..), MC6TogglePosition(..), intToMC6Action, intToMC6MsgType, mc6ActionToInt, mc6ToggleToInt)
import Data.Midi (unCC)
import Data.Pedal (PedalDef, PedalId(..), Control(..), LabelSource(..), Section)
import Effect.Aff.Class (class MonadAff)
import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input =
  { controlBanks :: Array ControlBank
  , activeControlBankIdx :: Maybe Int
  , registry :: PedalRegistry
  , mc6BoardBankNum :: Int
  }

data Output
  = SaveControlBanks (Array ControlBank) (Maybe Int)
  | SyncControlBankToMC6

type State =
  { input :: Input
  , selectedBankIdx :: Int
  , editBankName :: String
  , editBankNumber :: String
  , editBankDescription :: String
  , editReturnSwitch :: Int
  , showNewBankForm :: Boolean
  , newBankName :: String
  , browserPedalId :: Maybe PedalId
  , browserTargetSwitch :: Int
  }

data Action
  = Receive Input
  | SelectBank Int
  -- Bank editing
  | UpdateBankName String
  | UpdateBankNumber String
  | UpdateBankDescription String
  | UpdateReturnSwitch String
  -- Bank CRUD
  | NewBank
  | CommitNewBank
  | CancelNewBank
  | UpdateNewBankName String
  | DuplicateBank
  | DeleteBank
  -- Switch editing (switchIdx, value)
  | UpdateLabel Int String
  | UpdateLongName Int String
  | UpdateToggle Int Boolean
  -- Message CRUD (switchIdx, ...)
  | AddMessage Int MC6MsgType
  | AddCCToggle Int
  | AddCCMomentary Int
  | DeleteMessage Int Int           -- switchIdx, msgIdx
  | UpdateMsgChannel Int Int String -- switchIdx, msgIdx, value
  | UpdateMsgData1 Int Int String
  | UpdateMsgData2 Int Int String
  | UpdateMsgData3 Int Int String
  | UpdateMsgData4 Int Int String
  | UpdateMsgAction Int Int String
  | UpdateMsgToggle Int Int String
  | UpdateMsgType Int Int String
  -- CC Browser
  | SelectBrowserPedal String
  | SelectBrowserTarget String
  | AddFromBrowser Int Int Boolean  -- channel, cc, isToggle
  -- Sync
  | SyncToMC6

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
  let bank = Array.index i.controlBanks 0
  in { input: i
     , selectedBankIdx: 0
     , editBankName: fromMaybe "" (bank <#> _.name)
     , editBankNumber: fromMaybe "" (bank <#> \b -> show b.mc6BankNumber)
     , editBankDescription: fromMaybe "" (bank <#> _.description)
     , editReturnSwitch: fromMaybe 6 (bank <#> _.returnSwitchIndex)
     , showNewBankForm: false
     , newBankName: ""
     , browserPedalId: Nothing
     , browserTargetSwitch: 0
     }

selectedBank :: State -> Maybe ControlBank
selectedBank st = Array.index st.input.controlBanks st.selectedBankIdx

-- | Rotating palette for bank identity colors
bankColor :: Int -> String
bankColor idx = case idx `mod` 8 of
  0 -> "#1a1a1a"
  1 -> "#1e40af"
  2 -> "#7e22ce"
  3 -> "#b45309"
  4 -> "#15803d"
  5 -> "#be123c"
  6 -> "#0e7490"
  7 -> "#6d28d9"
  _ -> "#1a1a1a"

-- | Physical MC6 layout: top row DEF (3,4,5), middle ABC (0,1,2), bottom GHI (6,7,8)
switchGridOrder :: Array Int
switchGridOrder = [3, 4, 5, 0, 1, 2, 6, 7, 8]

-- | Per-switch accent colors (muted, distinguishable)
switchColor :: Int -> String
switchColor = case _ of
  0 -> "#2563eb"  -- A blue
  1 -> "#7c3aed"  -- B violet
  2 -> "#db2777"  -- C pink
  3 -> "#d97706"  -- D amber
  4 -> "#059669"  -- E emerald
  5 -> "#0891b2"  -- F cyan
  6 -> "#4f46e5"  -- G indigo
  7 -> "#c026d3"  -- H fuchsia
  8 -> "#dc2626"  -- I red
  _ -> "#666"

switchLetter :: Int -> String
switchLetter = case _ of
  0 -> "A"
  1 -> "B"
  2 -> "C"
  3 -> "D"
  4 -> "E"
  5 -> "F"
  6 -> "G"
  7 -> "H"
  8 -> "I"
  _ -> "?"

mc6MsgTypeLabel :: MC6MsgType -> String
mc6MsgTypeLabel = case _ of
  MsgEmpty -> "Empty"
  MsgPC -> "PC"
  MsgCC -> "CC"
  MsgNote -> "Note"
  MsgBankJump -> "BankJump"
  MsgDelay -> "Delay"
  MsgSetToggle -> "SetToggle"
  MsgEngagePreset -> "Engage"
  MsgBankUp -> "BankUp"
  MsgBankDown -> "BankDown"
  MsgTogglePage -> "TogglePage"
  MsgOther n -> "Type " <> show n
  _ -> "Other"

-- ──── Render ────

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (H.ClassName "controls-view") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-left-panel") ]
        [ renderBankList state ]
    , renderAllSwitches state
    ]

renderBankList :: forall m. State -> H.ComponentHTML Action () m
renderBankList state =
  HH.div [ HP.class_ (H.ClassName "controls-bank-list") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-bank-list-header") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-heading") ] [ HH.text "Banks" ]
        , HH.button
            [ HP.class_ (H.ClassName "controls-btn-small")
            , HE.onClick \_ -> NewBank
            ]
            [ HH.text "+ New" ]
        ]
    , if state.showNewBankForm
        then renderNewBankForm state
        else HH.text ""
    , HH.div [ HP.class_ (H.ClassName "controls-bank-items") ]
        (Array.mapWithIndex (renderBankItem state) state.input.controlBanks)
    ]

renderNewBankForm :: forall m. State -> H.ComponentHTML Action () m
renderNewBankForm state =
  HH.div [ HP.class_ (H.ClassName "controls-new-bank-form") ]
    [ HH.input
        [ HP.type_ HP.InputText
        , HP.placeholder "Bank name"
        , HP.value state.newBankName
        , HE.onValueInput UpdateNewBankName
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-form-buttons") ]
        [ HH.button
            [ HP.class_ (H.ClassName "controls-btn-small")
            , HE.onClick \_ -> CommitNewBank
            ]
            [ HH.text "Create" ]
        , HH.button
            [ HP.class_ (H.ClassName "controls-btn-small")
            , HE.onClick \_ -> CancelNewBank
            ]
            [ HH.text "Cancel" ]
        ]
    ]

renderBankItem :: forall m. State -> Int -> ControlBank -> H.ComponentHTML Action () m
renderBankItem state idx bank =
  let isSelected = idx == state.selectedBankIdx
      color = bankColor idx
      headerStyle = if isSelected then "background: " <> color else ""
  in HH.div [ HP.class_ (H.ClassName ("controls-bank-item" <> if isSelected then " selected" else "")) ]
    [ HH.div
        [ HP.class_ (H.ClassName "controls-bank-item-header")
        , HP.attr (HH.AttrName "style") headerStyle
        , HE.onClick \_ -> SelectBank idx
        ]
        [ HH.div [ HP.class_ (H.ClassName "controls-bank-item-name") ] [ HH.text bank.name ]
        , HH.div [ HP.class_ (H.ClassName "controls-bank-item-num") ] [ HH.text ("Bank " <> show bank.mc6BankNumber) ]
        ]
    , if isSelected
        then HH.div [ HP.class_ (H.ClassName "controls-bank-expanded") ]
          [ renderSwitchGrid state
          , renderBankProperties state
          , HH.div [ HP.class_ (H.ClassName "controls-bank-actions") ]
              [ HH.button
                  [ HP.class_ (H.ClassName "controls-btn-small")
                  , HE.onClick \_ -> DuplicateBank
                  ]
                  [ HH.text "Dup" ]
              , HH.button
                  [ HP.class_ (H.ClassName "controls-btn-small controls-btn-danger")
                  , HE.onClick \_ -> DeleteBank
                  ]
                  [ HH.text "Del" ]
              ]
          ]
        else HH.text ""
    ]

renderSwitchGrid :: forall m. State -> H.ComponentHTML Action () m
renderSwitchGrid state =
  HH.div [ HP.class_ (H.ClassName "controls-switch-panel") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-switch-grid") ]
        (map (renderSwitchCell state) switchGridOrder)
    ]

renderSwitchCell :: forall m. State -> Int -> H.ComponentHTML Action () m
renderSwitchCell state idx =
  let mBank = selectedBank state
      mSw = mBank >>= \b -> Array.index b.switches idx
      label = fromMaybe "" (mSw <#> _.label)
      isReturn = fromMaybe false (mBank <#> \b -> b.returnSwitchIndex == idx)
      cls = "controls-switch" <> (if isReturn then " return" else "")
      color = switchColor idx
  in HH.div
    [ HP.class_ (H.ClassName cls)
    , HP.attr (HH.AttrName "style") ("border-left: 3px solid " <> color)
    ]
    [ HH.div [ HP.class_ (H.ClassName "controls-switch-letter"), HP.attr (HH.AttrName "style") ("color: " <> color) ] [ HH.text (switchLetter idx) ]
    , HH.div [ HP.class_ (H.ClassName "controls-switch-label") ] [ HH.text label ]
    ]

renderBankProperties :: forall m. State -> H.ComponentHTML Action () m
renderBankProperties state =
  HH.div [ HP.class_ (H.ClassName "controls-bank-props") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-field-row") ]
        [ HH.label_ [ HH.text "Name" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.value state.editBankName
            , HE.onValueInput UpdateBankName
            ]
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-field-row") ]
        [ HH.label_ [ HH.text "MC6 Bank" ]
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.value state.editBankNumber
            , HE.onValueInput UpdateBankNumber
            , HP.attr (HH.AttrName "min") "0"
            , HP.attr (HH.AttrName "max") "29"
            , HP.attr (HH.AttrName "style") "width: 60px"
            ]
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-field-row") ]
        [ HH.label_ [ HH.text "Return Sw" ]
        , HH.select
            [ HP.value (show state.editReturnSwitch)
            , HE.onValueChange UpdateReturnSwitch
            ]
            (Array.range 0 8 <#> \i ->
              HH.option [ HP.value (show i) ] [ HH.text (switchLetter i) ]
            )
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-field-row controls-field-row-top") ]
        [ HH.label_ [ HH.text "Notes" ]
        , HH.textarea
            [ HP.value state.editBankDescription
            , HP.placeholder "Description"
            , HP.rows 3
            , HE.onValueInput UpdateBankDescription
            ]
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-field-row") ]
        [ HH.button
            [ HP.class_ (H.ClassName "controls-btn controls-btn-accent")
            , HP.attr (HH.AttrName "style") ("background: " <> bankColor state.selectedBankIdx <> "; border-color: " <> bankColor state.selectedBankIdx)
            , HE.onClick \_ -> SyncToMC6
            ]
            [ HH.text "Sync to MC6" ]
        ]
    ]

-- ──── All Switches Panel (right column) ────

renderAllSwitches :: forall m. State -> H.ComponentHTML Action () m
renderAllSwitches state = case selectedBank state of
  Nothing ->
    HH.div [ HP.class_ (H.ClassName "controls-message-editor") ]
      [ HH.p [ HP.class_ (H.ClassName "controls-empty") ] [ HH.text "Select a bank" ] ]
  Just bank ->
    let color = bankColor state.selectedBankIdx
        renderIdx i = case Array.index bank.switches i of
          Nothing -> HH.text ""
          Just sw -> renderSwitchSection color bank.returnSwitchIndex i sw
    in HH.div [ HP.class_ (H.ClassName "controls-message-editor") ]
      [ HH.div [ HP.class_ (H.ClassName "controls-sw-grid") ]
          (map renderIdx switchGridOrder)
      , renderCCBrowser state
      ]

renderSwitchSection :: forall m. String -> Int -> Int -> ControlBankSwitch -> H.ComponentHTML Action () m
renderSwitchSection bankCol returnIdx swIdx sw =
  let isReturn = swIdx == returnIdx
      swColor = switchColor swIdx
  in HH.div
    [ HP.class_ (H.ClassName "controls-sw-section")
    , HP.attr (HH.AttrName "style") ("border-left: 3px solid " <> swColor)
    ]
    [ HH.div [ HP.class_ (H.ClassName "controls-sw-section-header") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-sw-section-letter"), HP.attr (HH.AttrName "style") ("color: " <> swColor) ] [ HH.text (switchLetter swIdx) ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.class_ (H.ClassName "controls-sw-label-input")
            , HP.value sw.label
            , HP.attr (HH.AttrName "maxlength") "8"
            , HP.placeholder "label"
            , HE.onValueInput (UpdateLabel swIdx)
            ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.class_ (H.ClassName "controls-sw-long-input")
            , HP.value sw.longName
            , HP.attr (HH.AttrName "maxlength") "24"
            , HP.placeholder "long name"
            , HE.onValueInput (UpdateLongName swIdx)
            ]
        , HH.label [ HP.class_ (H.ClassName "controls-sw-toggle-label") ]
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.checked sw.toToggle
                , HE.onChecked (UpdateToggle swIdx)
                ]
            , HH.text " Tgl"
            ]
        , if isReturn
            then HH.span [ HP.class_ (H.ClassName "controls-sw-return-badge") ] [ HH.text "RTN" ]
            else HH.text ""
        ]
    , if Array.null sw.messages
        then HH.text ""
        else HH.div [ HP.class_ (H.ClassName "controls-messages") ]
          (Array.mapWithIndex (\i msg -> renderMessageRow bankCol swIdx i msg) sw.messages)
    , renderAddButtons swIdx
    ]

renderMessageRow :: forall m. String -> Int -> Int -> MC6Message -> H.ComponentHTML Action () m
renderMessageRow color swIdx msgIdx msg =
  HH.div [ HP.class_ (H.ClassName "controls-message-row") ]
    [ HH.span [ HP.class_ (H.ClassName "controls-msg-type-badge"), HP.attr (HH.AttrName "style") ("background: " <> color) ]
        [ HH.text (mc6MsgTypeLabel msg.msgType) ]
    , renderMsgFields swIdx msgIdx msg
    , HH.button
        [ HP.class_ (H.ClassName "controls-msg-delete")
        , HE.onClick \_ -> DeleteMessage swIdx msgIdx
        ]
        [ HH.text "\x00D7" ]
    ]

renderMsgFields :: forall m. Int -> Int -> MC6Message -> H.ComponentHTML Action () m
renderMsgFields swIdx msgIdx msg = case msg.msgType of
  MsgCC ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Ch" (show msg.channel) (UpdateMsgChannel swIdx msgIdx) "38px"
      , msgField "CC" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , msgField "Val" (show msg.data2) (UpdateMsgData2 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      , toggleSelect swIdx msgIdx msg.togglePosition
      ]
  MsgPC ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Ch" (show msg.channel) (UpdateMsgChannel swIdx msgIdx) "38px"
      , msgField "PC" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      ]
  MsgBankJump ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Bank" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      ]
  MsgDelay ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "ms" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "55px" ]
  MsgEngagePreset ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Preset" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "55px"
      , actionSelect swIdx msgIdx msg.action
      ]
  MsgNote ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Ch" (show msg.channel) (UpdateMsgChannel swIdx msgIdx) "38px"
      , msgField "Note" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , msgField "Vel" (show msg.data2) (UpdateMsgData2 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      ]
  MsgSetToggle ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "d1" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      ]
  _ ->
    HH.span [ HP.class_ (H.ClassName "controls-msg-fields") ]
      [ msgField "Ch" (show msg.channel) (UpdateMsgChannel swIdx msgIdx) "38px"
      , msgField "d1" (show msg.data1) (UpdateMsgData1 swIdx msgIdx) "45px"
      , msgField "d2" (show msg.data2) (UpdateMsgData2 swIdx msgIdx) "45px"
      , actionSelect swIdx msgIdx msg.action
      , toggleSelect swIdx msgIdx msg.togglePosition
      ]

msgField :: forall m. String -> String -> (String -> Action) -> String -> H.ComponentHTML Action () m
msgField lbl val onChange w =
  HH.label [ HP.class_ (H.ClassName "controls-msg-field") ]
    [ HH.span [ HP.class_ (H.ClassName "controls-msg-field-label") ] [ HH.text lbl ]
    , HH.input
        [ HP.type_ HP.InputNumber
        , HP.value val
        , HP.attr (HH.AttrName "style") ("width: " <> w)
        , HE.onValueInput onChange
        ]
    ]

actionSelect :: forall m. Int -> Int -> MC6Action -> H.ComponentHTML Action () m
actionSelect swIdx msgIdx current =
  HH.select
    [ HP.class_ (H.ClassName "controls-msg-select")
    , HP.value (show (mc6ActionToInt current))
    , HE.onValueChange (UpdateMsgAction swIdx msgIdx)
    ]
    [ HH.option [ HP.value "0" ] [ HH.text "None" ]
    , HH.option [ HP.value "1" ] [ HH.text "Press" ]
    , HH.option [ HP.value "2" ] [ HH.text "Release" ]
    , HH.option [ HP.value "3" ] [ HH.text "LongPress" ]
    , HH.option [ HP.value "4" ] [ HH.text "LPRelease" ]
    , HH.option [ HP.value "5" ] [ HH.text "DblTap" ]
    , HH.option [ HP.value "6" ] [ HH.text "DblTapRel" ]
    , HH.option [ HP.value "9" ] [ HH.text "RelAll" ]
    ]

toggleSelect :: forall m. Int -> Int -> MC6TogglePosition -> H.ComponentHTML Action () m
toggleSelect swIdx msgIdx current =
  HH.select
    [ HP.class_ (H.ClassName "controls-msg-select")
    , HP.value (show (mc6ToggleToInt current))
    , HE.onValueChange (UpdateMsgToggle swIdx msgIdx)
    ]
    [ HH.option [ HP.value "0" ] [ HH.text "Tg Off" ]
    , HH.option [ HP.value "1" ] [ HH.text "Tg On" ]
    , HH.option [ HP.value "2" ] [ HH.text "Both" ]
    ]

renderAddButtons :: forall m. Int -> H.ComponentHTML Action () m
renderAddButtons swIdx =
  HH.div [ HP.class_ (H.ClassName "controls-add-message") ]
    [ HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddCCToggle swIdx ] [ HH.text "+Toggle" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddCCMomentary swIdx ] [ HH.text "+Mom" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddMessage swIdx MsgCC ] [ HH.text "+CC" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddMessage swIdx MsgPC ] [ HH.text "+PC" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddMessage swIdx MsgBankJump ] [ HH.text "+BkJmp" ]
    , HH.button [ HP.class_ (H.ClassName "controls-btn-tiny"), HE.onClick \_ -> AddMessage swIdx MsgDelay ] [ HH.text "+Delay" ]
    ]

-- ──── CC Browser ────

renderCCBrowser :: forall m. State -> H.ComponentHTML Action () m
renderCCBrowser state =
  HH.div [ HP.class_ (H.ClassName "controls-cc-browser") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-cc-browser-header") ] [ HH.text "Pedal CC Browser" ]
    , HH.div [ HP.class_ (H.ClassName "controls-field-row") ]
        [ HH.label_ [ HH.text "Pedal" ]
        , HH.select
            [ HP.value (fromMaybe "" (state.browserPedalId <#> show))
            , HE.onValueChange SelectBrowserPedal
            ]
            ( [ HH.option [ HP.value "" ] [ HH.text "-- Select --" ] ]
              <> map pedalOption (CRegistry.registryPedals state.input.registry)
            )
        , HH.label_ [ HH.text "Add to" ]
        , HH.select
            [ HP.value (show state.browserTargetSwitch)
            , HE.onValueChange SelectBrowserTarget
            ]
            (Array.range 0 8 <#> \i ->
              HH.option [ HP.value (show i) ] [ HH.text (switchLetter i) ]
            )
        ]
    , case state.browserPedalId of
        Nothing -> HH.text ""
        Just pid -> case CRegistry.findPedal state.input.registry pid of
          Nothing -> HH.text ""
          Just def -> renderPedalSections state def
    ]

pedalOption :: forall m. PedalDef -> H.ComponentHTML Action () m
pedalOption def =
  HH.option [ HP.value (show def.meta.id) ] [ HH.text (def.meta.name <> " (ch " <> show def.meta.defaultChannel <> ")") ]

renderPedalSections :: forall m. State -> PedalDef -> H.ComponentHTML Action () m
renderPedalSections state def =
  HH.div [ HP.class_ (H.ClassName "controls-cc-sections") ]
    (map (renderSection state def.meta.defaultChannel) def.sections)

renderSection :: forall m. State -> Int -> Section -> H.ComponentHTML Action () m
renderSection state ch section =
  let ccRows = Array.mapMaybe (controlToCC ch) section.controls
  in if Array.null ccRows
    then HH.text ""
    else HH.div [ HP.class_ (H.ClassName "controls-cc-section") ]
      [ HH.div [ HP.class_ (H.ClassName "controls-cc-section-name") ] [ HH.text section.name ]
      , HH.div_ (map (renderCCRow state) ccRows)
      ]

type CCRow = { ch :: Int, cc :: Int, label :: String, isToggle :: Boolean }

controlToCC :: Int -> Control -> Maybe CCRow
controlToCC ch = case _ of
  Slider r -> Just { ch, cc: unCC r.cc, label: resolveLabel r.label, isToggle: false }
  Toggle r -> Just { ch, cc: unCC r.cc, label: r.label, isToggle: true }
  Momentary r -> Just { ch, cc: unCC r.cc, label: r.label, isToggle: false }
  Segmented r -> Just { ch, cc: unCC r.cc, label: r.label, isToggle: false }
  Dropdown r -> Just { ch, cc: unCC r.cc, label: r.label, isToggle: false }
  _ -> Nothing
  where
  resolveLabel = case _ of
    Static s -> s
    ModeMap r -> "CC " <> show (unCC r.cc)
    ChannelMode _ -> "CC (mode)"

renderCCRow :: forall m. State -> CCRow -> H.ComponentHTML Action () m
renderCCRow state row =
  HH.div [ HP.class_ (H.ClassName "controls-cc-row") ]
    [ HH.span [ HP.class_ (H.ClassName "controls-cc-num") ] [ HH.text ("CC" <> show row.cc) ]
    , HH.span [ HP.class_ (H.ClassName "controls-cc-label") ] [ HH.text row.label ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser row.ch row.cc true
        ]
        [ HH.text ("+" <> switchLetter state.browserTargetSwitch <> " Tgl") ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser row.ch row.cc false
        ]
        [ HH.text ("+" <> switchLetter state.browserTargetSwitch <> " Mom") ]
    ]

-- ──── Action Handlers ────

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input -> do
    st <- H.get
    if Array.length input.controlBanks /= Array.length st.input.controlBanks
      then H.put (initialState input)
      else H.modify_ _ { input = input }

  SelectBank idx -> do
    -- Save pending bank property edits first
    commitBankProps
    H.modify_ \st ->
      let mBank = Array.index st.input.controlBanks idx
      in st { selectedBankIdx = idx
            , editBankName = fromMaybe "" (mBank <#> _.name)
            , editBankNumber = fromMaybe "" (mBank <#> \b -> show b.mc6BankNumber)
            , editBankDescription = fromMaybe "" (mBank <#> _.description)
            , editReturnSwitch = fromMaybe 6 (mBank <#> _.returnSwitchIndex)
            }

  -- Bank properties
  UpdateBankName s -> do
    H.modify_ _ { editBankName = s }
    commitBankProps
  UpdateBankNumber s -> do
    H.modify_ _ { editBankNumber = s }
    commitBankProps
  UpdateBankDescription s -> do
    H.modify_ _ { editBankDescription = s }
    commitBankProps
  UpdateReturnSwitch s -> case Int.fromString s of
    Nothing -> pure unit
    Just i -> do
      H.modify_ _ { editReturnSwitch = i }
      commitBankProps

  -- Bank CRUD
  NewBank -> H.modify_ _ { showNewBankForm = true, newBankName = "" }
  CancelNewBank -> H.modify_ _ { showNewBankForm = false }
  UpdateNewBankName s -> H.modify_ _ { newBankName = s }
  CommitNewBank -> do
    st <- H.get
    when (st.newBankName /= "") do
      let nextBankNum = case Array.last st.input.controlBanks of
            Just cb -> cb.mc6BankNumber + 1
            Nothing -> 20
          newBank = emptyControlBank st.newBankName nextBankNum
          banks = st.input.controlBanks <> [newBank]
          newIdx = Array.length banks - 1
      H.modify_ _ { showNewBankForm = false }
      H.raise (SaveControlBanks banks (Just newIdx))
      handleAction (SelectBank newIdx)

  DuplicateBank -> do
    st <- H.get
    case selectedBank st of
      Nothing -> pure unit
      Just bank -> do
        let dup = bank { id = bank.id <> "-copy", name = bank.name <> " Copy", mc6BankNumber = bank.mc6BankNumber + 1 }
            banks = Array.snoc st.input.controlBanks dup
            newIdx = Array.length banks - 1
        H.raise (SaveControlBanks banks (Just newIdx))
        handleAction (SelectBank newIdx)

  DeleteBank -> do
    st <- H.get
    when (Array.length st.input.controlBanks > 0) do
      let banks = fromMaybe st.input.controlBanks (Array.deleteAt st.selectedBankIdx st.input.controlBanks)
          newIdx = if st.selectedBankIdx >= Array.length banks
                   then max 0 (Array.length banks - 1)
                   else st.selectedBankIdx
          mActive = if Array.null banks then Nothing else Just newIdx
      H.raise (SaveControlBanks banks mActive)
      when (not (Array.null banks)) do
        handleAction (SelectBank newIdx)

  -- Switch editing — modify bank data directly
  UpdateLabel swIdx s -> do
    modifySwitch swIdx \sw -> sw { label = s }
    save
  UpdateLongName swIdx s -> do
    modifySwitch swIdx \sw -> sw { longName = s }
    save
  UpdateToggle swIdx b -> do
    modifySwitch swIdx \sw -> sw { toToggle = b }
    save

  -- Message CRUD
  AddMessage swIdx msgType -> do
    modifySwitchMessages swIdx \msgs -> msgs <> [newMessage msgType]
    save
  AddCCToggle swIdx -> do
    modifySwitchMessages swIdx \msgs -> msgs <> ccToggleMessages 1 0
    save
  AddCCMomentary swIdx -> do
    modifySwitchMessages swIdx \msgs -> msgs <> ccMomentaryMessages 1 0
    save
  DeleteMessage swIdx msgIdx -> do
    modifySwitchMessages swIdx \msgs -> fromMaybe msgs (Array.deleteAt msgIdx msgs)
    save

  -- Message field updates
  UpdateMsgChannel swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { channel = v }
    Nothing -> msg
  UpdateMsgData1 swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { data1 = v }
    Nothing -> msg
  UpdateMsgData2 swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { data2 = v }
    Nothing -> msg
  UpdateMsgData3 swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { data3 = v }
    Nothing -> msg
  UpdateMsgData4 swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { data4 = v }
    Nothing -> msg
  UpdateMsgAction swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { action = intToMC6Action v }
    Nothing -> msg
  UpdateMsgToggle swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { togglePosition = intToToggle v }
    Nothing -> msg
  UpdateMsgType swIdx msgIdx s -> updateMsg swIdx msgIdx \msg -> case Int.fromString s of
    Just v -> msg { msgType = intToMC6MsgType v }
    Nothing -> msg

  -- CC Browser
  SelectBrowserPedal s ->
    if s == ""
      then H.modify_ _ { browserPedalId = Nothing }
      else H.modify_ _ { browserPedalId = Just (PedalId s) }
  SelectBrowserTarget s -> case Int.fromString s of
    Just i -> H.modify_ _ { browserTargetSwitch = i }
    Nothing -> pure unit
  AddFromBrowser ch cc isToggle -> do
    st <- H.get
    let msgs = if isToggle then ccToggleMessages ch cc else ccMomentaryMessages ch cc
    modifySwitchMessages st.browserTargetSwitch \existing -> existing <> msgs
    save

  SyncToMC6 -> do
    commitBankProps
    H.raise SyncControlBankToMC6

-- ──── Helpers ────

intToToggle :: Int -> MC6TogglePosition
intToToggle = case _ of
  0 -> ToggleOff
  1 -> ToggleOn
  _ -> ToggleBoth

newMessage :: MC6MsgType -> MC6Message
newMessage msgType =
  { msgType
  , channel: 1
  , data1: 0
  , data2: 0
  , data3: 0
  , data4: 0
  , action: ActionPress
  , togglePosition: ToggleBoth
  , msgIndex: 0
  }

emptySwitch :: ControlBankSwitch
emptySwitch =
  { label: ""
  , longName: ""
  , toToggle: false
  , messages: []
  }

emptyControlBank :: String -> Int -> ControlBank
emptyControlBank name bankNum =
  { id: "bank-" <> show bankNum
  , name
  , description: ""
  , mc6BankNumber: bankNum
  , returnSwitchIndex: 6
  , switches: Array.replicate 9 emptySwitch
  }

-- | Modify a switch in the selected bank
modifySwitch :: forall m. MonadAff m => Int -> (ControlBankSwitch -> ControlBankSwitch) -> H.HalogenM State Action () Output m Unit
modifySwitch swIdx f = do
  st <- H.get
  case Array.index st.input.controlBanks st.selectedBankIdx of
    Nothing -> pure unit
    Just bank -> do
      let newSwitches = fromMaybe bank.switches (Array.modifyAt swIdx f bank.switches)
          newBank = bank { switches = newSwitches }
          newBanks = fromMaybe st.input.controlBanks (Array.updateAt st.selectedBankIdx newBank st.input.controlBanks)
      H.modify_ _ { input = st.input { controlBanks = newBanks } }

-- | Modify messages of a specific switch
modifySwitchMessages :: forall m. MonadAff m => Int -> (Array MC6Message -> Array MC6Message) -> H.HalogenM State Action () Output m Unit
modifySwitchMessages swIdx f =
  modifySwitch swIdx \sw -> sw { messages = reindexMessages (f sw.messages) }

-- | Update a single message field
updateMsg :: forall m. MonadAff m => Int -> Int -> (MC6Message -> MC6Message) -> H.HalogenM State Action () Output m Unit
updateMsg swIdx msgIdx f = do
  modifySwitchMessages swIdx \msgs -> fromMaybe msgs (Array.modifyAt msgIdx f msgs)
  save

-- | Apply bank property edits to data and save
commitBankProps :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
commitBankProps = do
  st <- H.get
  case Array.index st.input.controlBanks st.selectedBankIdx of
    Nothing -> pure unit
    Just bank -> do
      let bankNum = fromMaybe bank.mc6BankNumber (Int.fromString st.editBankNumber)
          updatedBank = bank
            { name = st.editBankName
            , mc6BankNumber = bankNum
            , description = st.editBankDescription
            , returnSwitchIndex = st.editReturnSwitch
            }
          banks = fromMaybe st.input.controlBanks
            (Array.updateAt st.selectedBankIdx updatedBank st.input.controlBanks)
      H.modify_ _ { input = st.input { controlBanks = banks } }
      H.raise (SaveControlBanks banks (Just st.selectedBankIdx))

-- | Save current bank data
save :: forall m. MonadAff m => H.HalogenM State Action () Output m Unit
save = do
  st <- H.get
  H.raise (SaveControlBanks st.input.controlBanks (Just st.selectedBankIdx))

reindexMessages :: Array MC6Message -> Array MC6Message
reindexMessages = Array.mapWithIndex \i m -> m { msgIndex = i }
