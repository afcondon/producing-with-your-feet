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
import Data.Pedal (PedalDef, PedalId, Control(..), LabelSource(..), Section)
import Data.String.CodeUnits (contains)
import Data.String.Common (toLower)
import Data.String.Pattern (Pattern(..))
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

-- | Flat searchable entry for one CC control from the pedal registry
type CCEntry =
  { pedalId :: PedalId
  , pedalName :: String
  , pedalShort :: String
  , brand :: String
  , channel :: Int
  , sectionName :: String
  , cc :: Int
  , label :: String
  , controlKind :: String
  , isToggle :: Boolean
  }

type State =
  { input :: Input
  , selectedBankIdx :: Int
  , editBankName :: String
  , editBankNumber :: String
  , editBankDescription :: String
  , editReturnSwitch :: Int
  , showNewBankForm :: Boolean
  , newBankName :: String
  , ccIndex :: Array CCEntry
  , searchQuery :: String
  , expandedPedals :: Array PedalId
  , browserTargetSwitch :: Int
  , showDictionary :: Boolean
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
  | UpdateLooper Int Boolean
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
  -- CC Search + Dictionary
  | UpdateSearch String
  | ToggleDictionary
  | ToggleDictPedal PedalId
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
     , ccIndex: buildCCIndex i.registry
     , searchQuery: ""
     , expandedPedals: []
     , browserTargetSwitch: 0
     , showDictionary: false
     }

-- | Flatten the entire pedal registry into a searchable array of CC entries
buildCCIndex :: PedalRegistry -> Array CCEntry
buildCCIndex reg = Array.concatMap flattenPedal (CRegistry.registryPedals reg)
  where
  flattenPedal :: PedalDef -> Array CCEntry
  flattenPedal def = Array.concatMap (flattenSection def) def.sections

  flattenSection :: PedalDef -> Section -> Array CCEntry
  flattenSection def section = Array.mapMaybe (toEntry def section) section.controls

  toEntry :: PedalDef -> Section -> Control -> Maybe CCEntry
  toEntry def section ctrl = case ctrl of
    Slider r -> Just (mk (resolveLabel r.label) (unCC r.cc) "slider" false)
    Toggle r -> Just (mk r.label (unCC r.cc) "toggle" true)
    Momentary r -> Just (mk r.label (unCC r.cc) "momentary" false)
    Segmented r -> Just (mk r.label (unCC r.cc) "segmented" false)
    Dropdown r -> Just (mk r.label (unCC r.cc) "dropdown" false)
    _ -> Nothing
    where
    mk lbl ccNum kind isTgl =
      { pedalId: def.meta.id
      , pedalName: def.meta.name
      , pedalShort: def.meta.shortName
      , brand: def.meta.brand
      , channel: def.meta.defaultChannel
      , sectionName: section.name
      , cc: ccNum
      , label: lbl
      , controlKind: kind
      , isToggle: isTgl
      }

  resolveLabel :: LabelSource -> String
  resolveLabel = case _ of
    Static s -> s
    ModeMap r -> "CC " <> show (unCC r.cc)
    ChannelMode _ -> "CC (mode)"

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
    [ HH.div [ HP.class_ (H.ClassName "controls-top-row") ]
        [ HH.div [ HP.class_ (H.ClassName "controls-bank-col") ]
            [ renderBankList state
            , renderSwitchGrid state
            ]
        , renderBankPropsCol state
        , renderSearchPanel state
        ]
    , HH.div [ HP.class_ (H.ClassName "controls-bottom-row") ]
        [ renderAllSwitches state ]
    , if state.showDictionary then renderDictionaryOverlay state else HH.text ""
    ]

renderBankList :: forall m. State -> H.ComponentHTML Action () m
renderBankList state =
  HH.div [ HP.class_ (H.ClassName "controls-bank-list") ]
    [ if state.showNewBankForm
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
        ( if isSelected
            then
              [ HH.input
                  [ HP.type_ HP.InputText
                  , HP.class_ (H.ClassName "controls-bank-name-input")
                  , HP.value state.editBankName
                  , HE.onValueInput UpdateBankName
                  ]
              , HH.div [ HP.class_ (H.ClassName "controls-bank-item-num") ]
                  [ HH.text "Bank "
                  , HH.input
                      [ HP.type_ HP.InputNumber
                      , HP.class_ (H.ClassName "controls-bank-num-input")
                      , HP.value state.editBankNumber
                      , HE.onValueInput UpdateBankNumber
                      , HP.attr (HH.AttrName "min") "0"
                      , HP.attr (HH.AttrName "max") "29"
                      ]
                  ]
              ]
            else
              [ HH.div [ HP.class_ (H.ClassName "controls-bank-item-name") ] [ HH.text bank.name ]
              , HH.div [ HP.class_ (H.ClassName "controls-bank-item-num") ] [ HH.text ("Bank " <> show bank.mc6BankNumber) ]
              ]
        )
    ]

renderBankPropsCol :: forall m. State -> H.ComponentHTML Action () m
renderBankPropsCol state = case selectedBank state of
  Nothing -> HH.text ""
  Just _ ->
    HH.div [ HP.class_ (H.ClassName "controls-props-col") ]
      [ renderBankProperties state
      , HH.div [ HP.class_ (H.ClassName "controls-bank-actions") ]
          [ HH.button
              [ HP.class_ (H.ClassName "controls-btn-small")
              , HE.onClick \_ -> NewBank
              ]
              [ HH.text "+ New" ]
          , HH.button
              [ HP.class_ (H.ClassName "controls-btn controls-btn-accent")
              , HP.attr (HH.AttrName "style") ("background: " <> bankColor state.selectedBankIdx <> "; border-color: " <> bankColor state.selectedBankIdx)
              , HE.onClick \_ -> SyncToMC6
              ]
              [ HH.text "Sync" ]
          , HH.button
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
        , HH.label [ HP.class_ (H.ClassName "controls-sw-toggle-label") ]
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.checked (hasLooperMode sw)
                , HE.onChecked (UpdateLooper swIdx)
                ]
            , HH.text " Loop"
            ]
        , if isReturn
            then HH.span [ HP.class_ (H.ClassName "controls-sw-return-badge") ] [ HH.text "RTN" ]
            else HH.text ""
        ]
    , let indexed = Array.mapWithIndex (\i msg -> { idx: i, msg }) sw.messages
          visible = Array.filter (\r -> r.msg.msgType /= MsgLooperMode) indexed
      in if Array.null visible
        then HH.text ""
        else HH.div [ HP.class_ (H.ClassName "controls-messages") ]
          (map (\r -> renderMessageRow bankCol swIdx r.idx r.msg) visible)
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

-- ──── Search Panel (right side of top row) ────

renderSearchPanel :: forall m. State -> H.ComponentHTML Action () m
renderSearchPanel state =
  HH.div [ HP.class_ (H.ClassName "controls-search-panel") ]
    [ HH.div [ HP.class_ (H.ClassName "controls-search-input-row") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-search-icon") ] [ HH.text "\x1F50D" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.class_ (H.ClassName "controls-search-input")
            , HP.placeholder "Search CCs across all pedals..."
            , HP.value state.searchQuery
            , HE.onValueInput UpdateSearch
            ]
        , HH.span [ HP.class_ (H.ClassName "controls-search-target") ]
            [ HH.text "Add to:"
            , HH.select
                [ HP.value (show state.browserTargetSwitch)
                , HE.onValueChange SelectBrowserTarget
                ]
                (Array.range 0 8 <#> \i ->
                  HH.option [ HP.value (show i) ] [ HH.text (switchLetter i) ]
                )
            ]
        , HH.button
            [ HP.class_ (H.ClassName "controls-btn-small")
            , HE.onClick \_ -> ToggleDictionary
            ]
            [ HH.text "Browse All" ]
        ]
    , renderSearchResults state
    ]

renderSearchResults :: forall m. State -> H.ComponentHTML Action () m
renderSearchResults state =
  let q = toLower state.searchQuery
  in if q == ""
    then HH.text ""
    else
      let matches = Array.filter (matchEntry q) state.ccIndex
      in if Array.null matches
        then HH.div [ HP.class_ (H.ClassName "controls-search-empty") ] [ HH.text "No matches" ]
        else HH.div [ HP.class_ (H.ClassName "controls-search-results") ]
          (map (renderSearchRow state) matches)

matchEntry :: String -> CCEntry -> Boolean
matchEntry q entry =
  contains (Pattern q) (toLower entry.label)
  || contains (Pattern q) (toLower (show entry.cc))
  || contains (Pattern q) (toLower entry.pedalName)
  || contains (Pattern q) (toLower entry.pedalShort)
  || contains (Pattern q) (toLower entry.sectionName)
  || contains (Pattern q) (toLower entry.controlKind)

renderSearchRow :: forall m. State -> CCEntry -> H.ComponentHTML Action () m
renderSearchRow state entry =
  HH.div [ HP.class_ (H.ClassName "controls-search-row") ]
    [ HH.span [ HP.class_ (H.ClassName "controls-search-pedal") ] [ HH.text entry.pedalShort ]
    , HH.span [ HP.class_ (H.ClassName "controls-cc-num") ] [ HH.text ("CC" <> show entry.cc) ]
    , HH.span [ HP.class_ (H.ClassName "controls-search-ch") ] [ HH.text ("ch" <> show entry.channel) ]
    , HH.span [ HP.class_ (H.ClassName "controls-cc-label") ] [ HH.text entry.label ]
    , HH.span [ HP.class_ (H.ClassName "controls-search-kind") ] [ HH.text entry.controlKind ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser entry.channel entry.cc true
        ]
        [ HH.text ("+" <> switchLetter state.browserTargetSwitch <> " Tgl") ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser entry.channel entry.cc false
        ]
        [ HH.text ("+" <> switchLetter state.browserTargetSwitch <> " Mom") ]
    ]

-- ──── CC Dictionary (modal overlay) ────

renderDictionaryOverlay :: forall m. State -> H.ComponentHTML Action () m
renderDictionaryOverlay state =
  HH.div [ HP.class_ (H.ClassName "controls-dict-overlay") ]
    [ HH.div
        [ HP.class_ (H.ClassName "controls-dict-backdrop")
        , HE.onClick \_ -> ToggleDictionary
        ]
        []
    , HH.div [ HP.class_ (H.ClassName "controls-dict-modal") ]
        [ HH.div [ HP.class_ (H.ClassName "controls-dict-modal-header") ]
            [ HH.span [ HP.class_ (H.ClassName "controls-dict-header") ] [ HH.text "CC Dictionary" ]
            , HH.button
                [ HP.class_ (H.ClassName "controls-dict-close")
                , HE.onClick \_ -> ToggleDictionary
                ]
                [ HH.text "\x00D7" ]
            ]
        , renderDictionary state
        ]
    ]

renderDictionary :: forall m. State -> H.ComponentHTML Action () m
renderDictionary state =
  let pedals = CRegistry.registryPedals state.input.registry
  in HH.div [ HP.class_ (H.ClassName "controls-dict-list") ]
    (map (renderDictPedal state) pedals)

renderDictPedal :: forall m. State -> PedalDef -> H.ComponentHTML Action () m
renderDictPedal state def =
  let pid = def.meta.id
      isExpanded = Array.elem pid state.expandedPedals
      arrow = if isExpanded then "\x25BE " else "\x25B8 "
  in HH.div [ HP.class_ (H.ClassName ("controls-dict-pedal" <> if isExpanded then " expanded" else "")) ]
    [ HH.div
        [ HP.class_ (H.ClassName "controls-dict-pedal-header")
        , HE.onClick \_ -> ToggleDictPedal pid
        ]
        [ HH.span [ HP.class_ (H.ClassName "controls-dict-arrow") ] [ HH.text arrow ]
        , HH.span [ HP.class_ (H.ClassName "controls-dict-pedal-name") ] [ HH.text def.meta.name ]
        , HH.span [ HP.class_ (H.ClassName "controls-dict-pedal-meta") ]
            [ HH.text (def.meta.brand <> ", ch " <> show def.meta.defaultChannel) ]
        ]
    , if isExpanded
        then HH.div [ HP.class_ (H.ClassName "controls-dict-sections") ]
          (Array.mapMaybe (renderDictSection state def.meta.defaultChannel) def.sections)
        else HH.text ""
    ]

renderDictSection :: forall m. State -> Int -> Section -> Maybe (H.ComponentHTML Action () m)
renderDictSection state ch section =
  let entries = Array.mapMaybe (controlToCCEntry ch) section.controls
  in if Array.null entries
    then Nothing
    else Just $
      HH.div [ HP.class_ (H.ClassName "controls-dict-section") ]
        [ HH.span [ HP.class_ (H.ClassName "controls-dict-section-name") ] [ HH.text (section.name <> ":") ]
        , HH.span [ HP.class_ (H.ClassName "controls-dict-controls") ]
            (Array.concatMap (\e -> [ renderDictControl state e ]) entries)
        ]

type DictEntry = { ch :: Int, cc :: Int, label :: String, kind :: String, isToggle :: Boolean }

controlToCCEntry :: Int -> Control -> Maybe DictEntry
controlToCCEntry ch = case _ of
  Slider r -> Just { ch, cc: unCC r.cc, label: resolveLabel r.label, kind: "slider", isToggle: false }
  Toggle r -> Just { ch, cc: unCC r.cc, label: r.label, kind: "toggle", isToggle: true }
  Momentary r -> Just { ch, cc: unCC r.cc, label: r.label, kind: "momentary", isToggle: false }
  Segmented r -> Just { ch, cc: unCC r.cc, label: r.label, kind: "segmented", isToggle: false }
  Dropdown r -> Just { ch, cc: unCC r.cc, label: r.label, kind: "dropdown", isToggle: false }
  _ -> Nothing
  where
  resolveLabel = case _ of
    Static s -> s
    ModeMap r -> "CC " <> show (unCC r.cc)
    ChannelMode _ -> "CC (mode)"

renderDictControl :: forall m. State -> DictEntry -> H.ComponentHTML Action () m
renderDictControl _state entry =
  HH.span [ HP.class_ (H.ClassName "controls-dict-control") ]
    [ HH.span [ HP.class_ (H.ClassName "controls-dict-control-label") ]
        [ HH.text (entry.label <> " CC" <> show entry.cc) ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser entry.ch entry.cc true
        ]
        [ HH.text "+Tgl" ]
    , HH.button
        [ HP.class_ (H.ClassName "controls-btn-tiny")
        , HE.onClick \_ -> AddFromBrowser entry.ch entry.cc false
        ]
        [ HH.text "+Mom" ]
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
  UpdateLooper swIdx b -> do
    modifySwitch swIdx \sw ->
      let without = Array.filter (\m -> m.msgType /= MsgLooperMode) sw.messages
      in sw { messages = if b then without <> [looperModeMessage] else without }
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

  -- CC Search + Dictionary
  UpdateSearch s -> H.modify_ _ { searchQuery = s }
  ToggleDictionary -> H.modify_ \st -> st { showDictionary = not st.showDictionary }
  ToggleDictPedal pid -> H.modify_ \st ->
    if Array.elem pid st.expandedPedals
      then st { expandedPedals = Array.filter (_ /= pid) st.expandedPedals }
      else st { expandedPedals = st.expandedPedals <> [pid] }
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

hasLooperMode :: ControlBankSwitch -> Boolean
hasLooperMode sw = Array.any (\m -> m.msgType == MsgLooperMode) sw.messages

looperModeMessage :: MC6Message
looperModeMessage =
  { msgType: MsgLooperMode
  , channel: 0
  , data1: 0
  , data2: 0
  , data3: 0
  , data4: 0
  , action: ActionPress
  , togglePosition: ToggleBoth
  , msgIndex: 0
  }

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
