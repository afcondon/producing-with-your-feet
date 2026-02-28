module Component.Loopy.Panel
  ( component
  , Output(..)
  , Input
  , Slot
  ) where

import Prelude

import Component.Loopy.ClipSettings as ClipSettings
import Data.Array as Array
import Data.Const (Const)
import Data.Loopy as Loopy
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Engine (MidiConnections)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot = H.Slot (Const Void) Output

type Input =
  { connections :: MidiConnections
  , loopyTwisterActive :: Boolean
  , selectedLoop :: Int
  , loopStates :: Array Loopy.LoopState
  , heldEncoder :: Maybe Int
  , clipSettings :: Array Loopy.ClipSettings
  }

data Output
  = LoopSelected Int
  | ActionFired Loopy.LoopyAction
  | TwisterModeToggled
  | GenerateProject
  | ClipSettingChanged Int Loopy.ClipSettings

type ChildSlots =
  ( clipSettings :: ClipSettings.Slot Unit
  )

type State =
  { input :: Input
  }

data Action
  = Receive Input
  | ClickLoop Int
  | ClickAction Loopy.LoopyAction
  | ClickTitle
  | ClickGenerate
  | HandleClipSettings ClipSettings.Output

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

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div [ HP.class_ (H.ClassName "loopy-panel") ]
    [ HH.div
        [ HP.class_ (H.ClassName titleClass)
        , HE.onClick \_ -> ClickTitle
        ]
        [ HH.span_ [ HH.text titleText ]
        , HH.button
            [ HP.class_ (H.ClassName "loopy-action-btn loopy-generate-btn")
            , HE.onClick \_ -> ClickGenerate
            ]
            [ HH.text ".lpproj" ]
        ]
    , HH.div [ HP.class_ (H.ClassName "loopy-grid") ]
        (map renderGroup Loopy.groups)
    , HH.div [ HP.class_ (H.ClassName "loopy-actions") ]
        (map renderAction Loopy.actions)
    , HH.slot (Proxy :: _ "clipSettings") unit ClipSettings.component
        { settings: currentClipSettings
        , loopColor: selectedLoopColor
        }
        HandleClipSettings
    ]
  where
  selectedIdx = state.input.selectedLoop
  currentClipSettings = fromMaybe Loopy.defaultClipSettings
    (Array.index state.input.clipSettings selectedIdx)
  selectedLoopColor = colorForLoop selectedIdx

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
        aClip = Array.index state.input.clipSettings aIdx
        bClip = Array.index state.input.clipSettings bIdx
    in HH.div [ HP.class_ (H.ClassName "loopy-group") ]
      [ loopButton group.loopA group.color "A" aState aClip
      , loopButton group.loopB group.color "B" bState bClip
      ]

  loopButton (Loopy.LoopIndex idx) color label mLoopState mClipSettings =
    let isSelected = state.input.selectedLoop == idx
        style = "border-color: " <> color.color
                <> if isSelected
                     then "; background: " <> color.color <> "; color: #fff"
                     else "; color: " <> color.color
        -- Clip badges: one-shot and retrospective
        isOneShot = case mClipSettings of
          Just cs -> not cs.loop
          Nothing -> false
        isRetro = case mClipSettings of
          Just cs -> cs.retrospective
          Nothing -> false
        -- Loop state details
        vol = case mLoopState of
          Just ls | ls.volume /= 100 -> Just (show ls.volume)
          _ -> Nothing
        spd = case mLoopState of
          Just ls | ls.speed /= 64 -> Just ("s" <> show ls.speed)
          _ -> Nothing
        isMuted = case mLoopState of
          Just ls -> ls.muted
          Nothing -> false
        isSoloed = case mLoopState of
          Just ls -> ls.soloed
          Nothing -> false
        isCleared = case mLoopState of
          Just ls -> ls.cleared
          Nothing -> false
    in HH.button
      [ HP.class_ (H.ClassName (if isSelected then "loopy-loop selected" else "loopy-loop"))
      , HP.attr (HH.AttrName "style") style
      , HE.onClick \_ -> ClickLoop idx
      ]
      ( -- Top-left badge: one-shot
        (if isOneShot then [ HH.span [ HP.class_ (H.ClassName "loop-badge loop-badge-tl") ] [ HH.text "1\x00d7" ] ] else [])
        -- Top-right badge: retrospective
        <> (if isRetro then [ HH.span [ HP.class_ (H.ClassName "loop-badge loop-badge-tr") ] [ HH.text "R" ] ] else [])
        -- Center: letter
        <> [ HH.span [ HP.class_ (H.ClassName "loop-letter") ] [ HH.text label ] ]
        -- Below letter: volume (when non-default)
        <> (case vol of
              Just v -> [ HH.span [ HP.class_ (H.ClassName "loop-vol") ] [ HH.text v ] ]
              Nothing -> [])
        -- Below volume: speed (when non-default)
        <> (case spd of
              Just s -> [ HH.span [ HP.class_ (H.ClassName "loop-spd") ] [ HH.text s ] ]
              Nothing -> [])
        -- Flags row
        <> (if isMuted || isSoloed || isCleared
              then [ HH.span [ HP.class_ (H.ClassName "loop-flags") ]
                ( (if isMuted then [ HH.span [ HP.class_ (H.ClassName "loopy-flag loopy-flag-muted") ] [ HH.text "M" ] ] else [])
                  <> (if isSoloed then [ HH.span [ HP.class_ (H.ClassName "loopy-flag loopy-flag-soloed") ] [ HH.text "S" ] ] else [])
                  <> (if isCleared then [ HH.span [ HP.class_ (H.ClassName "loopy-flag loopy-flag-cleared") ] [ HH.text "C" ] ] else [])
                ) ]
              else [])
      )

  renderAction def =
    HH.button
      [ HP.class_ (H.ClassName "loopy-action-btn")
      , HE.onClick \_ -> ClickAction def.action
      ]
      [ HH.text def.label ]

-- | Look up the color for a loop index from the groups data
colorForLoop :: Int -> String
colorForLoop idx =
  let groupIdx = idx / 2
  in case Array.index Loopy.groups groupIdx of
    Just g -> g.color.color
    Nothing -> "#999"

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Receive input -> H.modify_ _ { input = input }
  ClickLoop idx -> H.raise (LoopSelected idx)
  ClickAction action -> H.raise (ActionFired action)
  ClickTitle -> H.raise TwisterModeToggled
  ClickGenerate -> H.raise GenerateProject
  HandleClipSettings (ClipSettings.ClipSettingChanged settings) -> do
    st <- H.get
    H.raise (ClipSettingChanged st.input.selectedLoop settings)
