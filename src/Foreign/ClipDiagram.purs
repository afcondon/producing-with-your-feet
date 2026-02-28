module Foreign.ClipDiagram
  ( renderClipDiagram
  ) where

import Prelude

import Data.Loopy (ClipSettings, CountInMode(..), CountOutMode(..), RecordEndAction(..), BeatQuantPreset(..))
import Effect (Effect)
import Web.DOM.Element (Element)

-- | Plain JS record matching the FFI's expected shape
type JsClipSettings =
  { countIn :: String
  , countOut :: String
  , autoCountOut :: Boolean
  , recordEndAction :: String
  , overdubFeedback :: Number
  , beatQuant :: String
  , phaseLocked :: Boolean
  , loop :: Boolean
  , threshold :: Boolean
  , intro :: Boolean
  , tail :: Boolean
  , retrospective :: Boolean
  }

foreign import renderClipDiagramImpl :: JsClipSettings -> String -> Element -> Effect Unit

toJs :: ClipSettings -> JsClipSettings
toJs s =
  { countIn: countInToStr s.countIn
  , countOut: countOutToStr s.countOut
  , autoCountOut: s.autoCountOut
  , recordEndAction: endToStr s.recordEndAction
  , overdubFeedback: s.overdubFeedback
  , beatQuant: beatQuantToStr s.beatQuant
  , phaseLocked: s.phaseLocked
  , loop: s.loop
  , threshold: s.threshold
  , intro: s.intro
  , tail: s.tail
  , retrospective: s.retrospective
  }
  where
  countInToStr = case _ of
    CountInNone -> "none"
    CountInMaster -> "master"
    CountInLoop -> "loop"
  countOutToStr = case _ of
    CountOutNone -> "none"
    CountOutMaster -> "master"
    CountOutLoop -> "loop"
  endToStr = case _ of
    EndPlay -> "play"
    EndStop -> "stop"
    EndOverdub -> "overdub"
  beatQuantToStr = case _ of
    BeatQuantOff -> "off"
    BeatQuant16Tight -> "16t"
    BeatQuant16Med -> "16m"
    BeatQuant16Loose -> "16l"
    BeatQuant32Tight -> "32t"
    BeatQuant32Med -> "32m"
    BeatQuant32Loose -> "32l"

-- | Render the clip settings SVG timeline diagram into a container element.
renderClipDiagram :: ClipSettings -> String -> Element -> Effect Unit
renderClipDiagram settings loopColor container =
  renderClipDiagramImpl (toJs settings) loopColor container
