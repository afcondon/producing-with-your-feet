-- | What a footswitch *means*, as opposed to what it sends.
-- |
-- | The MC6 knows only messages. A preset is a bag of up to sixteen of them and
-- | the device has no opinion about what they are for. That is why its own
-- | editor cannot help you: with nothing but channels and numbers there is
-- | nothing to organise, check or draw.
-- |
-- | A verb is the missing layer. Four of them, and they form a **scope ladder** —
-- | each changes strictly more of the rig than the one above:
-- |
-- |     Navigation   changes what every other switch means
-- |     Action       changes one control on one pedal
-- |     PedalPreset  changes one pedal entirely
-- |     Scene        changes the whole board
-- |
-- | The ladder is not decoration. Message cost rises with scope, so a view that
-- | colours by verb is also showing you which switches are expensive; and the
-- | verb decides whether a switch is timing-critical, which decides whether it
-- | may live on a long press (see `DESIGN-CONTROLS.md` §2).
-- |
-- | `Raw` is the fifth case and the reason this can be adopted without a
-- | migration. The MC6 has thirty-six message types and we model four verbs;
-- | anything unrecognised keeps its messages verbatim and still programs
-- | correctly. Nothing is lost by classifying, and nothing has to be
-- | re-authored to benefit.
module Data.MC6.Verb
  ( Verb(..)
  , NavTarget(..)
  , ActionShape(..)
  , classify
  , verbLabel
  , verbScope
  , isTimingCritical
  ) where

import Prelude

import Config.Registry (PedalRegistry)
import Config.Registry as CRegistry
import Data.Array as Array
import Data.MC6.Types (MC6Action(..), MC6Message, MC6MsgType(..), MC6TogglePosition(..))
import Data.Maybe (Maybe(..))
import Data.Pedal (PedalId)

-- | Where a navigation switch goes.
data NavTarget
  = ToBank Int
  | BankUp
  | BankDown
  | TogglePage

derive instance Eq NavTarget

-- | How an action behaves underfoot. The MC6 tracks toggle state itself, so
-- | this is a real behavioural difference rather than a labelling one.
data ActionShape
  = Momentary   -- ^ on press, off on release: holds, swells, retriggers
  | Toggling    -- ^ alternates between two positions: freeze, infinite, boost
  | OneShot     -- ^ fires and forgets: tap tempo, clear

derive instance Eq ActionShape

data Verb
  = Navigation NavTarget
  | Action { pedalId :: PedalId, cc :: Int, shape :: ActionShape }
  | PedalPreset { pedalId :: PedalId, program :: Int }
  | Scene { cc :: Int }
  -- | Messages we can program but not name. Carried verbatim.
  | Raw
  | Blank

derive instance Eq Verb

-- | How much of the rig this changes. Drives colour, and orders the legend.
verbScope :: Verb -> Int
verbScope = case _ of
  Blank -> 0
  Navigation _ -> 1
  Action _ -> 2
  PedalPreset _ -> 3
  Scene _ -> 4
  Raw -> 5

-- | Whether this verb must be on a short press.
-- |
-- | Only actions are timing-critical, and that is the whole argument for the
-- | press rule: a record punch or a swell has to land where you meant, and a
-- | long press is by construction late. Navigation can hide behind a hold
-- | precisely because arriving a moment later costs nothing.
isTimingCritical :: Verb -> Boolean
isTimingCritical = case _ of
  Action _ -> true
  _ -> false

verbLabel :: Verb -> String
verbLabel = case _ of
  Blank -> "empty"
  Navigation (ToBank n) -> "go to bank " <> show n
  Navigation BankUp -> "bank up"
  Navigation BankDown -> "bank down"
  Navigation TogglePage -> "toggle page"
  Action _ -> "action"
  PedalPreset p -> "pedal preset " <> show p.program
  Scene _ -> "scene"
  Raw -> "raw messages"

-- | Read a verb out of the messages a switch already carries.
-- |
-- | This is what makes the model adoptable: banks authored by hand in
-- | Morningstar's editor, years before any of this existed, classify without
-- | being touched. It leans on exactly the channel-to-pedal mapping that
-- | observation uses (`Engine.pedalsOnChannel`) — a CC on channel 3 is a MOOD
-- | CC whether we sent it or overheard it.
-- |
-- | Deliberately conservative: anything that does not match a shape cleanly
-- | becomes `Raw` rather than being forced into a verb. A wrong label on a
-- | footswitch is worse than no label, because it would be believed.
-- |
-- | `boardRecallChannel` is the app's own relay channel; a CC there is a board
-- | recall rather than a pedal control, since no pedal answers on it.
classify :: PedalRegistry -> Int -> Array MC6Message -> Verb
classify registry boardRecallChannel messages =
  case Array.filter (\m -> m.msgType /= MsgEmpty) messages of
    [] -> Blank

    [ m ] | m.msgType == MsgBankJump -> Navigation (ToBank m.data1)
    [ m ] | m.msgType == MsgBankUp -> Navigation BankUp
    [ m ] | m.msgType == MsgBankDown -> Navigation BankDown
    [ m ] | m.msgType == MsgTogglePage -> Navigation TogglePage

    -- A single CC on the relay channel: one press expanding to a whole board.
    [ m ] | m.msgType == MsgCC && m.channel == boardRecallChannel ->
      Scene { cc: m.data1 }

    [ m ] | m.msgType == MsgPC -> case pedalOn m.channel of
      Just pid -> PedalPreset { pedalId: pid, program: m.data1 }
      Nothing -> Raw

    -- One CC, no partner: fires and forgets.
    [ m ] | m.msgType == MsgCC -> case pedalOn m.channel of
      Just pid -> Action { pedalId: pid, cc: m.data1, shape: OneShot }
      Nothing -> Raw

    -- The two-message CC shapes. Both send 127 then 0 on the same CC; what
    -- separates them is *when* the 0 goes out — on release (momentary) or on
    -- the next press (toggle). Getting this backwards would turn a hold into a
    -- latch, which is why it is read from the messages rather than a flag.
    [ a, b ] | isPair a b -> case pedalOn a.channel of
      Just pid
        | a.togglePosition == ToggleOn && b.togglePosition == ToggleOff ->
            Action { pedalId: pid, cc: a.data1, shape: Toggling }
        | a.action == ActionPress && b.action == ActionRelease ->
            Action { pedalId: pid, cc: a.data1, shape: Momentary }
      _ -> Raw

    _ -> Raw
  where
  pedalOn :: Int -> Maybe PedalId
  pedalOn ch = map _.meta.id
    (Array.find (\d -> d.meta.defaultChannel == ch) (CRegistry.registryPedals registry))

  isPair a b =
    a.msgType == MsgCC && b.msgType == MsgCC
      && a.channel == b.channel
      && a.data1 == b.data1
      && a.data2 == 127
      && b.data2 == 0
