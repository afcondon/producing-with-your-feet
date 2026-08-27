-- | From a footswitch to the daemon: the looper's adapter.
-- |
-- | ## What this is
-- |
-- | `Data.Looper.Machine` decides what a gesture *means* and returns a list of
-- | `Machine.Action`s. Something has to carry those out — write to the socket,
-- | move the focus, put the refusal on screen. That is this module, and it is
-- | the whole of the looper's route to the outside world.
-- |
-- | It lived in `Component.App` until 2026-08-27, where the four functions sat
-- | between the MC6's SysEx uploader and the folder backup and nothing said
-- | they were one thing.
-- |
-- | ## Why `ShowBank` is an argument
-- |
-- | Every action here is the looper's own business except one. `Machine.ShowBank`
-- | asks the *pedalboard* to turn to a page — a courtesy to a device this module
-- | has no business knowing about, which needs an editor session, which is held
-- | by whoever owns the MC6 wire.
-- |
-- | So it is injected, and the type is the argument for the split: this module
-- | is polymorphic in the action and the slot row, which means it **cannot**
-- | dispatch an app action or query a child component even by accident. The
-- | looper reaches the daemon by itself and asks the hub for the favour.
-- |
-- | ## The rule that survives the move
-- |
-- | `runAction` is the only writer to `LooperSocket.send` in the app. Screen
-- | button, footswitch and Twister encoder all arrive as a `Duty`, go through
-- | `Machine.perform`, and come out here. A second route to the socket is the
-- | fault this whole shape exists to prevent — it has been built twice and
-- | removed twice.
module Component.Looper.Control
  ( ShowBank
  , runGesture
  , runAction
  , followBoard
  , deferralOf
  ) where

import Prelude

import Component.Twister.Lights (rigOf)
import Data.Int as Int
import Data.Looper.Banks as LoopBanks
import Data.Looper.Machine as Machine
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Engine (AppState)
import Foreign.LooperSocket as LooperSocket
import Halogen as H

-- | Ask the pedalboard to show one of the loop machine's banks.
-- |
-- | Supplied by the caller because it needs an MC6 editor session. Forked by
-- | the caller too, and that is not an implementation detail: the loop closes
-- | and plays on the engine's own schedule, opening a session takes the better
-- | part of a second, and audio must never wait on the display.
type ShowBank act slots o m =
  LoopBanks.BankSlot -> H.HalogenM AppState act slots o m Unit

-- | What a gesture means, and then doing it.
-- |
-- | The meaning is a pure function of the gesture and the *daemon's* report of
-- | the loops — this app models no loop state of its own, so there is nothing
-- | here that can fall out of step with the engine.
runGesture
  :: forall act slots o m. MonadAff m
  => ShowBank act slots o m
  -> LoopBanks.SwitchGesture -> H.HalogenM AppState act slots o m Unit
runGesture showBank g = do
  st <- H.get
  let rig = rigOf st
  followBoard g
  traverse_ (runAction showBank (deferralOf st.looperDeferral g)) (Machine.act rig g)

-- | How late this command already is, before it has gone anywhere.
-- |
-- | The daemon spends it where a frame matters (`@ms` in its dispatch) and
-- | strips it everywhere else, so everything can be stamped without the app
-- | having to know which commands care.
-- |
-- | **It is the device's own threshold rather than a measurement**, and that is
-- | what moving gesture recognition onto the MC6 cost. The app used to see the
-- | switch go down and could subtract; now it sees one message that the device
-- | withheld until it knew, and the length of that wait is a setting rather than
-- | an observation. See `Engine.looperDeferral` for what each number is worth.
-- |
-- | **Except when there was no wait at all.** A switch carrying one meaning is
-- | programmed on `ActionPress`, which the device fires at press-down — so its
-- | tap is not late, and saying it was would have the daemon reach back into the
-- | pre-roll ring for time that has not passed. That is a fact about how the
-- | switch was programmed, so it is read from the table that programmed it.
deferralOf
  :: { tapMs :: Number, holdMs :: Number } -> LoopBanks.SwitchGesture -> Number
deferralOf d g
  | LoopBanks.firesAtPressDown g.slot g.switch g.gesture = 0.0
  | otherwise = case g.gesture of
      LoopBanks.Tap -> d.tapMs
      -- The window again, measured from the second press. Doubles used to be
      -- dated from the *first* of the pair, on the grounds that it is where the
      -- player committed; that is no longer knowable, and a double tap is not
      -- the gesture anything sample-critical hangs on.
      LoopBanks.Double -> d.tapMs
      LoopBanks.Hold -> d.holdMs

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
  :: forall act slots o m. MonadAff m
  => LoopBanks.SwitchGesture -> H.HalogenM AppState act slots o m Unit
followBoard g =
  H.modify_ _ { looperBankShown = case LoopBanks.sendsTo g.slot g.switch g.gesture of
      Just (LoopBanks.ToSlot to) -> Just to
      Just LoopBanks.ToBoard -> Nothing
      -- Not a navigating switch, so the board stayed where the press came from.
      Nothing -> Just g.slot
    }

runAction
  :: forall act slots o m. MonadAff m
  => ShowBank act slots o m
  -> Number -> Machine.Action -> H.HalogenM AppState act slots o m Unit
runAction showBank late a = do
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
    -- The one action this module cannot carry out itself, and deliberately:
    -- reaching the MC6 means an editor session, and sessions belong to whoever
    -- owns that wire. See `ShowBank`.
    Machine.ShowBank slot -> do
      note (Machine.describe a)
      showBank slot
    Machine.Unavailable why -> note why
    Machine.Handled what -> note what
  where
  note msg = H.modify_ _ { looperLastAction = Just msg }
