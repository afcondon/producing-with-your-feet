-- | From a footswitch to the daemon: the looper's adapter.
-- |
-- | ## What this is
-- |
-- | `Data.Looper.Machine` decides what a gesture *means* and returns a list of
-- | `Machine.Action`s. Something has to carry those out — write to the socket,
-- | move the focus, say what was refused. That is this module, and it is
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
  , deferralOf
  ) where

import Prelude

import Component.Twister.Lights (rigOf)
import Data.Int as Int
import Data.Looper.Banks as LoopBanks
import Data.Looper.Machine as Machine
import Data.Looper.Press as Press
import Data.Looper.Switchboard as Switchboard
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
  traverse_ (runAction showBank (deferralOf st.looperDeferral g)) (Switchboard.act rig g)

-- | How late this command already is: `Data.Looper.Press.deferralOf`, shared
-- | with the looper's own process.
deferralOf :: { tapMs :: Number, holdMs :: Number } -> LoopBanks.SwitchGesture -> Number
deferralOf = Press.deferralOf

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
      unless ok $ note ("no daemon — " <> c <> " went nowhere")
    -- Here at once, so the next press acts on it without waiting a frame,
    -- and told to the daemon, which holds the selection every surface on it
    -- shares (a bare loop number selects; the snapshot reports `selected`).
    Machine.Focus i -> do
      H.modify_ _ { looperFocus = i }
      void $ liftEffect $ LooperSocket.send (show i)
    -- **Forked on purpose.** The loop closes and plays on the engine's own
    -- schedule; the bank change is a courtesy that either lands or does not.
    -- Audio must never wait on the display, and opening an editor session takes
    -- the better part of a second.
    -- The one action this module cannot carry out itself, and deliberately:
    -- reaching the MC6 means an editor session, and sessions belong to whoever
    -- owns that wire. See `ShowBank`.
    Machine.ShowBank slot -> showBank slot
    -- Already said: the line above describes every action, these included.
    Machine.Unavailable _ -> pure unit
    Machine.Handled _ -> pure unit
  where
  -- **The console, since the looper's page left for Quadrat** (2026-11-07).
  -- The page's log was where these lines were read; Quadrat's Loop page reads
  -- the daemon's own acks, and what is left here is this app's fallback,
  -- which has no page of its own to speak on.
  note msg = liftEffect $ Console.log ("looper: " <> msg)
