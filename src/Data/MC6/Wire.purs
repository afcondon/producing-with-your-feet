-- | Getting frames to the MC6 in a context it will accept them in.
-- |
-- | `Data.MC6.SysEx` says what the bytes are; this says when they may leave.
-- | The two were the same job until a bank change went out on its own and the
-- | device ignored it — correct bytes, correct checksum, addressed to an editor
-- | session that had never been opened. Nothing reported anything, because from
-- | the wire's point of view nothing had gone wrong.
-- |
-- | So the bracket is no longer something to remember. Sending a `Frame Session`
-- | needs an `Open`, sending a `Frame Upload` needs an `Uploading`, and neither
-- | can be built outside this module. Forgetting to connect is a type error.
module Data.MC6.Wire
  ( Open
  , Uploading
  , sendLoose
  , sendAck
  , send
  , sendUpload
  , withSession
  , withUpload
  , openSession
  , closeSession
  ) where

import Prelude

import Data.MC6.SysEx as SysEx
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Foreign.WebMIDI as MIDI

-- | Proof that an editor session is open on this output, and therefore the
-- | right to send session frames through it.
-- |
-- | A token rather than a `Frame Session -> m Unit` function, because a held
-- | session has to be *stored* — the app keeps one in its state while the board
-- | is in use — and a function closed over a monad cannot be a field of the
-- | state that monad operates on.
newtype Open = Open MIDI.MIDIOutput

-- | Proof that an upload is open, which the device only grants inside a session.
-- | `withUpload` takes an `Open`, so that nesting is a fact about the types
-- | rather than about the order two calls happen to appear in.
newtype Uploading = Uploading MIDI.MIDIOutput

-- | The one place bytes go out, and therefore the one place they are logged.
-- | Private: exported polymorphically in `c` it would hand anyone the right to
-- | send a session frame outside a session, which is the whole thing being
-- | prevented.
emit :: forall c m. MonadEffect m => MIDI.MIDIOutput -> SysEx.Frame c -> m Unit
emit out frame = liftEffect do
  Console.log $ "MC6 SysEx SEND [" <> SysEx.frameLabel frame <> "]: "
    <> SysEx.toHexString (SysEx.frameBytes frame)
  MIDI.send out (SysEx.frameBytes frame)

-- | Send a frame that needs no session. Only connect and disconnect qualify, so
-- | in practice this is the editor handshake used as a cable test.
sendLoose :: forall m. MonadEffect m => MIDI.MIDIOutput -> SysEx.Frame SysEx.Loose -> m Unit
sendLoose = emit

-- | Acknowledge a frame that arrived, echoing its checksum.
-- |
-- | Outside the brackets because it is not something we decide to do: the device
-- | streams and waits to be told each frame landed, so this is flow control
-- | answering an arrival. Takes the checksum rather than a frame, so there is
-- | nothing to build and nothing to get wrong at the call site.
sendAck :: forall m. MonadEffect m => MIDI.MIDIOutput -> Int -> m Unit
sendAck out cs = emit out (SysEx.sysexAcknowledge cs)

send :: forall m. MonadEffect m => Open -> SysEx.Frame SysEx.Session -> m Unit
send (Open out) = emit out

sendUpload :: forall m. MonadEffect m => Uploading -> SysEx.Frame SysEx.Upload -> m Unit
sendUpload (Uploading out) = emit out

-- | Open a session and leave it open.
-- |
-- | For the case the bracket cannot express: a session held across many user
-- | actions, which is how Morningstar's own editor works and the only way the
-- | device will accept bank changes while the board is being played. Whoever
-- | calls this owns closing it — that is the price of a session outliving a
-- | callback, and it is why `withSession` remains the default.
-- |
-- | **Closes before it opens.** A session lives on the device, not in this tab,
-- | so a reload leaves one held by nobody — and every previous version of this
-- | assumed the MC6 was idle when we arrived, which is precisely the assumption
-- | this module exists to stop making. A disconnect with nothing to disconnect
-- | is the same frame that ends every read, so it costs one message and a
-- | settle to start from a state we chose rather than one we inherited.
-- |
-- | It also makes a pre-existing session *detectable*: the device answers a real
-- | disconnect with `EditorMode false`, so a caller watching for that learns
-- | that something else — Morningstar's editor in another tab — had the device.
openSession :: forall m. MonadAff m => MIDI.MIDIOutput -> m Open
openSession out = do
  emit out SysEx.sysexDisconnect
  liftAff (delay (Milliseconds 150.0))
  emit out SysEx.sysexConnect
  liftAff (delay (Milliseconds 250.0))
  pure (Open out)

closeSession :: forall m. MonadEffect m => Open -> m Unit
closeSession (Open out) = emit out SysEx.sysexDisconnect

-- | Open an editor session, do something in it, close it.
-- |
-- | Deliberately does not wait for the device to prove the session is live:
-- | what counts as proof is the caller's business — a bank read waits for bank
-- | names, a jump waits for a bank report — and a fixed delay pretending to be
-- | a handshake is how the silent version of this bug survived. The settle is
-- | only long enough that the first request does not arrive before connect.
withSession :: forall m a. MonadAff m => MIDI.MIDIOutput -> (Open -> m a) -> m a
withSession out action = do
  open <- openSession out
  a <- action open
  closeSession open
  pure a

-- | Open an upload inside a session, write, commit, close.
-- |
-- | A separate bracket from `withSession`, not a flag on it: asking a question
-- | should not be able to leave the device somewhere a half-finished write
-- | could land. The delays are the ones this has always used — the device
-- | answers `start` and `complete` and we do not yet wait for either, which is
-- | worth fixing but is not what this module is about.
withUpload :: forall m a. MonadAff m => Open -> (Uploading -> m a) -> m a
withUpload open@(Open out) action = do
  -- Keeps the total wait before the upload opens at the 500ms this path has
  -- always used; `openSession` has already settled for 250.
  liftAff (delay (Milliseconds 250.0))
  send open SysEx.sysexStartUpload
  liftAff (delay (Milliseconds 500.0))
  a <- action (Uploading out)
  liftAff (delay (Milliseconds 300.0))
  send open SysEx.sysexCompleteUpload
  liftAff (delay (Milliseconds 500.0))
  pure a
