module Foreign.WebMIDI
  ( MIDIAccess
  , MIDIOutput
  , MIDIInput
  , MidiPort
  , requestMIDIAccess
  , getOutputs
  , getInputs
  , openOutput
  , openInput
  , send
  , sendCC
  , sendPC
  , onMessage
  , onStateChange
  , randomUUID
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Midi (CC, Channel, MidiValue, ProgramNumber, unCC, unChannel, unMidiValue, unProgramNumber)
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

foreign import data MIDIAccess :: Type
foreign import data MIDIOutput :: Type
foreign import data MIDIInput :: Type

type MidiPort =
  { id :: String
  , name :: String
  }

foreign import requestMIDIAccessImpl
  :: (MIDIAccess -> Effect Unit)
  -> (Error -> Effect Unit)
  -> Effect Unit

foreign import getOutputsImpl :: MIDIAccess -> Effect (Array MidiPort)
foreign import getInputsImpl :: MIDIAccess -> Effect (Array MidiPort)

foreign import openOutputImpl
  :: (MIDIOutput -> Maybe MIDIOutput)
  -> Maybe MIDIOutput
  -> MIDIAccess
  -> String
  -> Effect (Maybe MIDIOutput)

foreign import openInputImpl
  :: (MIDIInput -> Maybe MIDIInput)
  -> Maybe MIDIInput
  -> MIDIAccess
  -> String
  -> Effect (Maybe MIDIInput)

foreign import sendImpl :: MIDIOutput -> Array Int -> Effect Unit
foreign import onMessageImpl :: MIDIInput -> (Array Int -> Effect Unit) -> Effect (Effect Unit)
foreign import onStateChangeImpl :: MIDIAccess -> Effect Unit -> Effect (Effect Unit)
foreign import randomUUID :: Effect String

requestMIDIAccess :: Aff MIDIAccess
requestMIDIAccess = makeAff \cb -> do
  requestMIDIAccessImpl
    (\access -> cb (Right access))
    (\err -> cb (Left err))
  pure nonCanceler

getOutputs :: MIDIAccess -> Effect (Array MidiPort)
getOutputs = getOutputsImpl

getInputs :: MIDIAccess -> Effect (Array MidiPort)
getInputs = getInputsImpl

openOutput :: MIDIAccess -> String -> Effect (Maybe MIDIOutput)
openOutput = openOutputImpl Just Nothing

openInput :: MIDIAccess -> String -> Effect (Maybe MIDIInput)
openInput = openInputImpl Just Nothing

send :: MIDIOutput -> Array Int -> Effect Unit
send = sendImpl

sendCC :: MIDIOutput -> Channel -> CC -> MidiValue -> Effect Unit
sendCC output ch ccNum val =
  send output [ 0xB0 + (unChannel ch - 1), unCC ccNum, unMidiValue val ]

sendPC :: MIDIOutput -> Channel -> ProgramNumber -> Effect Unit
sendPC output ch pc =
  send output [ 0xC0 + (unChannel ch - 1), unProgramNumber pc ]

onMessage :: MIDIInput -> (Array Int -> Effect Unit) -> Effect (Effect Unit)
onMessage = onMessageImpl

onStateChange :: MIDIAccess -> Effect Unit -> Effect (Effect Unit)
onStateChange = onStateChangeImpl
