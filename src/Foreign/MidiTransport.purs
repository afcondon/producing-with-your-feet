-- | **Which way PWYF reaches its controllers**: the browser's Web MIDI, or
-- | Helm's hub (`music/helm/hub`), the daemon that holds the MC6 and the
-- | Twister so that PWYF and Quadrat can share them.
-- |
-- | Declared in the URL, never guessed: `?midi=hub` asks for the hub, and
-- | anything else is Web MIDI as before. A page that silently fell back from
-- | one to the other would be the two-owners problem the hub exists to end.
module Foreign.MidiTransport
  ( MidiTransport(..)
  , midiTransport
  , hubPort
  ) where

import Prelude

import Effect (Effect)

data MidiTransport
  = Browser
  -- | The hub's WebSocket, on the host the page came from.
  | Hub String

foreign import askedForHubImpl :: Effect Boolean
foreign import hostnameImpl :: Effect String

-- | The hub's port, as registered with Bosun.
hubPort :: Int
hubPort = 3039

midiTransport :: Effect MidiTransport
midiTransport = do
  hub <- askedForHubImpl
  if hub then do
    host <- hostnameImpl
    pure (Hub ("ws://" <> host <> ":" <> show hubPort))
  else pure Browser
