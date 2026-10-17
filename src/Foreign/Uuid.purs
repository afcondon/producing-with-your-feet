-- | A fresh identifier from the platform's own generator. It used to live in
-- | `Foreign.WebMIDI` only because that module already owned a `.js` file;
-- | the MIDI layer is Helm's now and this is not MIDI.
module Foreign.Uuid (randomUUID) where

import Effect (Effect)

foreign import randomUUIDImpl :: Effect String

randomUUID :: Effect String
randomUUID = randomUUIDImpl
