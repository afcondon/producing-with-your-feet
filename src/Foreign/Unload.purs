-- | Run something on the way out of the page.
-- |
-- | For state that lives on a device rather than in the browser: an editor
-- | session held open on the MC6 outlives the tab that opened it, so closing
-- | the tab without closing the session leaves the instrument in a mode nobody
-- | asked for and nothing to say so. A reload during development does it every
-- | time.
-- |
-- | Deliberately a plain `Effect` callback rather than a Halogen subscription.
-- | `beforeunload` handlers only get to do synchronous work — the page is going
-- | away — and a subscription dispatches through Aff, so the frames would be
-- | queued behind an unload that has already happened.
module Foreign.Unload
  ( onBeforeUnload
  ) where

import Prelude

import Effect (Effect)

-- | Register a handler, and get back the action that removes it again.
-- |
-- | Returning the remover rather than leaving it registered matters here: the
-- | handler is only correct while a session is actually held, and one left
-- | installed after release would send a disconnect for a session that is
-- | already closed.
foreign import onBeforeUnload :: Effect Unit -> Effect (Effect Unit)
