-- | Open a generated document in its own tab.
-- |
-- | **Its own tab, and that is the whole point.** The alternative is a print
-- | stylesheet over the app, and a print view of the app is a print view you
-- | have to be looking at — while Chrome throttles a background tab and the
-- | looper stops handling Twister messages. The sheet is for the paper beside
-- | the rig; the app keeps focus.
module Foreign.Sheet
  ( openSheet
  ) where

import Effect (Effect)

-- | Returns whether a window actually opened.
-- |
-- | **Reported rather than assumed**, because a blocked popup is silent: the
-- | call returns, nothing throws, and no tab appears. That is the exact shape
-- | of failure this project has been caught by more than once — the ack path is
-- | usually the bug — so the answer comes back and the caller says so.
foreign import openSheetImpl :: String -> Effect Boolean

openSheet :: String -> Effect Boolean
openSheet = openSheetImpl
