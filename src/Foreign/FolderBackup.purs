module Foreign.FolderBackup
  ( BackupStatus
  , pickAndSetFolder
  , attemptReconnect
  , reconnectWithPrompt
  , disconnectFolder
  , saveBackupNow
  , scheduleBackup
  , getStatus
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

-- | Snapshot of the folder-backup state. `connected` is true when a handle is
-- | live (picked or successfully re-granted this session). `folderName` is the
-- | directory's display name. `lastSaveAt` is an ISO timestamp string, empty
-- | if no save has happened this session. `lastError` carries the most recent
-- | error message, empty if the last operation succeeded.
type BackupStatus =
  { connected :: Boolean
  , folderName :: String
  , lastSaveAt :: String
  , lastError :: String
  }

-- All async FFI returns a display string on success (folder name, or "" for
-- "no folder connected"). Errors are Error values.

foreign import pickAndSetFolderImpl
  :: (Error -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
foreign import attemptReconnectImpl
  :: (Error -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
foreign import reconnectWithPromptImpl
  :: (Error -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
foreign import disconnectFolderImpl
  :: (Error -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
foreign import saveBackupNowImpl
  :: (Error -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit
foreign import getStatusImpl :: Effect BackupStatus
foreign import scheduleBackupImpl :: Effect Unit

-- Helper: turn a callback-style FFI into Aff returning Maybe String.
-- Nothing means "no folder picked / cancelled"; Just name means connected.
aff :: ((Error -> Effect Unit) -> (String -> Effect Unit) -> Effect Unit)
    -> Aff (Maybe String)
aff k = makeAff \cb -> do
  k (\err -> cb (Left err))
    (\name -> cb (Right (if name == "" then Nothing else Just name)))
  pure nonCanceler

-- | Prompt the user to pick a directory. Returns the folder name on success,
-- | Nothing if they cancelled. Must be called from a user-gesture handler.
pickAndSetFolder :: Aff (Maybe String)
pickAndSetFolder = aff pickAndSetFolderImpl

-- | On app startup, try to reconnect silently using the handle stored in
-- | IndexedDB. Returns the folder name if permission is already granted,
-- | Nothing if there's no stored handle or the browser wants a fresh gesture.
attemptReconnect :: Aff (Maybe String)
attemptReconnect = aff attemptReconnectImpl

-- | Re-request permission for the stored handle. Needs a user gesture
-- | (button click). Returns the folder name if granted.
reconnectWithPrompt :: Aff (Maybe String)
reconnectWithPrompt = aff reconnectWithPromptImpl

-- | Clear the stored handle and disconnect. Always returns Nothing.
disconnectFolder :: Aff (Maybe String)
disconnectFolder = aff disconnectFolderImpl

-- | Immediately write a backup envelope to the connected folder. Writes
-- | latest.json unconditionally and history/YYYY-MM-DD.json if today's
-- | snapshot doesn't already exist. Returns the folder name on success.
saveBackupNow :: Aff (Maybe String)
saveBackupNow = aff saveBackupNowImpl

-- | Schedule a debounced auto-save (30 s after the last schedule call).
-- | Safe to call on every state change — a pending timer gets reset.
-- | No-op if no folder is connected.
scheduleBackup :: Effect Unit
scheduleBackup = scheduleBackupImpl

getStatus :: Effect BackupStatus
getStatus = getStatusImpl
