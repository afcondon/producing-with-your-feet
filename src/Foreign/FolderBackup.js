// FolderBackup FFI — Chrome File System Access API + IndexedDB handle persistence.
//
// The app backs up its localStorage state to a user-granted directory. The handle
// is kept in IndexedDB so subsequent page loads can reconnect silently (if the
// browser grants permission without a fresh user gesture) or via a Reconnect button.
//
// Public surface (callback-style, to fit the existing Foreign.FileIO FFI idiom):
//   pickAndSetFolderImpl(onError, onSuccess)
//   attemptReconnectImpl(onError, onSuccess)
//   reconnectWithPromptImpl(onError, onSuccess)
//   disconnectFolderImpl(onError, onSuccess)
//   saveBackupNowImpl(onError, onSuccess)
//   getStatusImpl — synchronous, returns { connected, folderName, lastSaveAt, lastError }
//
// onSuccess callbacks receive the display folder name (or empty string when
// disconnected / cancelled), so a single shape works everywhere.

const DB_NAME  = 'pwyf-backup';
const DB_STORE = 'handles';
const HANDLE_KEY = 'backup-dir';

const state = {
  handle: null,
  lastSaveAt: null,
  lastError: null,
};

// -----------------------------------------------------------------------------
// IndexedDB tiny helpers
// -----------------------------------------------------------------------------

const openIDB = () => new Promise((resolve, reject) => {
  const req = indexedDB.open(DB_NAME, 1);
  req.onupgradeneeded = () => {
    if (!req.result.objectStoreNames.contains(DB_STORE)) {
      req.result.createObjectStore(DB_STORE);
    }
  };
  req.onsuccess = () => resolve(req.result);
  req.onerror = () => reject(req.error);
});

const idbPut = async (key, value) => {
  const db = await openIDB();
  await new Promise((resolve, reject) => {
    const tx = db.transaction(DB_STORE, 'readwrite');
    tx.objectStore(DB_STORE).put(value, key);
    tx.oncomplete = resolve;
    tx.onerror = () => reject(tx.error);
  });
  db.close();
};

const idbGet = async (key) => {
  const db = await openIDB();
  const result = await new Promise((resolve, reject) => {
    const tx = db.transaction(DB_STORE, 'readonly');
    const req = tx.objectStore(DB_STORE).get(key);
    req.onsuccess = () => resolve(req.result ?? null);
    req.onerror = () => reject(req.error);
  });
  db.close();
  return result;
};

const idbDelete = async (key) => {
  const db = await openIDB();
  await new Promise((resolve, reject) => {
    const tx = db.transaction(DB_STORE, 'readwrite');
    tx.objectStore(DB_STORE).delete(key);
    tx.oncomplete = resolve;
    tx.onerror = () => reject(tx.error);
  });
  db.close();
};

// -----------------------------------------------------------------------------
// Backup envelope — single JSON with every pedal-explorer-* localStorage key.
// Values are stored parsed so the file is inspectable; restore re-stringifies.
// -----------------------------------------------------------------------------

const makeBackupEnvelope = () => {
  const storage = {};
  for (let i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (!key || !key.startsWith('pedal-explorer-')) continue;
    const raw = localStorage.getItem(key);
    try { storage[key] = JSON.parse(raw); }
    catch { storage[key] = raw; }  // fall back to raw string if unparseable
  }
  return JSON.stringify({
    version: 1,
    exportedAt: new Date().toISOString(),
    origin: location.origin,
    storage,
  }, null, 2);
};

// -----------------------------------------------------------------------------
// Write latest.json + history/YYYY-MM-DD.json (history only if today missing)
// -----------------------------------------------------------------------------

const writeToHandle = async (handle, content) => {
  // latest.json (always overwrite)
  const latest = await handle.getFileHandle('latest.json', { create: true });
  const latestW = await latest.createWritable();
  await latestW.write(content);
  await latestW.close();

  // history/YYYY-MM-DD.json — idempotent (don't overwrite if today's file exists)
  try {
    const today = new Date().toISOString().slice(0, 10);
    const dir = await handle.getDirectoryHandle('history', { create: true });
    let exists = false;
    try {
      await dir.getFileHandle(`${today}.json`, { create: false });
      exists = true;
    } catch (e) {
      if (e.name !== 'NotFoundError') throw e;
    }
    if (!exists) {
      const hf = await dir.getFileHandle(`${today}.json`, { create: true });
      const hw = await hf.createWritable();
      await hw.write(content);
      await hw.close();
    }
  } catch (e) {
    // history failures shouldn't block the primary write — log and continue
    console.warn('[PWYF folder-backup] history write failed:', e);
  }
};

// -----------------------------------------------------------------------------
// Public FFI — each function is (onError, onSuccess) => Effect Unit
// onSuccess receives a display string (folder name or empty string for "no folder")
// -----------------------------------------------------------------------------

export const pickAndSetFolderImpl = function (onError) {
  return function (onSuccess) {
    return function () {
      (async () => {
        try {
          if (!window.showDirectoryPicker) {
            throw new Error('File System Access API unavailable (try Chrome).');
          }
          const handle = await window.showDirectoryPicker({ mode: 'readwrite' });
          await idbPut(HANDLE_KEY, handle);
          state.handle = handle;
          state.lastError = null;
          onSuccess(handle.name)();
        } catch (e) {
          if (e && e.name === 'AbortError') {
            // user cancelled — surface an empty string, not an error
            onSuccess('')();
            return;
          }
          state.lastError = e.message || String(e);
          onError(new Error(state.lastError))();
        }
      })();
    };
  };
};

export const attemptReconnectImpl = function (onError) {
  return function (onSuccess) {
    return function () {
      (async () => {
        try {
          const stored = await idbGet(HANDLE_KEY);
          if (!stored) { onSuccess('')(); return; }
          const perm = await stored.queryPermission({ mode: 'readwrite' });
          if (perm === 'granted') {
            state.handle = stored;
            state.lastError = null;
            onSuccess(stored.name)();
          } else {
            // Permission not automatic — user needs to click Reconnect.
            // Keep the stored handle in IDB but leave state.handle null.
            onSuccess('')();
          }
        } catch (e) {
          state.lastError = e.message || String(e);
          onError(new Error(state.lastError))();
        }
      })();
    };
  };
};

export const reconnectWithPromptImpl = function (onError) {
  return function (onSuccess) {
    return function () {
      (async () => {
        try {
          const stored = await idbGet(HANDLE_KEY);
          if (!stored) { onSuccess('')(); return; }
          const perm = await stored.requestPermission({ mode: 'readwrite' });
          if (perm === 'granted') {
            state.handle = stored;
            state.lastError = null;
            onSuccess(stored.name)();
          } else {
            onSuccess('')();
          }
        } catch (e) {
          state.lastError = e.message || String(e);
          onError(new Error(state.lastError))();
        }
      })();
    };
  };
};

export const disconnectFolderImpl = function (onError) {
  return function (onSuccess) {
    return function () {
      (async () => {
        try {
          state.handle = null;
          state.lastSaveAt = null;
          state.lastError = null;
          await idbDelete(HANDLE_KEY);
          onSuccess('')();
        } catch (e) {
          onError(new Error(e.message || String(e)))();
        }
      })();
    };
  };
};

export const saveBackupNowImpl = function (onError) {
  return function (onSuccess) {
    return function () {
      (async () => {
        try {
          if (!state.handle) {
            throw new Error('No backup folder connected.');
          }
          const envelope = makeBackupEnvelope();
          await writeToHandle(state.handle, envelope);
          state.lastSaveAt = new Date().toISOString();
          state.lastError = null;
          onSuccess(state.handle.name)();
        } catch (e) {
          state.lastError = e.message || String(e);
          onError(new Error(state.lastError))();
        }
      })();
    };
  };
};

export const getStatusImpl = function () {
  return {
    connected:   !!state.handle,
    folderName:  state.handle ? state.handle.name : '',
    lastSaveAt:  state.lastSaveAt || '',
    lastError:   state.lastError || '',
  };
};

// -----------------------------------------------------------------------------
// Phase 2 hook — debounced auto-save (wired in later)
// -----------------------------------------------------------------------------

let debounceTimer = null;
const DEBOUNCE_MS = 30_000;

export const scheduleBackupImpl = function () {
  return function () {
    if (!state.handle) return;  // no-op when disconnected
    if (debounceTimer) clearTimeout(debounceTimer);
    debounceTimer = setTimeout(() => {
      debounceTimer = null;
      (async () => {
        try {
          const envelope = makeBackupEnvelope();
          await writeToHandle(state.handle, envelope);
          state.lastSaveAt = new Date().toISOString();
          state.lastError = null;
        } catch (e) {
          state.lastError = e.message || String(e);
          console.warn('[PWYF folder-backup] auto-save failed:', e);
        }
      })();
    }, DEBOUNCE_MS);
  };
};
