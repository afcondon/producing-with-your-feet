-- | Filesystem side of pwyf-store.
-- |
-- | Every document is a file. Nothing is parsed beyond `jsonParser`, which is
-- | only used to hand the bytes back as JSON rather than a string — the store
-- | has no opinion about what a preset contains.
-- |
-- | Reads are forgiving: a missing directory or an unreadable file yields an
-- | empty result rather than a 500, because a store that has never been written
-- | to is a normal state on first run, not an error.
module Server.Store
  ( ensureLayout
  , snapshot
  , replaceAll
  , listPresets
  , writePreset
  , deletePreset
  , listPatches
  , writePatch
  , deletePatch
  , readAssignments
  , writeAssignments
  ) where

import Prelude

import Data.Argonaut.Core as AJ
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Foldable (sum)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, attempt)
import Foreign.Object as Object
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.FS.Perms (permsAll)
import Node.Path (concat)

presetsDir :: String -> String
presetsDir dir = concat [ dir, "presets" ]

patchesDir :: String -> String
patchesDir dir = concat [ dir, "patches" ]

assignmentsPath :: String -> String
assignmentsPath dir = concat [ dir, "assignments.json" ]

-- | mkdir -p, tolerant of already existing.
mkdirP :: String -> Aff Unit
mkdirP p = void $ attempt $ FS.mkdir' p { recursive: true, mode: permsAll }

ensureLayout :: String -> Aff Unit
ensureLayout dir = do
  mkdirP dir
  mkdirP (presetsDir dir)
  mkdirP (patchesDir dir)

-- | Directory entries, or none if the directory is absent.
lsSafe :: String -> Aff (Array String)
lsSafe p = do
  r <- attempt (FS.readdir p)
  pure case r of
    Left _ -> []
    Right xs -> xs

jsonFiles :: Array String -> Array String
jsonFiles = Array.filter (String.contains (String.Pattern ".json"))

-- | Read one file as JSON. Unreadable or malformed files are skipped rather
-- | than failing the whole listing — one corrupt preset should not make the
-- | library unreadable.
readJson :: String -> Aff (Maybe AJ.Json)
readJson p = do
  r <- attempt (FS.readTextFile UTF8 p)
  pure case r of
    Left _ -> Nothing
    Right txt -> hush (jsonParser txt)

writeJson :: String -> AJ.Json -> Aff Unit
writeJson p j = FS.writeTextFile UTF8 p (AJ.stringify j)

rmSafe :: String -> Aff Unit
rmSafe p = void $ attempt (FS.unlink p)

-- ---------------------------------------------------------------- presets

-- | Walks presets/<pedalId>/*.json across every pedal directory.
listPresets :: String -> Aff AJ.Json
listPresets dir = do
  pedals <- lsSafe (presetsDir dir)
  nested <- traverse (readPedalDir dir) pedals
  pure $ AJ.fromArray (Array.concat nested)

readPedalDir :: String -> String -> Aff (Array AJ.Json)
readPedalDir dir pedalId = do
  let d = concat [ presetsDir dir, pedalId ]
  files <- jsonFiles <$> lsSafe d
  ms <- traverse (\f -> readJson (concat [ d, f ])) files
  pure (Array.catMaybes ms)

writePreset :: String -> String -> String -> AJ.Json -> Aff Unit
writePreset dir pedalId presetId j = do
  let d = concat [ presetsDir dir, pedalId ]
  mkdirP d
  writeJson (concat [ d, presetId <> ".json" ]) j

deletePreset :: String -> String -> String -> Aff Unit
deletePreset dir pedalId presetId =
  rmSafe (concat [ presetsDir dir, pedalId, presetId <> ".json" ])

-- ---------------------------------------------------------------- patches

listPatches :: String -> Aff AJ.Json
listPatches dir = do
  files <- jsonFiles <$> lsSafe (patchesDir dir)
  ms <- traverse (\f -> readJson (concat [ patchesDir dir, f ])) files
  pure $ AJ.fromArray (Array.catMaybes ms)

writePatch :: String -> String -> AJ.Json -> Aff Unit
writePatch dir patchId j = do
  mkdirP (patchesDir dir)
  writeJson (concat [ patchesDir dir, patchId <> ".json" ]) j

deletePatch :: String -> String -> Aff Unit
deletePatch dir patchId =
  rmSafe (concat [ patchesDir dir, patchId <> ".json" ])

-- ------------------------------------------------------------ assignments

readAssignments :: String -> Aff AJ.Json
readAssignments dir = do
  m <- readJson (assignmentsPath dir)
  pure case m of
    Nothing -> AJ.fromArray []
    Just j -> j

writeAssignments :: String -> AJ.Json -> Aff Unit
writeAssignments dir j = do
  mkdirP dir
  writeJson (assignmentsPath dir) j

-- ---------------------------------------------------------------- snapshot

-- | Everything in one document. This is what the client pulls on load and
-- | keeps as its local cache, so the iPad survives the Mac being unreachable.
snapshot :: String -> Aff AJ.Json
snapshot dir = do
  presets <- listPresets dir
  patches <- listPatches dir
  assignments <- readAssignments dir
  pure $ AJ.fromObject $ Object.fromFoldable
    [ Tuple "version" (AJ.fromNumber 1.0)
    , Tuple "presets" presets
    , Tuple "patches" patches
    , Tuple "assignments" assignments
    ]

-- ------------------------------------------------------------- replaceAll

-- | "Make the store look like this."
-- |
-- | The client persists whole collections rather than individual records, so
-- | this is the write that matches how it already thinks. It reconciles rather
-- | than rewrites: unchanged files are still written, but files whose records
-- | have gone are deleted, so a removed preset actually disappears instead of
-- | lingering forever.
-- |
-- | This is the one place the store reads inside a document, and only for
-- | `id` and `pedalId` — it needs identity to know which file a record is.
-- | It still has no opinion about content.
-- |
-- | Two guards, because a reconciling write is one bug away from being an
-- | erase, and the bug it would be one away from is exactly the one that
-- | already cost a preset library:
-- |
-- |   * A collection ABSENT from the body is left alone. `{}` is a no-op,
-- |     not "delete everything".
-- |   * A collection present but EMPTY, against a non-empty store, is
-- |     refused unless `force`. A client that has failed to load its state
-- |     and then autosaves would otherwise wipe the store, and it would look
-- |     like a successful save.
-- |
-- | Deleting the last record deliberately is still possible one at a time
-- | through DELETE, or wholesale through the forcing route.
replaceAll :: String -> Boolean -> AJ.Json -> Aff (Either String Unit)
replaceAll dir force j = do
  let
    fld k = AJ.toObject j >>= Object.lookup k
    mArr k = fld k >>= AJ.toArray

  existingPresets <- countPresets dir
  existingPatches <- countPatches dir

  let
    wipes existing incoming = not force && existing > 0 && Array.null incoming
    presetWipe = maybe false (wipes existingPresets) (mArr "presets")
    patchWipe = maybe false (wipes existingPatches) (mArr "patches")

  if presetWipe || patchWipe then
    pure $ Left
      ( "refusing to empty a non-empty store: "
          <> (if presetWipe then show existingPresets <> " presets " else "")
          <> (if patchWipe then show existingPatches <> " patches " else "")
          <> "would be deleted. Retry against /api/snapshot/force if intended."
      )
  else do
    ensureLayout dir

    case mArr "presets" of
      Nothing -> pure unit
      Just presets -> do
        kept <- traverse (writeOnePreset dir) presets
        prunePresets dir (Array.catMaybes kept)

    case mArr "patches" of
      Nothing -> pure unit
      Just patches -> do
        kept <- traverse (writeOnePatch dir) patches
        prunePatches dir (Array.catMaybes kept)

    case fld "assignments" of
      Just a -> writeAssignments dir a
      Nothing -> pure unit

    pure (Right unit)

countPresets :: String -> Aff Int
countPresets dir = do
  pedals <- lsSafe (presetsDir dir)
  counts <- traverse
    (\p -> Array.length <<< jsonFiles <$> lsSafe (concat [ presetsDir dir, p ]))
    pedals
  pure (sum counts)

countPatches :: String -> Aff Int
countPatches dir = Array.length <<< jsonFiles <$> lsSafe (patchesDir dir)

strField :: String -> AJ.Json -> Maybe String
strField k j = AJ.toObject j >>= Object.lookup k >>= AJ.toString

-- | Returns the relative path written, so the prune step knows what to keep.
writeOnePreset :: String -> AJ.Json -> Aff (Maybe String)
writeOnePreset dir j = case strField "pedalId" j, strField "id" j of
  Just pedalId, Just presetId -> do
    writePreset dir pedalId presetId j
    pure (Just (pedalId <> "/" <> presetId <> ".json"))
  _, _ -> pure Nothing

writeOnePatch :: String -> AJ.Json -> Aff (Maybe String)
writeOnePatch dir j = case strField "id" j of
  Just patchId -> do
    writePatch dir patchId j
    pure (Just (patchId <> ".json"))
  Nothing -> pure Nothing

prunePresets :: String -> Array String -> Aff Unit
prunePresets dir keep = do
  pedals <- lsSafe (presetsDir dir)
  void $ traverse (prunePedalDir dir keep) pedals

prunePedalDir :: String -> Array String -> String -> Aff Unit
prunePedalDir dir keep pedalId = do
  files <- jsonFiles <$> lsSafe (concat [ presetsDir dir, pedalId ])
  let stale = Array.filter (\f -> not (Array.elem (pedalId <> "/" <> f) keep)) files
  void $ traverse (\f -> rmSafe (concat [ presetsDir dir, pedalId, f ])) stale

prunePatches :: String -> Array String -> Aff Unit
prunePatches dir keep = do
  files <- jsonFiles <$> lsSafe (patchesDir dir)
  let stale = Array.filter (\f -> not (Array.elem f keep)) files
  void $ traverse (\f -> rmSafe (concat [ patchesDir dir, f ])) stale
