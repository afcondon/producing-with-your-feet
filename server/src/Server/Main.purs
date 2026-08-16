-- | pwyf-store — a dumb JSON document store for presets, patches and MC6
-- | assignments.
-- |
-- | Deliberately ignorant of the domain: it never parses a preset, so there is
-- | no schema here to drift out of step with `Data.Preset`. The client owns the
-- | codecs; this owns durability. The one thing it does know is where a
-- | document belongs on disk, and the client tells it that in the path.
-- |
-- | Layout under $PWYF_STORE_DIR:
-- |
-- |     presets/<pedalId>/<presetId>.json
-- |     patches/<patchId>.json
-- |     banks/<bankId>.json
-- |     assignments.json
-- |
-- | One file per record, so `git log` on the store says which preset changed
-- | rather than "presets.json changed".
module Server.Main where

-- `(/)` is the route-segment operator here, not division.
import Prelude hiding ((/))

import Data.Argonaut.Core as AJ
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Data.Generic.Rep (class Generic)
import HTTPurple
  ( Method(..)
  , Request
  , ResponseM
  , ServerM
  , badRequest
  , notFound
  , response'
  , ok'
  , serve
  , toString
  )
import HTTPurple.Headers (ResponseHeaders, headers)
import HTTPurple.Status as Status
import Node.Process as Process
import Routing.Duplex (RouteDuplex', root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Server.Store as Store

data Route
  = Health
  | Snapshot
  | SnapshotForce
  | Presets
  | PresetOne String String
  | Patches
  | PatchOne String
  | Banks
  | BankOne String
  | Assignments

derive instance Generic Route _

route :: RouteDuplex' Route
route = root $ sum
  { "Health": "health" / noArgs
  , "Snapshot": "api" / "snapshot" / noArgs
  , "SnapshotForce": "api" / "snapshot" / "force" / noArgs
  , "Presets": "api" / "presets" / noArgs
  , "PresetOne": "api" / "presets" / segment / segment
  , "Patches": "api" / "patches" / noArgs
  , "PatchOne": "api" / "patches" / segment
  , "Banks": "api" / "banks" / noArgs
  , "BankOne": "api" / "banks" / segment
  , "Assignments": "api" / "assignments" / noArgs
  }

-- | Permissive CORS: the app is served from a different origin (and, on the
-- | iPad, from a capacitor:// one), which is the whole reason this exists.
jsonHeaders :: ResponseHeaders
jsonHeaders = headers
  [ Tuple "Content-Type" "application/json"
  , Tuple "Access-Control-Allow-Origin" "*"
  , Tuple "Access-Control-Allow-Methods" "GET, PUT, DELETE, OPTIONS"
  , Tuple "Access-Control-Allow-Headers" "Content-Type"
  ]

okJson :: AJ.Json -> ResponseM
okJson = ok' jsonHeaders <<< AJ.stringify

-- | 409: the request was well-formed but would have destroyed data.
conflict :: String -> ResponseM
conflict msg = response' Status.conflict jsonHeaders
  (AJ.stringify (AJ.fromString msg))

router :: String -> Request Route -> ResponseM
router dir { route: r, method, body } = case r, method of
  _, Options -> ok' jsonHeaders ""

  Health, Get -> okJson (AJ.fromString "ok")

  Snapshot, Get -> okJson =<< liftAff (Store.snapshot dir)
  Snapshot, Put -> putSnapshot false
  SnapshotForce, Put -> putSnapshot true

  Presets, Get -> okJson =<< liftAff (Store.listPresets dir)

  PresetOne pedalId presetId, Put -> withJsonBody \j -> do
    liftAff (Store.writePreset dir pedalId presetId j)
    okJson j
  PresetOne pedalId presetId, Delete -> do
    liftAff (Store.deletePreset dir pedalId presetId)
    okJson (AJ.fromString "deleted")

  Patches, Get -> okJson =<< liftAff (Store.listPatches dir)

  PatchOne patchId, Put -> withJsonBody \j -> do
    liftAff (Store.writePatch dir patchId j)
    okJson j
  PatchOne patchId, Delete -> do
    liftAff (Store.deletePatch dir patchId)
    okJson (AJ.fromString "deleted")

  Banks, Get -> okJson =<< liftAff (Store.listBanks dir)

  BankOne bankId, Put -> withJsonBody \j -> do
    liftAff (Store.writeBank dir bankId j)
    okJson j
  BankOne bankId, Delete -> do
    liftAff (Store.deleteBank dir bankId)
    okJson (AJ.fromString "deleted")

  Assignments, Get -> okJson =<< liftAff (Store.readAssignments dir)
  Assignments, Put -> withJsonBody \j -> do
    liftAff (Store.writeAssignments dir j)
    okJson j

  _, _ -> notFound
  where
  putSnapshot force = withJsonBody \j -> do
    outcome <- liftAff (Store.replaceAll dir force j)
    case outcome of
      Left msg -> conflict msg
      Right _ -> okJson =<< liftAff (Store.snapshot dir)

  withJsonBody k = do
    raw <- toString body
    case jsonParser raw of
      Left err -> badRequest ("invalid JSON: " <> err)
      Right j -> k j

-- | Bosun requires the port to come from the environment.
resolvePort :: Effect Int
resolvePort = do
  mp <- Process.lookupEnv "PORT"
  pure $ fromMaybe 3002 (mp >>= Int.fromString)

resolveStoreDir :: Effect String
resolveStoreDir = do
  md <- Process.lookupEnv "PWYF_STORE_DIR"
  pure $ fromMaybe
    "/Users/afc/work/afc-work/infovore-larder-db/data/producing-with-your-feet"
    md

main :: ServerM
main = do
  port <- resolvePort
  dir <- resolveStoreDir
  -- Fire-and-forget: every write path calls mkdir -p anyway, so this is only
  -- to make the layout visible on a fresh store before anything is saved.
  launchAff_ (Store.ensureLayout dir)
  Console.log $ "pwyf-store listening on :" <> show port <> "  ·  store=" <> dir
  serve { port, hostname: "0.0.0.0" } { route, router: router dir }
