# PureScript Build & Serve

Build, bundle, and serve the Pedal Explorer PureScript app.

## Usage

```
/ps                  # Show status (build state, server)
/ps build            # spago build (compile only)
/ps bundle           # spago build + bundle (updates static/index.js)
/ps serve            # Start local server on port 8090
/ps serve stop       # Stop local server
/ps rebuild          # Clean output, build, and bundle
```

## Arguments

$ARGUMENTS

## Instructions

You are the build assistant for Pedal Explorer (`explorer-ps`), a PureScript/Halogen app.

### 0. Paths

```
PROJECT=/Users/afc/work/afc-work/explorer-ps
STATIC=$PROJECT/static
BUNDLE=$STATIC/index.js
```

### 1. Parse the Request

- No arguments or "status": Show build state and whether server is running
- `build`: Compile PureScript only (`spago build`)
- `bundle`: Compile + bundle for browser (`spago bundle`)
- `serve`: Start a local HTTP server
- `serve stop`: Stop the local server
- `rebuild`: Remove `output/`, then build + bundle

### 2. Status Check

```bash
# Build state
ls -la /Users/afc/work/afc-work/explorer-ps/static/index.js 2>/dev/null

# Server running?
lsof -i :8090 2>/dev/null | head -3
```

### 3. Build

```bash
cd /Users/afc/work/afc-work/explorer-ps
spago build
```

This compiles PureScript to JS in `output/`. The browser loads `static/index.js` (the bundle), so a build alone won't update the running app.

### 4. Bundle

```bash
cd /Users/afc/work/afc-work/explorer-ps
spago bundle --outfile static/index.js
```

This runs `spago build` then bundles into `static/index.js` via esbuild. **The `--outfile` flag is required** — without it, spago writes to `./index.js` in the project root, not `static/`. After bundling, refresh the browser to see changes.

### 5. Rebuild (Clean)

```bash
cd /Users/afc/work/afc-work/explorer-ps
rm -rf output
spago bundle --outfile static/index.js
```

### 6. Serve

```bash
cd /Users/afc/work/afc-work/explorer-ps/static
npx serve . -p 8090 &
```

Access at http://localhost:8090

### 7. Stop Server

```bash
lsof -ti :8090 | xargs kill
```

### 8. Quick Reference

| Action | Command |
|--------|---------|
| Compile only | `spago build` |
| Compile + bundle | `spago bundle --outfile static/index.js` |
| Clean rebuild | `rm -rf output && spago bundle --outfile static/index.js` |
| Start server | `cd static && npx serve . -p 8090 &` |
| Stop server | `lsof -ti :8090 \| xargs kill` |
| Check server | `lsof -i :8090` |
| Open in browser | http://localhost:8090 |

### 9. Notes

- `spago build` compiles PureScript to `output/` but does **not** update the browser bundle
- `spago bundle` does both compile and bundle — this is the command you want after code changes
- The bundle target is `static/index.js` (esbuild default output matching the `<script>` tag in `index.html`)
- Port 8090 is used to avoid collision with CodeExplorer (3000-3009) and other ecosystem services
- The `--migrate` warning from spago is cosmetic and can be ignored
